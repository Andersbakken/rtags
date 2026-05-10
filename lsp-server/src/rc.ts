import { execFile, spawn, type ChildProcess } from "node:child_process";

const activeChildren = new Set<ChildProcess>();

export function killActiveRcChildren(signal: NodeJS.Signals = "SIGTERM"): void {
    for (const child of activeChildren) {
        try {
            child.kill(signal);
        } catch {
            // child may already be dead; ignore
        }
    }
    activeChildren.clear();
}

export interface RcOptions {
    rcPath: string;
    socketFile?: string;
    timeoutMs: number;
}

export interface UnsavedFile {
    path: string;
    contents: string;
}

export function loadRcOptions(): RcOptions {
    return {
        rcPath: process.env.RTAGS_RC_PATH || "rc",
        socketFile: process.env.RTAGS_SOCKET_FILE || undefined,
        timeoutMs: parseInt(process.env.RTAGS_TIMEOUT_MS || "30000", 10),
    };
}

function buildArgs(args: string[], options: RcOptions): string[] {
    const fullArgs: string[] = [];
    if (options.socketFile) {
        fullArgs.push("--socket-file", options.socketFile);
    }
    fullArgs.push(...args);
    return fullArgs;
}

function mapRcError(err: unknown, rcPath: string): Error {
    const error = err as {
        code?: string;
        stderr?: string;
        message?: string;
    };
    if (error.code === "ENOENT") {
        return new Error(
            `rc binary not found at "${rcPath}". Set RTAGS_RC_PATH to the path of the rc executable.`,
        );
    }
    if (
        error.stderr?.includes("Connection refused") ||
        error.stderr?.includes("Can't seem to connect")
    ) {
        return new Error("rdm is not running. Start it with: rdm --daemon");
    }
    const stderr = error.stderr?.trim();
    if (stderr) {
        return new Error(`rc error: ${stderr}`);
    }
    return new Error(`rc failed: ${error.message}`);
}

export function execRc(args: string[], options: RcOptions): Promise<string> {
    return new Promise<string>((resolve, reject) => {
        const child = execFile(
            options.rcPath,
            buildArgs(args, options),
            {
                encoding: "utf-8",
                timeout: options.timeoutMs,
                maxBuffer: 32 * 1024 * 1024,
            },
            (err, stdout) => {
                activeChildren.delete(child);
                if (err) {
                    reject(mapRcError(err, options.rcPath));
                    return;
                }
                resolve(stdout.trimEnd());
            },
        );
        activeChildren.add(child);
    });
}

export async function execRcJson(
    args: string[],
    options: RcOptions,
): Promise<unknown> {
    const output = await execRc([...args, "--json"], options);
    if (!output || output === "null") {
        return null;
    }
    try {
        return JSON.parse(output);
    } catch {
        return { raw: output };
    }
}

export async function execRcWithUnsaved(
    args: string[],
    unsaved: UnsavedFile[],
    options: RcOptions,
): Promise<string> {
    if (unsaved.length === 0) return execRc(args, options);

    const fullArgs = buildArgs(args, options);
    const buffers: Buffer[] = [];
    for (const u of unsaved) {
        const buf = Buffer.from(u.contents, "utf-8");
        fullArgs.push("--unsaved-file", `${u.path}:${buf.byteLength}`);
        buffers.push(buf);
    }

    return new Promise<string>((resolve, reject) => {
        const child = spawn(options.rcPath, fullArgs, {
            stdio: ["pipe", "pipe", "pipe"],
        });
        activeChildren.add(child);
        const stdoutChunks: Buffer[] = [];
        const stderrChunks: Buffer[] = [];
        let timer: NodeJS.Timeout | null = null;
        if (options.timeoutMs > 0) {
            timer = setTimeout(() => {
                child.kill("SIGTERM");
                reject(new Error(`rc timed out after ${options.timeoutMs}ms`));
            }, options.timeoutMs);
        }
        child.stdout.on("data", (b: Buffer) => stdoutChunks.push(b));
        child.stderr.on("data", (b: Buffer) => stderrChunks.push(b));
        child.on("error", (err) => {
            if (timer) clearTimeout(timer);
            activeChildren.delete(child);
            reject(mapRcError(err, options.rcPath));
        });
        child.on("close", (code) => {
            if (timer) clearTimeout(timer);
            activeChildren.delete(child);
            const stdout = Buffer.concat(stdoutChunks).toString("utf-8");
            const stderr = Buffer.concat(stderrChunks).toString("utf-8");
            if (code !== 0 && code !== null) {
                reject(
                    mapRcError(
                        { code: "RC_NONZERO", stderr, message: stderr },
                        options.rcPath,
                    ),
                );
                return;
            }
            resolve(stdout.trimEnd());
        });

        // EPIPE on stdin is reported by the close handler; absorb here.
        child.stdin.on("error", () => {});
        try {
            for (const buf of buffers) {
                if (child.stdin.destroyed) break;
                child.stdin.write(buf);
            }
            if (!child.stdin.destroyed) child.stdin.end();
        } catch {
            // synchronous EPIPE
        }
    });
}

export async function execRcJsonWithUnsaved(
    args: string[],
    unsaved: UnsavedFile[],
    options: RcOptions,
): Promise<unknown> {
    const output = await execRcWithUnsaved(
        [...args, "--json"],
        unsaved,
        options,
    );
    if (!output || output === "null") return null;
    try {
        return JSON.parse(output);
    } catch {
        return { raw: output };
    }
}
