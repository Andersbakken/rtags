import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

// rc can emit very large payloads (a single --code-complete-at can exceed
// 2.5MB), and node's execFile default maxBuffer is only 1MB.
const MAX_BUFFER = 64 * 1024 * 1024;

export interface RcOptions {
    rcPath: string;
    socketFile?: string;
    timeoutMs: number;
    /**
     * Path (file or directory) handed to rdm via --current-file so that
     * name-based queries resolve against the intended project instead of
     * whatever project happens to be rdm's sticky "current" one.
     */
    projectHint?: string;
}

export function loadRcOptions(): RcOptions {
    return {
        rcPath: process.env.RTAGS_RC_PATH || "rc",
        socketFile: process.env.RTAGS_SOCKET_FILE || undefined,
        timeoutMs: parseInt(process.env.RTAGS_TIMEOUT_MS || "30000", 10),
        projectHint: process.env.RTAGS_PROJECT || process.cwd(),
    };
}

/**
 * Resolve the --current-file value for a query. An explicit per-call hint
 * (the queried file, or a caller supplied project) wins over the configured
 * default.
 */
export function currentFileArgs(
    options: RcOptions,
    hint?: string,
): string[] {
    const value = hint || options.projectHint;
    return value ? ["--current-file", value] : [];
}

export async function execRc(
    args: string[],
    options: RcOptions,
): Promise<string> {
    const fullArgs: string[] = [];
    if (options.socketFile) {
        fullArgs.push("--socket-file", options.socketFile);
    }
    fullArgs.push(...args);

    try {
        const { stdout } = await execFileAsync(options.rcPath, fullArgs, {
            encoding: "utf-8",
            timeout: options.timeoutMs,
            maxBuffer: MAX_BUFFER,
        });
        return stdout.trimEnd();
    } catch (err: unknown) {
        const error = err as {
            code?: string | number;
            stdout?: string;
            stderr?: string;
            message?: string;
        };
        if (error.code === "ENOENT") {
            throw new Error(
                `rc binary not found at "${options.rcPath}". Set RTAGS_RC_PATH to the path of the rc executable.`,
            );
        }
        if (
            error.stderr?.includes("Connection refused") ||
            error.stderr?.includes("Can't seem to connect")
        ) {
            throw new Error("rdm is not running. Start it with: rdm --daemon");
        }
        const stderr = error.stderr?.trim();
        if (stderr) {
            throw new Error(`rc error: ${stderr}`);
        }
        // rc exits non-zero with no output at all when a query simply has no
        // match (e.g. --class-hierarchy on a class with no bases, or a
        // location that holds no symbol). That is not a failure.
        const stdout = error.stdout?.trimEnd();
        if (stdout) {
            return stdout;
        }
        if (typeof error.code === "number") {
            return "";
        }
        throw new Error(`rc failed: ${error.message}`);
    }
}

export async function execRcJson(
    args: string[],
    options: RcOptions,
): Promise<unknown> {
    const output = await execRc([...args, "--json"], options);
    if (!output) {
        return null;
    }
    try {
        return JSON.parse(output);
    } catch {
        // Not every rc command honors --json; hand back the raw text so the
        // caller still sees usable output.
        return output;
    }
}
