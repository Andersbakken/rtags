import { execFile } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

export interface RcOptions {
    rcPath: string;
    socketFile?: string;
    timeoutMs: number;
}

export function loadRcOptions(): RcOptions {
    return {
        rcPath: process.env.RTAGS_RC_PATH || "rc",
        socketFile: process.env.RTAGS_SOCKET_FILE || undefined,
        timeoutMs: parseInt(process.env.RTAGS_TIMEOUT_MS || "30000", 10),
    };
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
        });
        return stdout.trimEnd();
    } catch (err: unknown) {
        const error = err as {
            code?: string;
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
            throw new Error(
                "rdm is not running. Start it with: rdm --daemon",
            );
        }
        const stderr = error.stderr?.trim();
        if (stderr) {
            throw new Error(`rc error: ${stderr}`);
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
        // Some commands don't produce JSON even with --json flag
        return { raw: output };
    }
}
