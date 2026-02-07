// RTags test helper - manages rdm lifecycle and rc commands
import { execFileSync, spawn } from "node:child_process";
import { mkdtempSync, mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { tmpdir } from "node:os";

const BINARY_DIR = process.env.RTAGS_BINARY_DIR;
if (!BINARY_DIR) {
    console.error("Set RTAGS_BINARY_DIR to the directory containing rdm and rc");
    process.exit(1);
}

const RDM = join(BINARY_DIR, "rdm");
const RC = join(BINARY_DIR, "rc");
const SLEEP_MS = 100;
const MAX_RETRIES = 200;

function sleepSync(ms) {
    try {
        execFileSync("sleep", [String(ms / 1000)], { timeout: ms + 1000 });
    } catch {
        // ignore
    }
}

export class RTags {
    #socketFile;
    #dataDir;
    #logFile;
    #rdm;
    #tmpDir;

    constructor() {
        this.#tmpDir = mkdtempSync(join(tmpdir(), "rtags-test-"));
        const rtagsDir = join(this.#tmpDir, ".rtags");
        mkdirSync(rtagsDir);
        this.#socketFile = join(this.#tmpDir, ".socket");
        this.#dataDir = join(rtagsDir, "db");
        this.#logFile = join(rtagsDir, "rdm.log");
    }

    start() {
        this.#rdm = spawn(
            RDM,
            [
                "--no-rc",
                "--socket-file",
                this.#socketFile,
                "--log-file",
                this.#logFile,
                "--log-file-log-level=debug",
                "--exclude-filter=/none",
                "--watch-sources-only",
                "--data-dir",
                this.#dataDir
            ],
            { stdio: ["ignore", "inherit", "inherit"], cwd: this.#tmpDir }
        );
        // Wait for rdm to accept connections by retrying a simple command
        this.rc("--status");
    }

    stop() {
        if (this.#rdm) {
            this.#rdm.kill();
            this.#rdm = null;
        }
    }

    rc(...args) {
        const flatArgs = args.flat();
        if (!flatArgs.includes("--socket-file") && !flatArgs.includes("-n")) {
            flatArgs.unshift("--socket-file", this.#socketFile);
        }

        for (let i = 0; i < MAX_RETRIES; i++) {
            try {
                const out = execFileSync(RC, flatArgs, { encoding: "utf-8", timeout: 30000 });
                return out.trimEnd();
            } catch {
                if (i === MAX_RETRIES - 1) {
                    throw new Error(`rc ${flatArgs.join(" ")} failed after ${MAX_RETRIES} retries`);
                }
                sleepSync(SLEEP_MS);
            }
        }
    }

    clearDatabase() {
        this.rc("-C");
    }

    index(dir, files, compileCommands) {
        const srcFiles = files.filter((f) => /\.(cpp|c)$/.test(f));

        // Write compile_commands.json and load it via rc -J
        const commands = srcFiles.map((f, i) => {
            const srcPath = join(dir, f);
            const cmd = compileCommands?.[i] ?? `clang++ -std=c++14 -I${dir} -c ${srcPath}`;
            return {
                directory: dir,
                command: cmd,
                file: srcPath
            };
        });
        const ccPath = join(dir, "compile_commands.json");
        writeFileSync(ccPath, JSON.stringify(commands, null, 2));
        this.rc("--project-root", dir, "-J", dir, "--wait");

        // Wait for each source file to be indexed and ready
        for (const f of srcFiles) {
            this.rc("--is-indexed", join(dir, f));
        }
    }

    followLocation(file, line, col, ...extra) {
        return this.rc("--follow-location", `${file}:${line}:${col}`, ...extra);
    }

    references(file, line, col, ...extra) {
        return this.rc("--references", `${file}:${line}:${col}`, ...extra);
    }

    symbolInfo(file, line, col) {
        return this.rc("--symbol-info", `${file}:${line}:${col}`);
    }

    complete(file, line, col) {
        return this.rc("--code-complete-at", `${file}:${line}:${col}`, "--synchronous-completions");
    }

    findSymbol(name, ...extra) {
        return this.rc("--find-symbols", name, ...extra);
    }

    referenceName(name) {
        return this.rc("--references-name", name);
    }

    listSymbols(prefix) {
        const args = ["--list-symbols"];
        if (prefix) {
            args.push(prefix);
        }
        return this.rc(...args);
    }

    includePath(file, line, col) {
        return this.rc("--include-path", `${file}:${line}:${col}`);
    }
}
