import { spawn, type ChildProcess } from "node:child_process";
import {
    DiagnosticSeverity,
    type Diagnostic,
    type Connection,
    type DiagnosticRelatedInformation,
} from "vscode-languageserver/node.js";
import {
    rtagsToLspPosition,
    byteToLspChar,
    pathToUri,
    type PositionEncoding,
} from "./transforms.js";
import type { RcOptions } from "./rc.js";
import { readFileSync } from "node:fs";

export interface FixitEntry {
    line: number;
    column: number;
    length?: number;
    message: string;
}

export class FixitCache {
    private map = new Map<string, FixitEntry[]>();

    set(uri: string, entries: FixitEntry[]): void {
        if (entries.length === 0) this.map.delete(uri);
        else this.map.set(uri, entries);
    }

    get(uri: string): FixitEntry[] | undefined {
        return this.map.get(uri);
    }

    clear(uri: string): void {
        this.map.delete(uri);
    }
}

interface RtagsDiag {
    column?: number;
    line?: number;
    file?: string;
    message?: string;
    type?: string;
    length?: number;
    children?: RtagsDiag[];
}

interface DiagBatch {
    checkStyle?: Record<string, RtagsDiag[]>;
}

function severityFromType(type?: string): DiagnosticSeverity | null {
    switch (type) {
        case "error":
        case "fatal":
            return DiagnosticSeverity.Error;
        case "warning":
            return DiagnosticSeverity.Warning;
        case "note":
        case "skipped":
        default:
            return null;
    }
}

type LinesReader = (path: string) => string[];

function makeBatchLineReader(): LinesReader {
    const localCache = new Map<string, string[]>();
    return (path) => {
        const cached = localCache.get(path);
        if (cached) return cached;
        try {
            const lines = readFileSync(path, "utf-8").split(/\r?\n/);
            localCache.set(path, lines);
            return lines;
        } catch {
            return [];
        }
    };
}

function makeRange(
    file: string,
    d: RtagsDiag,
    encoding: PositionEncoding,
    readLines: LinesReader,
): { start: { line: number; character: number }; end: { line: number; character: number } } {
    const line = (d.line ?? 1) - 1;
    const col = (d.column ?? 1) - 1;
    const lineContent = readLines(file)[line] ?? "";
    const length = d.length ?? 1;
    const start = rtagsToLspPosition(
        lineContent,
        line + 1,
        col + 1,
        encoding,
    );
    const endByte = col + length;
    return {
        start,
        end: {
            line: start.line,
            character: byteToLspChar(lineContent, endByte, encoding),
        },
    };
}

function buildRelatedInfo(
    d: RtagsDiag,
    encoding: PositionEncoding,
    readLines: LinesReader,
): DiagnosticRelatedInformation[] | undefined {
    const children = d.children;
    if (!children || children.length === 0) return undefined;
    const out: DiagnosticRelatedInformation[] = [];
    for (const c of children) {
        if (!c.file) continue;
        const range = makeRange(c.file, c, encoding, readLines);
        out.push({
            location: { uri: pathToUri(c.file), range },
            message: c.message ?? "",
        });
    }
    return out.length ? out : undefined;
}

export interface DiagStreamOptions {
    rcOptions: RcOptions;
    encoding: PositionEncoding;
    connection: Connection;
    fixits: FixitCache;
}

export class DiagStream {
    private child: ChildProcess | null = null;
    private buffer = "";
    private restarting = false;
    private backoffMs = 500;
    private stopped = false;

    constructor(private opts: DiagStreamOptions) {}

    start(): void {
        this.stopped = false;
        this.spawn();
    }

    stop(): void {
        this.stopped = true;
        this.child?.kill("SIGTERM");
        this.child = null;
    }

    private spawn(): void {
        const args: string[] = [];
        if (this.opts.rcOptions.socketFile) {
            args.push("--socket-file", this.opts.rcOptions.socketFile);
        }
        args.push("--diagnostics", "--json", "--silent-query");
        const child = spawn(this.opts.rcOptions.rcPath, args, {
            stdio: ["ignore", "pipe", "pipe"],
        });
        this.child = child;

        child.stdout?.setEncoding("utf-8");
        child.stdout?.on("data", (chunk: string) => this.onData(chunk));
        child.stderr?.setEncoding("utf-8");
        child.stderr?.on("data", (chunk: string) => {
            this.opts.connection.console.warn(`diagstream stderr: ${chunk.trim()}`);
        });
        child.on("error", (err) => {
            this.opts.connection.console.error(
                `diagstream spawn error: ${err.message}`,
            );
        });
        child.on("close", () => {
            this.child = null;
            if (this.stopped) return;
            this.scheduleRestart();
        });
    }

    private scheduleRestart(): void {
        if (this.restarting) return;
        this.restarting = true;
        const delay = Math.min(this.backoffMs, 30_000);
        setTimeout(() => {
            this.restarting = false;
            this.backoffMs = Math.min(this.backoffMs * 2, 30_000);
            this.spawn();
        }, delay);
    }

    private onData(chunk: string): void {
        this.buffer += chunk;
        let nl: number;
        while ((nl = this.buffer.indexOf("\n")) !== -1) {
            const line = this.buffer.slice(0, nl).trim();
            this.buffer = this.buffer.slice(nl + 1);
            if (!line) continue;
            this.handleLine(line);
        }
    }

    private handleLine(line: string): void {
        let parsed: DiagBatch;
        try {
            parsed = JSON.parse(line) as DiagBatch;
        } catch (err) {
            this.opts.connection.console.warn(
                `diagstream: bad JSON line (${(err as Error).message}); skipping`,
            );
            return;
        }
        this.backoffMs = 500;
        const checkStyle = parsed.checkStyle;
        if (!checkStyle) return;
        const readLines = makeBatchLineReader();
        for (const file of Object.keys(checkStyle)) {
            const diags = checkStyle[file];
            const uri = pathToUri(file);
            const lspDiags: Diagnostic[] = [];
            const fixits: FixitEntry[] = [];
            for (const d of diags) {
                if (d.type === "fixit") {
                    fixits.push({
                        line: d.line ?? 1,
                        column: d.column ?? 1,
                        length: d.length,
                        message: d.message ?? "",
                    });
                    continue;
                }
                const sev = severityFromType(d.type);
                if (sev == null) continue;
                lspDiags.push({
                    range: makeRange(file, d, this.opts.encoding, readLines),
                    severity: sev,
                    message: d.message ?? "",
                    source: "rtags",
                    relatedInformation: buildRelatedInfo(
                        d,
                        this.opts.encoding,
                        readLines,
                    ),
                });
            }
            this.opts.fixits.set(uri, fixits);
            this.opts.connection.sendDiagnostics({ uri, diagnostics: lspDiags });
        }
    }
}
