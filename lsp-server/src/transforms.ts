import { pathToFileURL, fileURLToPath } from "node:url";
import type { Position } from "vscode-languageserver/node.js";

export type PositionEncoding = "utf-8" | "utf-16" | "utf-32";

export interface RtagsLocation {
    file: string;
    line: number;
    col: number;
    context: string;
}

const LOC_LINE = /^(.+?):(\d+):(\d+):?\t?(.*)$/;

export function parseLocationLine(line: string): RtagsLocation | null {
    const m = line.match(LOC_LINE);
    if (!m) return null;
    return {
        file: m[1],
        line: parseInt(m[2], 10),
        col: parseInt(m[3], 10),
        context: m[4] ?? "",
    };
}

export function parseLocationLines(output: string): RtagsLocation[] {
    return output
        .split("\n")
        .map((l) => l.trim())
        .filter((l) => l.length > 0)
        .map(parseLocationLine)
        .filter((x): x is RtagsLocation => x !== null);
}

export interface RtagsLocStr {
    file: string;
    line: number;
    col: number;
}

const LOC_STR = /^(.+):(\d+):(\d+):?$/;

export function parseLocString(s: string): RtagsLocStr | null {
    const m = s.match(LOC_STR);
    if (!m) return null;
    return {
        file: m[1],
        line: parseInt(m[2], 10),
        col: parseInt(m[3], 10),
    };
}

export function buildRcLoc(file: string, line: number, col: number): string {
    return `${file}:${line}:${col}`;
}

export function uriToPath(uri: string): string {
    return fileURLToPath(uri);
}

export function pathToUri(path: string): string {
    return pathToFileURL(path).href;
}

export function utf16ToByteOffset(line: string, char: number): number {
    if (char <= 0) return 0;
    const slice = line.slice(0, char);
    return Buffer.byteLength(slice, "utf8");
}

export function byteToUtf16Offset(line: string, byte: number): number {
    if (byte <= 0) return 0;
    let acc = 0;
    for (let i = 0; i < line.length; i++) {
        if (acc >= byte) return i;
        const code = line.charCodeAt(i);
        if (code < 0x80) {
            acc += 1;
        } else if (code < 0x800) {
            acc += 2;
        } else if (code >= 0xd800 && code <= 0xdbff) {
            acc += 4;
            i++;
        } else {
            acc += 3;
        }
    }
    return line.length;
}

export function byteToUtf32Offset(line: string, byte: number): number {
    if (byte <= 0) return 0;
    const utf16 = byteToUtf16Offset(line, byte);
    let cps = 0;
    for (let i = 0; i < utf16; ) {
        const code = line.charCodeAt(i);
        if (code >= 0xd800 && code <= 0xdbff) i += 2;
        else i += 1;
        cps++;
    }
    return cps;
}

export function byteToLspChar(
    line: string,
    byte: number,
    encoding: PositionEncoding,
): number {
    switch (encoding) {
        case "utf-8":
            return byte;
        case "utf-16":
            return byteToUtf16Offset(line, byte);
        case "utf-32":
            return byteToUtf32Offset(line, byte);
    }
}

export function lspPositionToRtagsCol(
    lineContent: string,
    pos: Position,
    encoding: PositionEncoding,
): { line: number; col: number } {
    const line = pos.line + 1;
    let col: number;
    switch (encoding) {
        case "utf-8":
            col = pos.character + 1;
            break;
        case "utf-16":
            col = utf16ToByteOffset(lineContent, pos.character) + 1;
            break;
        case "utf-32": {
            let utf16 = 0;
            let cps = 0;
            while (cps < pos.character && utf16 < lineContent.length) {
                const code = lineContent.charCodeAt(utf16);
                if (code >= 0xd800 && code <= 0xdbff) utf16 += 2;
                else utf16 += 1;
                cps++;
            }
            col = utf16ToByteOffset(lineContent, utf16) + 1;
            break;
        }
    }
    return { line, col };
}

export function rtagsToLspPosition(
    lineContent: string,
    rtagsLine: number,
    rtagsCol: number,
    encoding: PositionEncoding,
): Position {
    const line = rtagsLine - 1;
    const byte = rtagsCol - 1;
    let character: number;
    switch (encoding) {
        case "utf-8":
            character = byte;
            break;
        case "utf-16":
            character = byteToUtf16Offset(lineContent, byte);
            break;
        case "utf-32": {
            const utf16 = byteToUtf16Offset(lineContent, byte);
            character = 0;
            for (let i = 0; i < utf16; ) {
                const code = lineContent.charCodeAt(i);
                if (code >= 0xd800 && code <= 0xdbff) i += 2;
                else i += 1;
                character++;
            }
            break;
        }
    }
    return { line, character };
}

