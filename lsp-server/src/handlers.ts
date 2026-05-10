import {
    type Connection,
    type TextDocuments,
    type Definition,
    type DefinitionParams,
    type ReferenceParams,
    type Location,
    type ImplementationParams,
    type HoverParams,
    type Hover,
    type WorkspaceSymbolParams,
    type SymbolInformation,
    SymbolKind,
    type DocumentSymbolParams,
    type DocumentSymbol,
    type DocumentHighlightParams,
    type DocumentHighlight,
    DocumentHighlightKind,
    type DeclarationParams,
    type Declaration,
    type TypeDefinitionParams,
    type PrepareRenameParams,
    type RenameParams,
    type WorkspaceEdit,
    type Range,
    type CompletionParams,
    type CompletionItem,
    CompletionItemKind,
    type CodeActionParams,
    type CodeAction,
    CodeActionKind,
    type CallHierarchyPrepareParams,
    type CallHierarchyItem,
    type CallHierarchyIncomingCallsParams,
    type CallHierarchyIncomingCall,
    type CallHierarchyOutgoingCallsParams,
    type CallHierarchyOutgoingCall,
    type TypeHierarchyPrepareParams,
    type TypeHierarchyItem,
    type TypeHierarchySupertypesParams,
    type TypeHierarchySubtypesParams,
    type TextEdit,
} from "vscode-languageserver/node.js";
import { TextDocument } from "vscode-languageserver-textdocument";
import {
    execRc,
    execRcJson,
    execRcWithUnsaved,
    execRcJsonWithUnsaved,
    type RcOptions,
    type UnsavedFile,
} from "./rc.js";
import {
    parseLocationLines,
    parseLocationLine,
    parseLocString,
    buildRcLoc,
    rtagsToLspPosition,
    lspPositionToRtagsCol,
    byteToLspChar,
    pathToUri,
    uriToPath,
    type PositionEncoding,
    type RtagsLocation,
} from "./transforms.js";
import { readFileSync, statSync } from "node:fs";
import type { FixitCache } from "./diagstream.js";

export interface ServerContext {
    connection: Connection;
    documents: TextDocuments<TextDocument>;
    rcOptions: RcOptions;
    encoding: PositionEncoding;
    workspaceFolders: string[];
    fixits: FixitCache;
}

interface DocSymbolCacheEntry {
    mtimeMs: number;
    symbols: DocumentSymbol[];
}
const DOC_SYMBOL_CACHE_CAP = 32;
const docSymbolCache = new Map<string, DocSymbolCacheEntry>();

function docSymbolCacheGet(file: string): DocSymbolCacheEntry | undefined {
    const v = docSymbolCache.get(file);
    if (!v) return undefined;
    docSymbolCache.delete(file);
    docSymbolCache.set(file, v);
    return v;
}

function docSymbolCacheSet(file: string, entry: DocSymbolCacheEntry): void {
    if (docSymbolCache.has(file)) docSymbolCache.delete(file);
    docSymbolCache.set(file, entry);
    while (docSymbolCache.size > DOC_SYMBOL_CACHE_CAP) {
        const oldest = docSymbolCache.keys().next().value;
        if (oldest === undefined) break;
        docSymbolCache.delete(oldest);
    }
}

function getLineContent(
    ctx: ServerContext,
    uri: string,
    line0: number,
): string {
    const doc = ctx.documents.get(uri);
    if (doc) {
        return doc
            .getText({
                start: { line: line0, character: 0 },
                end: { line: line0 + 1, character: 0 },
            })
            .replace(/\r?\n$/, "");
    }
    try {
        const path = uriToPath(uri);
        const text = readFileSync(path, "utf-8");
        const lines = text.split(/\r?\n/);
        return lines[line0] ?? "";
    } catch {
        return "";
    }
}

function getUnsavedFiles(ctx: ServerContext): UnsavedFile[] {
    const out: UnsavedFile[] = [];
    for (const doc of ctx.documents.all()) {
        try {
            out.push({ path: uriToPath(doc.uri), contents: doc.getText() });
        } catch {
            // non-file URI; skip
        }
    }
    return out;
}

function lspToRcLocFromUri(
    ctx: ServerContext,
    uri: string,
    pos: { line: number; character: number },
): { file: string; line: number; col: number } {
    const file = uriToPath(uri);
    const lineContent = getLineContent(ctx, uri, pos.line);
    const { line, col } = lspPositionToRtagsCol(lineContent, pos, ctx.encoding);
    return { file, line, col };
}

function rtagsLocToLspLoc(
    ctx: ServerContext,
    rloc: { file: string; line: number; col: number },
    symbolLength = 0,
): Location {
    const uri = pathToUri(rloc.file);
    const lineContent = getLineContent(ctx, uri, rloc.line - 1);
    const start = rtagsToLspPosition(
        lineContent,
        rloc.line,
        rloc.col,
        ctx.encoding,
    );
    let end = start;
    if (symbolLength > 0) {
        const endByte = rloc.col - 1 + symbolLength;
        end = {
            line: start.line,
            character: byteToLspChar(lineContent, endByte, ctx.encoding),
        };
    }
    return { uri, range: { start, end } };
}

interface SymbolInfoJson {
    location?: string;
    symbolName?: string;
    symbolLength?: number;
    type?: string;
    kind?: string;
    linkage?: string;
    startLine?: number;
    startColumn?: number;
    endLine?: number;
    endColumn?: number;
    cf?: string;
    cfl?: string;
    cflcontext?: string;
    context?: string;
    targets?: SymbolInfoJson[];
    parent?: SymbolInfoJson;
    definition?: boolean;
    container?: boolean;
    usr?: string;
    reference?: boolean;
    arguments?: unknown;
    sizeof?: number;
    alignment?: number;
}

const SYMBOL_KIND_MAP: Record<string, SymbolKind> = {
    ClassDecl: SymbolKind.Class,
    ClassTemplate: SymbolKind.Class,
    StructDecl: SymbolKind.Struct,
    EnumDecl: SymbolKind.Enum,
    EnumConstantDecl: SymbolKind.EnumMember,
    UnionDecl: SymbolKind.Class,
    Namespace: SymbolKind.Namespace,
    TypedefDecl: SymbolKind.TypeParameter,
    TypeAliasDecl: SymbolKind.TypeParameter,
    FunctionDecl: SymbolKind.Function,
    FunctionTemplate: SymbolKind.Function,
    CXXMethod: SymbolKind.Method,
    Constructor: SymbolKind.Constructor,
    Destructor: SymbolKind.Constructor,
    ConversionFunction: SymbolKind.Method,
    FieldDecl: SymbolKind.Field,
    VarDecl: SymbolKind.Variable,
    ParmDecl: SymbolKind.Variable,
    MacroDefinition: SymbolKind.Constant,
    InclusionDirective: SymbolKind.File,
    UsingDeclaration: SymbolKind.Variable,
    UsingDirective: SymbolKind.Namespace,
};

function rtagsKindToLspSymbolKind(kind?: string): SymbolKind {
    if (!kind) return SymbolKind.Variable;
    return SYMBOL_KIND_MAP[kind] ?? SymbolKind.Variable;
}

const COMPLETION_KIND_MAP: Record<string, CompletionItemKind> = {
    ClassDecl: CompletionItemKind.Class,
    StructDecl: CompletionItemKind.Struct,
    EnumDecl: CompletionItemKind.Enum,
    EnumConstantDecl: CompletionItemKind.EnumMember,
    UnionDecl: CompletionItemKind.Struct,
    Namespace: CompletionItemKind.Module,
    TypedefDecl: CompletionItemKind.TypeParameter,
    TypeAliasDecl: CompletionItemKind.TypeParameter,
    FunctionDecl: CompletionItemKind.Function,
    CXXMethod: CompletionItemKind.Method,
    Constructor: CompletionItemKind.Constructor,
    FieldDecl: CompletionItemKind.Field,
    VarDecl: CompletionItemKind.Variable,
    ParmDecl: CompletionItemKind.Variable,
    MacroDefinition: CompletionItemKind.Constant,
};

function rtagsKindToCompletionKind(kind?: string): CompletionItemKind {
    if (!kind) return CompletionItemKind.Text;
    return COMPLETION_KIND_MAP[kind] ?? CompletionItemKind.Text;
}

async function getSymbolInfo(
    ctx: ServerContext,
    file: string,
    line: number,
    col: number,
    flags: string[] = [],
): Promise<SymbolInfoJson | null> {
    const args = [
        "--symbol-info",
        buildRcLoc(file, line, col),
        "--absolute-path",
        ...flags,
    ];
    const unsaved = getUnsavedFiles(ctx);
    const result = (await execRcJsonWithUnsaved(
        args,
        unsaved,
        ctx.rcOptions,
    )) as SymbolInfoJson | null;
    if (!result || typeof result !== "object" || !("location" in result)) {
        return null;
    }
    return result;
}

function symbolInfoToLspLoc(
    ctx: ServerContext,
    s: SymbolInfoJson,
): Location | null {
    if (!s.location) return null;
    const loc = parseLocString(s.location);
    if (!loc) return null;
    return rtagsLocToLspLoc(ctx, loc, s.symbolLength ?? 0);
}

function symbolInfoToSelectionRange(
    ctx: ServerContext,
    s: SymbolInfoJson,
): Range | null {
    if (!s.location) return null;
    const loc = parseLocString(s.location);
    if (!loc) return null;
    const uri = pathToUri(loc.file);
    const lineContent = getLineContent(ctx, uri, loc.line - 1);
    const start = rtagsToLspPosition(
        lineContent,
        loc.line,
        loc.col,
        ctx.encoding,
    );
    const symLen = s.symbolLength ?? 0;
    if (symLen <= 0) {
        return { start, end: start };
    }
    const endByte = loc.col - 1 + symLen;
    const character = byteToLspChar(lineContent, endByte, ctx.encoding);
    return { start, end: { line: start.line, character } };
}

function symbolInfoToFullRange(
    ctx: ServerContext,
    s: SymbolInfoJson,
): Range | null {
    if (
        s.startLine == null ||
        s.startColumn == null ||
        s.endLine == null ||
        s.endColumn == null
    ) {
        return symbolInfoToSelectionRange(ctx, s);
    }
    if (!s.location) return null;
    const loc = parseLocString(s.location);
    if (!loc) return null;
    const uri = pathToUri(loc.file);
    const startLine = getLineContent(ctx, uri, s.startLine - 1);
    const endLine =
        s.endLine === s.startLine
            ? startLine
            : getLineContent(ctx, uri, s.endLine - 1);
    return {
        start: rtagsToLspPosition(
            startLine,
            s.startLine,
            s.startColumn,
            ctx.encoding,
        ),
        end: rtagsToLspPosition(endLine, s.endLine, s.endColumn, ctx.encoding),
    };
}

interface RefJsonEntry {
    ctx?: string;
    loc: string;
}

function parseRefsJson(
    raw: unknown,
): Array<{ file: string; line: number; col: number; ctx: string }> {
    if (!Array.isArray(raw)) return [];
    const out: Array<{
        file: string;
        line: number;
        col: number;
        ctx: string;
    }> = [];
    for (const item of raw as RefJsonEntry[]) {
        const loc = parseLocString(item.loc);
        if (loc) out.push({ ...loc, ctx: item.ctx ?? "" });
    }
    return out;
}

// ---------------------------------------------------------------------------
// Navigation: definition, declaration, typeDefinition, implementation,
// references, documentHighlight
// ---------------------------------------------------------------------------

async function followLocation(
    ctx: ServerContext,
    uri: string,
    pos: { line: number; character: number },
    extraFlags: string[] = [],
): Promise<Location[]> {
    const { file, line, col } = lspToRcLocFromUri(ctx, uri, pos);
    const args = [
        "--follow-location",
        buildRcLoc(file, line, col),
        "--absolute-path",
        ...extraFlags,
    ];
    const unsaved = getUnsavedFiles(ctx);
    const output = await execRcWithUnsaved(args, unsaved, ctx.rcOptions);
    const locs = parseLocationLines(output);
    return locs.map((rloc) => rtagsLocToLspLoc(ctx, rloc));
}

async function onDefinition(
    ctx: ServerContext,
    params: DefinitionParams,
): Promise<Definition | null> {
    const locs = await followLocation(ctx, params.textDocument.uri, params.position);
    return locs.length === 0 ? null : locs;
}

async function onDeclaration(
    ctx: ServerContext,
    params: DeclarationParams,
): Promise<Declaration | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col, [
        "--symbol-info-include-targets",
    ]);
    if (!info) return null;
    const targets = info.targets ?? [];
    const declarations = targets.filter((t) => t.definition !== true);
    const source = declarations.length > 0 ? declarations : targets;
    const locs: Location[] = [];
    for (const t of source) {
        const loc = symbolInfoToLspLoc(ctx, t);
        if (loc) locs.push(loc);
    }
    return locs.length === 0 ? null : locs;
}

async function onImplementation(
    ctx: ServerContext,
    params: ImplementationParams,
): Promise<Definition | null> {
    const locs = await followLocation(ctx, params.textDocument.uri, params.position, [
        "--all-targets",
    ]);
    return locs.length === 0 ? null : locs;
}

async function onTypeDefinition(
    ctx: ServerContext,
    params: TypeDefinitionParams,
): Promise<Definition | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col, [
        "--symbol-info-include-targets",
    ]);
    if (!info) return null;

    const targets = info.targets ?? [];
    const typeKinds = new Set([
        "ClassDecl",
        "ClassTemplate",
        "StructDecl",
        "UnionDecl",
        "EnumDecl",
        "TypedefDecl",
        "TypeAliasDecl",
    ]);
    const typeTargets = targets.filter(
        (t) => t.kind && typeKinds.has(t.kind),
    );
    if (typeTargets.length > 0) {
        const locs: Location[] = [];
        for (const t of typeTargets) {
            const loc = symbolInfoToLspLoc(ctx, t);
            if (loc) locs.push(loc);
        }
        if (locs.length > 0) return locs;
    }

    const typeName = extractTypeName(info.type);
    if (!typeName) return null;

    const args = [
        "--find-symbols",
        typeName,
        "--absolute-path",
        "--max",
        "5",
    ];
    const output = await execRc(args, ctx.rcOptions);
    const locs = parseLocationLines(output).map((r) =>
        rtagsLocToLspLoc(ctx, r),
    );
    return locs.length === 0 ? null : locs;
}

function extractTypeName(t?: string): string | null {
    if (!t) return null;
    const cleaned = t
        .replace(/\bconst\b|\bvolatile\b|\brestrict\b/g, "")
        .replace(/[*&]+/g, "")
        .trim();
    const m = cleaned.match(/^([A-Za-z_][\w:]*)/);
    if (!m) return null;
    const name = m[1];
    const PRIMITIVES = new Set([
        "void",
        "bool",
        "char",
        "short",
        "int",
        "long",
        "float",
        "double",
        "signed",
        "unsigned",
        "auto",
    ]);
    if (PRIMITIVES.has(name)) return null;
    return name;
}

async function onReferences(
    ctx: ServerContext,
    params: ReferenceParams,
): Promise<Location[] | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const args = [
        "--references",
        buildRcLoc(file, line, col),
        "--absolute-path",
    ];
    if (params.context.includeDeclaration) args.push("--all-references");
    const unsaved = getUnsavedFiles(ctx);
    const raw = await execRcJsonWithUnsaved(args, unsaved, ctx.rcOptions);
    const refs = parseRefsJson(raw);
    if (refs.length === 0) return null;
    return refs.map((r) => rtagsLocToLspLoc(ctx, r));
}

async function onDocumentHighlight(
    ctx: ServerContext,
    params: DocumentHighlightParams,
): Promise<DocumentHighlight[] | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const args = [
        "--references",
        buildRcLoc(file, line, col),
        "--path-filter",
        file,
        "--all-references",
        "--absolute-path",
    ];
    const unsaved = getUnsavedFiles(ctx);
    const raw = await execRcJsonWithUnsaved(args, unsaved, ctx.rcOptions);
    const refs = parseRefsJson(raw);
    if (refs.length === 0) return null;
    return refs.map((r) => {
        const loc = rtagsLocToLspLoc(ctx, r);
        return {
            range: loc.range,
            kind: DocumentHighlightKind.Text,
        };
    });
}

// ---------------------------------------------------------------------------
// Hover
// ---------------------------------------------------------------------------

async function onHover(
    ctx: ServerContext,
    params: HoverParams,
): Promise<Hover | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col, [
        "--symbol-info-include-parents",
    ]);
    if (!info) return null;

    const parts: string[] = [];
    if (info.symbolName) parts.push(`**${info.symbolName}**`);
    if (info.type) parts.push(`\`${info.type}\``);
    if (info.kind) parts.push(`*${info.kind}*${info.linkage ? ` · ${info.linkage} linkage` : ""}`);
    if (info.cf) parts.push(`In: \`${info.cf}\``);
    if (info.parent?.symbolName && info.parent.symbolName !== info.symbolName) {
        parts.push(`Parent: \`${info.parent.symbolName}\``);
    }
    if (info.sizeof != null) {
        const a = info.alignment != null ? `, align ${info.alignment}` : "";
        parts.push(`Size ${info.sizeof}${a}`);
    }

    const range = symbolInfoToSelectionRange(ctx, info) ?? undefined;
    return {
        contents: { kind: "markdown", value: parts.join("\n\n") },
        range: range || undefined,
    };
}

// ---------------------------------------------------------------------------
// Symbols: workspace/symbol, documentSymbol
// ---------------------------------------------------------------------------

async function onWorkspaceSymbol(
    ctx: ServerContext,
    params: WorkspaceSymbolParams,
): Promise<SymbolInformation[]> {
    const query = params.query?.trim() ?? "";
    if (!query) return [];
    const args = [
        "--find-symbols",
        query,
        "--absolute-path",
        "--display-name",
        "--cursor-kind",
        "--max",
        "200",
    ];
    for (const folder of ctx.workspaceFolders) {
        args.push("--path-filter", folder);
    }
    const output = await execRc(args, ctx.rcOptions);
    const out: SymbolInformation[] = [];
    for (const raw of output.split("\n")) {
        const line = raw.trimEnd();
        if (!line) continue;
        const parts = line.split("\t");
        const loc = parseLocationLine(parts[0] ?? "");
        if (!loc) continue;
        const ctxText = parts[1] ?? "";
        const displayName = parts[2] ?? loc.context ?? "";
        const kindHint = parts[3] ?? "";
        const lspLoc = rtagsLocToLspLoc(ctx, loc);
        out.push({
            name: displayName || ctxText || query,
            kind: rtagsKindToLspSymbolKind(kindHint),
            location: lspLoc,
            containerName: undefined,
        });
    }
    return out;
}

const IMENU_KIND_FILTER =
    "-references,-vardecl,-parmdecl,-inclusiondirective,-*literal*,-enumconstantdecl,-classdecl-,-structdecl-,-classtemplate-,-statements,-lambdaexpr";

async function onDocumentSymbol(
    ctx: ServerContext,
    params: DocumentSymbolParams,
): Promise<DocumentSymbol[]> {
    const file = uriToPath(params.textDocument.uri);
    let mtimeMs = 0;
    try {
        mtimeMs = statSync(file).mtimeMs;
    } catch {
        return [];
    }
    const cached = docSymbolCacheGet(file);
    const doc = ctx.documents.get(params.textDocument.uri);
    const cacheable = !doc;
    if (cacheable && cached && cached.mtimeMs === mtimeMs) {
        return cached.symbols;
    }

    const namesArgs = [
        "--list-symbols",
        "--path-filter",
        file,
        "--path",
        file,
        "--kind-filter",
        IMENU_KIND_FILTER,
    ];
    const unsaved = getUnsavedFiles(ctx);
    const namesOutput = await execRcWithUnsaved(
        namesArgs,
        unsaved,
        ctx.rcOptions,
    );
    const basename = file.split("/").pop() ?? "";
    const lines = namesOutput
        .split("\n")
        .map((s) => s.trim())
        .filter((s) => s.length > 0 && !s.startsWith("#"));
    if (
        lines.length > 0 &&
        lines[lines.length - 1].includes("/") &&
        lines[lines.length - 1].endsWith(basename)
    ) {
        lines.pop();
    }
    const names = lines;

    const seen = new Set<string>();
    const seenLocs = new Set<string>();
    const symbols: DocumentSymbol[] = [];
    for (const name of names) {
        if (seen.has(name)) continue;
        seen.add(name);
        const findArgs = [
            "--find-symbols",
            name,
            "--path-filter",
            file,
            "--display-name",
            "--cursor-kind",
            "--no-context",
            "--absolute-path",
        ];
        const out = await execRcWithUnsaved(findArgs, unsaved, ctx.rcOptions);
        for (const ln of out.split("\n")) {
            const trimmed = ln.trimEnd();
            if (!trimmed) continue;
            const parts = trimmed.split("\t");
            const locText = parts[0];
            const loc = parseLocationLines(locText)[0];
            if (!loc) continue;
            const locKey = `${loc.file}:${loc.line}:${loc.col}`;
            if (seenLocs.has(locKey)) continue;
            seenLocs.add(locKey);
            const display = parts[1] || loc.context || name;
            const kindHint = parts[2] ?? "";

            const info = await getSymbolInfo(ctx, loc.file, loc.line, loc.col);
            const selectionRange =
                (info && symbolInfoToSelectionRange(ctx, info)) ??
                rtagsLocToLspLoc(ctx, loc).range;
            const range =
                (info && symbolInfoToFullRange(ctx, info)) ?? selectionRange;
            symbols.push({
                name: display,
                kind: rtagsKindToLspSymbolKind(kindHint),
                range,
                selectionRange,
            });
        }
    }
    if (cacheable) {
        docSymbolCacheSet(file, { mtimeMs, symbols });
    }
    return symbols;
}

// ---------------------------------------------------------------------------
// Rename: prepareRename, rename
// ---------------------------------------------------------------------------

async function onPrepareRename(
    ctx: ServerContext,
    params: PrepareRenameParams,
): Promise<{ range: Range; placeholder: string } | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col);
    if (!info) return null;
    const range = symbolInfoToSelectionRange(ctx, info);
    if (!range) return null;
    const beforeArgs = (info.symbolName ?? "").split("(")[0].trim();
    const lastWord = beforeArgs.split(/\s+/).pop() ?? "";
    const placeholder = lastWord.includes("::")
        ? (lastWord.split("::").pop() ?? lastWord)
        : lastWord;
    return { range, placeholder };
}

async function onRename(
    ctx: ServerContext,
    params: RenameParams,
): Promise<WorkspaceEdit | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col);
    const symbolLength = info?.symbolLength ?? 0;
    if (symbolLength <= 0) return null;

    const args = [
        "--references",
        buildRcLoc(file, line, col),
        "--rename",
        "--all-references",
        "--absolute-path",
    ];
    const unsaved = getUnsavedFiles(ctx);
    const raw = await execRcJsonWithUnsaved(args, unsaved, ctx.rcOptions);
    const refs = parseRefsJson(raw);
    if (refs.length === 0) return null;

    const changes: Record<string, TextEdit[]> = {};
    for (const r of refs) {
        const loc = rtagsLocToLspLoc(ctx, r, symbolLength);
        const list = (changes[loc.uri] ??= []);
        list.push({ range: loc.range, newText: params.newName });
    }
    return { changes };
}

// ---------------------------------------------------------------------------
// Completion
// ---------------------------------------------------------------------------

interface CompletionJson {
    completions?: Array<{
        annotation?: string;
        brief_comment?: string;
        completion?: string;
        kind?: string;
        parent?: string;
        priority?: number;
        signature?: string;
    }>;
}

async function onCompletion(
    ctx: ServerContext,
    params: CompletionParams,
): Promise<CompletionItem[]> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const args = [
        "--code-complete-at",
        buildRcLoc(file, line, col),
        "--synchronous-completions",
        "--code-complete-no-wait",
        "--absolute-path",
    ];
    const unsaved = getUnsavedFiles(ctx);
    const raw = (await execRcJsonWithUnsaved(
        args,
        unsaved,
        ctx.rcOptions,
    )) as CompletionJson | null;
    if (!raw || !raw.completions) return [];
    return raw.completions.map((c, idx) => ({
        label: c.completion ?? "",
        kind: rtagsKindToCompletionKind(c.kind),
        detail: c.signature || c.parent || undefined,
        documentation: c.brief_comment || undefined,
        sortText: String(idx).padStart(6, "0"),
        data: { priority: c.priority },
    }));
}

// ---------------------------------------------------------------------------
// Code Actions (fixits)
// ---------------------------------------------------------------------------

async function onCodeAction(
    ctx: ServerContext,
    params: CodeActionParams,
): Promise<CodeAction[]> {
    const only = params.context.only;
    if (only && only.length > 0) {
        const ours = CodeActionKind.QuickFix;
        const allowed = only.some(
            (k) => ours === k || ours.startsWith(k + "."),
        );
        if (!allowed) return [];
    }
    const file = uriToPath(params.textDocument.uri);
    const cached = ctx.fixits.get(params.textDocument.uri);
    if (!cached || cached.length === 0) return [];

    const fixitsText = await execRc(
        ["--fixits", file, "--absolute-path"],
        ctx.rcOptions,
    );
    const replacementByKey = new Map<string, { length: number; replacement: string }>();
    for (const line of fixitsText.split("\n")) {
        const m = line.match(/^(\d+):(\d+) (\d+) (.*)$/);
        if (!m) continue;
        const key = `${m[1]}:${m[2]}`;
        replacementByKey.set(key, {
            length: parseInt(m[3], 10),
            replacement: m[4],
        });
    }

    const out: CodeAction[] = [];
    for (const f of cached) {
        const key = `${f.line}:${f.column}`;
        const repl = replacementByKey.get(key);
        if (!repl) continue;
        const requestedRange = params.range;
        const fixitLine0 = f.line - 1;
        if (
            requestedRange.start.line > fixitLine0 ||
            requestedRange.end.line < fixitLine0
        ) {
            continue;
        }
        const lineContent = getLineContent(
            ctx,
            params.textDocument.uri,
            fixitLine0,
        );
        const start = rtagsToLspPosition(
            lineContent,
            f.line,
            f.column,
            ctx.encoding,
        );
        const endByte = f.column - 1 + repl.length;
        const character = byteToLspChar(lineContent, endByte, ctx.encoding);
        const range: Range = {
            start,
            end: { line: start.line, character },
        };
        out.push({
            title: f.message || `Apply fixit: ${repl.replacement}`,
            kind: CodeActionKind.QuickFix,
            edit: {
                changes: {
                    [params.textDocument.uri]: [
                        { range, newText: repl.replacement },
                    ],
                },
            },
        });
    }
    return out;
}

// ---------------------------------------------------------------------------
// Call hierarchy
// ---------------------------------------------------------------------------

function symbolInfoToCallHierarchyItem(
    ctx: ServerContext,
    s: SymbolInfoJson,
): CallHierarchyItem | null {
    if (!s.location) return null;
    const loc = parseLocString(s.location);
    if (!loc) return null;
    const range = symbolInfoToFullRange(ctx, s) ?? undefined;
    const selectionRange = symbolInfoToSelectionRange(ctx, s);
    if (!range || !selectionRange) return null;
    return {
        name: s.symbolName ?? "",
        kind: rtagsKindToLspSymbolKind(s.kind),
        uri: pathToUri(loc.file),
        range,
        selectionRange,
        detail: s.type,
        data: { location: s.location, usr: s.usr },
    };
}

async function onCallHierarchyPrepare(
    ctx: ServerContext,
    params: CallHierarchyPrepareParams,
): Promise<CallHierarchyItem[] | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col, [
        "--symbol-info-include-targets",
    ]);
    if (!info) return null;
    const candidates: SymbolInfoJson[] = [];
    if (info.targets && info.targets.length > 0) {
        for (const t of info.targets) {
            if (t.definition) candidates.push(t);
        }
        if (candidates.length === 0) candidates.push(info.targets[0]);
    } else {
        candidates.push(info);
    }
    const items: CallHierarchyItem[] = [];
    for (const c of candidates) {
        const item = symbolInfoToCallHierarchyItem(ctx, c);
        if (item) items.push(item);
    }
    return items.length === 0 ? null : items;
}

async function onCallHierarchyIncomingCalls(
    ctx: ServerContext,
    params: CallHierarchyIncomingCallsParams,
): Promise<CallHierarchyIncomingCall[]> {
    const data = params.item.data as { location?: string } | undefined;
    if (!data?.location) return [];
    const loc = parseLocString(data.location);
    if (!loc) return [];

    const args = [
        "--references",
        buildRcLoc(loc.file, loc.line, loc.col),
        "--containing-function",
        "--containing-function-location",
        "--absolute-path",
    ];
    const unsaved = getUnsavedFiles(ctx);
    const raw = (await execRcJsonWithUnsaved(
        args,
        unsaved,
        ctx.rcOptions,
    )) as Array<{
        ctx?: string;
        loc: string;
        cf?: string;
        cfl?: string;
    }> | null;
    if (!Array.isArray(raw)) return [];

    const grouped = new Map<
        string,
        {
            cf: string;
            cfl: string;
            ranges: Range[];
        }
    >();
    for (const item of raw) {
        if (!item.cfl || !item.cf) continue;
        const callerLoc = parseLocString(item.cfl);
        const refLoc = parseLocString(item.loc);
        if (!callerLoc || !refLoc) continue;
        const refLspLoc = rtagsLocToLspLoc(ctx, refLoc);
        const key = item.cfl;
        const entry = grouped.get(key) ?? {
            cf: item.cf,
            cfl: item.cfl,
            ranges: [],
        };
        entry.ranges.push(refLspLoc.range);
        grouped.set(key, entry);
    }

    const out: CallHierarchyIncomingCall[] = [];
    for (const entry of grouped.values()) {
        const callerLoc = parseLocString(entry.cfl);
        if (!callerLoc) continue;
        const callerInfo = await getSymbolInfo(
            ctx,
            callerLoc.file,
            callerLoc.line,
            callerLoc.col,
        );
        if (!callerInfo) continue;
        const item = symbolInfoToCallHierarchyItem(ctx, callerInfo);
        if (!item) continue;
        out.push({ from: item, fromRanges: entry.ranges });
    }
    return out;
}

async function onCallHierarchyOutgoingCalls(
    _ctx: ServerContext,
    _params: CallHierarchyOutgoingCallsParams,
): Promise<CallHierarchyOutgoingCall[]> {
    // rtags has no "calls from this function" query; would need
    // --tokens walking to synthesize.
    return [];
}

// ---------------------------------------------------------------------------
// Type hierarchy
// ---------------------------------------------------------------------------

interface ParsedHierarchyNode {
    name: string;
    file: string;
    line: number;
    col: number;
    children: ParsedHierarchyNode[];
}

function parseClassHierarchy(text: string): {
    superRoot?: ParsedHierarchyNode;
    subRoot?: ParsedHierarchyNode;
} {
    const lines = text.split("\n");
    let mode: "super" | "sub" | null = null;
    const superStack: { indent: number; node: ParsedHierarchyNode }[] = [];
    const subStack: { indent: number; node: ParsedHierarchyNode }[] = [];
    let superRoot: ParsedHierarchyNode | undefined;
    let subRoot: ParsedHierarchyNode | undefined;

    const NODE_RE = /^(\s*)(?:class|struct)\s+(\S+)\t(.+):(\d+):(\d+):?(?:\t.*)?$/;
    for (const raw of lines) {
        if (raw.startsWith("Superclasses:")) {
            mode = "super";
            continue;
        }
        if (raw.startsWith("Subclasses:")) {
            mode = "sub";
            continue;
        }
        if (!mode) continue;
        const m = raw.match(NODE_RE);
        if (!m) continue;
        const indent = m[1].length;
        const node: ParsedHierarchyNode = {
            name: m[2],
            file: m[3],
            line: parseInt(m[4], 10),
            col: parseInt(m[5], 10),
            children: [],
        };
        const stack = mode === "super" ? superStack : subStack;
        while (stack.length > 0 && stack[stack.length - 1].indent >= indent) {
            stack.pop();
        }
        if (stack.length === 0) {
            if (mode === "super") superRoot = node;
            else subRoot = node;
        } else {
            stack[stack.length - 1].node.children.push(node);
        }
        stack.push({ indent, node });
    }
    return { superRoot, subRoot };
}

async function nodeToTypeHierarchyItem(
    ctx: ServerContext,
    n: ParsedHierarchyNode,
): Promise<TypeHierarchyItem | null> {
    const info = await getSymbolInfo(ctx, n.file, n.line, n.col);
    if (info) {
        const item = symbolInfoToCallHierarchyItem(ctx, info);
        if (item) return item;
    }
    const uri = pathToUri(n.file);
    const lineContent = getLineContent(ctx, uri, n.line - 1);
    const start = rtagsToLspPosition(lineContent, n.line, n.col, ctx.encoding);
    const end = { line: start.line, character: start.character + n.name.length };
    return {
        name: n.name,
        kind: SymbolKind.Class,
        uri,
        range: { start, end },
        selectionRange: { start, end },
        data: { location: `${n.file}:${n.line}:${n.col}:` },
    };
}

async function onTypeHierarchyPrepare(
    ctx: ServerContext,
    params: TypeHierarchyPrepareParams,
): Promise<TypeHierarchyItem[] | null> {
    const { file, line, col } = lspToRcLocFromUri(
        ctx,
        params.textDocument.uri,
        params.position,
    );
    const info = await getSymbolInfo(ctx, file, line, col);
    if (!info) return null;
    const item = symbolInfoToCallHierarchyItem(ctx, info);
    if (!item) return null;
    return [item];
}

async function onTypeHierarchySupertypes(
    ctx: ServerContext,
    params: TypeHierarchySupertypesParams,
): Promise<TypeHierarchyItem[]> {
    const data = params.item.data as { location?: string } | undefined;
    const loc = data?.location ? parseLocString(data.location) : null;
    if (!loc) return [];
    const text = await execRc(
        [
            "--class-hierarchy",
            buildRcLoc(loc.file, loc.line, loc.col),
            "--absolute-path",
        ],
        ctx.rcOptions,
    );
    const { superRoot } = parseClassHierarchy(text);
    if (!superRoot) return [];
    const out: TypeHierarchyItem[] = [];
    const collect = async (n: ParsedHierarchyNode) => {
        for (const child of n.children) {
            const item = await nodeToTypeHierarchyItem(ctx, child);
            if (item) out.push(item);
        }
    };
    await collect(superRoot);
    return out;
}

async function onTypeHierarchySubtypes(
    ctx: ServerContext,
    params: TypeHierarchySubtypesParams,
): Promise<TypeHierarchyItem[]> {
    const data = params.item.data as { location?: string } | undefined;
    const loc = data?.location ? parseLocString(data.location) : null;
    if (!loc) return [];
    const text = await execRc(
        [
            "--class-hierarchy",
            buildRcLoc(loc.file, loc.line, loc.col),
            "--absolute-path",
        ],
        ctx.rcOptions,
    );
    const { subRoot } = parseClassHierarchy(text);
    if (!subRoot) return [];
    const out: TypeHierarchyItem[] = [];
    const collect = async (n: ParsedHierarchyNode) => {
        for (const child of n.children) {
            const item = await nodeToTypeHierarchyItem(ctx, child);
            if (item) out.push(item);
        }
    };
    await collect(subRoot);
    return out;
}

// ---------------------------------------------------------------------------
// Wire-up
// ---------------------------------------------------------------------------

function wrap<P, R>(
    handler: (ctx: ServerContext, params: P) => Promise<R>,
    ctx: ServerContext,
): (params: P) => Promise<R | null> {
    return async (params: P) => {
        try {
            return await handler(ctx, params);
        } catch (err) {
            ctx.connection.console.error(
                `handler failed: ${(err as Error).message}`,
            );
            return null as R | null;
        }
    };
}

export function registerHandlers(ctx: ServerContext): void {
    const c = ctx.connection;
    c.onDefinition(wrap(onDefinition, ctx));
    c.onDeclaration(wrap(onDeclaration, ctx));
    c.onTypeDefinition(wrap(onTypeDefinition, ctx));
    c.onImplementation(wrap(onImplementation, ctx));
    c.onReferences(wrap(onReferences, ctx));
    c.onDocumentHighlight(wrap(onDocumentHighlight, ctx));
    c.onHover(wrap(onHover, ctx));
    c.onWorkspaceSymbol(wrap(onWorkspaceSymbol, ctx));
    c.onDocumentSymbol(wrap(onDocumentSymbol, ctx));
    c.onPrepareRename(wrap(onPrepareRename, ctx));
    c.onRenameRequest(wrap(onRename, ctx));
    c.onCompletion(wrap(onCompletion, ctx));
    c.onCodeAction(wrap(onCodeAction, ctx));
    c.languages.callHierarchy.onPrepare(wrap(onCallHierarchyPrepare, ctx));
    c.languages.callHierarchy.onIncomingCalls(
        wrap(onCallHierarchyIncomingCalls, ctx),
    );
    c.languages.callHierarchy.onOutgoingCalls(
        wrap(onCallHierarchyOutgoingCalls, ctx),
    );
    c.languages.typeHierarchy.onPrepare(wrap(onTypeHierarchyPrepare, ctx));
    c.languages.typeHierarchy.onSupertypes(wrap(onTypeHierarchySupertypes, ctx));
    c.languages.typeHierarchy.onSubtypes(wrap(onTypeHierarchySubtypes, ctx));
}
