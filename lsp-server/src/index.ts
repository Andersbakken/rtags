import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    type InitializeParams,
    type InitializeResult,
    PositionEncodingKind,
    TextDocumentSyncKind,
    CodeActionKind,
} from "vscode-languageserver/node.js";
import { TextDocument } from "vscode-languageserver-textdocument";
import { loadRcOptions, killActiveRcChildren } from "./rc.js";
import { registerHandlers, type ServerContext } from "./handlers.js";
import { DiagStream, FixitCache } from "./diagstream.js";
import { uriToPath, type PositionEncoding } from "./transforms.js";

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments<TextDocument>(TextDocument);

function negotiateEncoding(params: InitializeParams): PositionEncoding {
    const offered = params.capabilities.general?.positionEncodings ?? [];
    if (offered.includes(PositionEncodingKind.UTF8)) return "utf-8";
    if (offered.includes(PositionEncodingKind.UTF32)) return "utf-32";
    return "utf-16";
}

const fixits = new FixitCache();
const context: ServerContext = {
    connection,
    documents,
    rcOptions: loadRcOptions(),
    encoding: "utf-16",
    workspaceFolders: [],
    fixits,
};

let diagStream: DiagStream | null = null;

function foldersFromInit(params: InitializeParams): string[] {
    if (params.workspaceFolders && params.workspaceFolders.length > 0) {
        return params.workspaceFolders
            .map((f) => {
                try {
                    return uriToPath(f.uri);
                } catch {
                    return null;
                }
            })
            .filter((p): p is string => !!p);
    }
    if (params.rootUri) {
        try {
            return [uriToPath(params.rootUri)];
        } catch {
            return [];
        }
    }
    return [];
}

function applyWorkspaceFolderChange(event: {
    added: { uri: string; name: string }[];
    removed: { uri: string; name: string }[];
}): void {
    const removed = new Set(
        event.removed
            .map((f) => {
                try {
                    return uriToPath(f.uri);
                } catch {
                    return null;
                }
            })
            .filter((p): p is string => !!p),
    );
    const added: string[] = [];
    for (const f of event.added) {
        try {
            added.push(uriToPath(f.uri));
        } catch {
            // skip non-file URIs
        }
    }
    context.workspaceFolders = [
        ...context.workspaceFolders.filter((f) => !removed.has(f)),
        ...added,
    ];
}

connection.onInitialize((params): InitializeResult => {
    context.encoding = negotiateEncoding(params);
    context.workspaceFolders = foldersFromInit(params);
    if (params.capabilities.workspace?.workspaceFolders) {
        connection.workspace.onDidChangeWorkspaceFolders(
            applyWorkspaceFolderChange,
        );
    }
    const positionEncoding =
        context.encoding === "utf-8"
            ? PositionEncodingKind.UTF8
            : context.encoding === "utf-32"
              ? PositionEncodingKind.UTF32
              : PositionEncodingKind.UTF16;
    return {
        capabilities: {
            positionEncoding,
            textDocumentSync: TextDocumentSyncKind.Incremental,
            definitionProvider: true,
            declarationProvider: true,
            typeDefinitionProvider: true,
            implementationProvider: true,
            referencesProvider: true,
            documentHighlightProvider: true,
            hoverProvider: true,
            workspaceSymbolProvider: true,
            documentSymbolProvider: true,
            renameProvider: { prepareProvider: true },
            completionProvider: {
                resolveProvider: false,
                triggerCharacters: [".", ":", ">"],
            },
            codeActionProvider: {
                codeActionKinds: [CodeActionKind.QuickFix],
            },
            callHierarchyProvider: true,
            typeHierarchyProvider: true,
            workspace: {
                workspaceFolders: {
                    supported: true,
                    changeNotifications: true,
                },
            },
        },
        serverInfo: { name: "rtags-lsp-server", version: "0.1.0" },
    };
});


connection.onInitialized(() => {
    diagStream = new DiagStream({
        rcOptions: context.rcOptions,
        encoding: context.encoding,
        connection,
        fixits,
    });
    diagStream.start();
});

connection.onShutdown(() => {
    diagStream?.stop();
    diagStream = null;
    killActiveRcChildren();
});

connection.onExit(() => {
    killActiveRcChildren("SIGKILL");
});

registerHandlers(context);
documents.listen(connection);
connection.listen();
