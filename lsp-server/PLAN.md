# rtags LSP server — implementation plan

Status: v1 implemented and smoke-tested against an indexed C++
project. See `sample-outputs/` for captured `rc` ground truth and
the file table at the bottom of this doc for which sources
implement which methods. Pending discussion with rtags maintainer
about whether this lives in the rtags tree or as a separate repo.

## Goal

Expose rtags' indexing and navigation features via the Language
Server Protocol so any LSP-aware editor (fresh, Helix, Neovim,
VSCode, Zed, Sublime LSP) can use rtags without a per-editor
integration.

A TypeScript wrapper that shells out to the existing `rc` CLI.
Same pattern as `../mcp-server/`, different protocol.

## Why this shape

`mcp-server/` is 390 lines of TypeScript that exposes 9 rtags
features to MCP clients by calling `rc` and translating the
output. It proves the rc-shell-out approach. The LSP wrapper is
structurally identical — different protocol layer, same `rc.ts`
core, more methods.

Alternatives considered:

- **Native C++ LSP in rtags itself.** Larger (~2000-3000 lines),
  more code to own, faster runtime. Worth it only if `rc` startup
  cost becomes unacceptable.
- **TS wrapper talking to `rdm` directly.** Skips `rc` startup
  overhead. Requires reimplementing rdm's wire protocol in TS
  (~1000 lines). Defer until perf demands it; the wire surface is
  the same either way, so swapping the bottom layer later is cheap.

## Decisions to confirm with maintainer

1. Lives in this directory mirroring `../mcp-server/`, or
   separate repo? Default assumption: in-tree, mirrors MCP layout.
2. Node ≥20 (matches mcp-server's `@types/node: ^22`).
3. `vscode-languageserver` + `vscode-languageserver-textdocument`
   for LSP framing.
4. Unsaved-buffer awareness via `--unsaved-file` is enabled in v1
   for all open documents on every query.
5. License: match rtags itself (BSD).

## File layout (current)

```
rtags/lsp-server/
  package.json
  tsconfig.json
  src/
    index.ts        # connection setup, capabilities, document sync, diag stream lifecycle
    rc.ts           # extended copy of mcp-server/src/rc.ts (added stdin-piping for --unsaved-file)
    handlers.ts     # all LSP method handlers, sectioned by area
    transforms.ts   # rtags ↔ LSP coord conversion, location parsers
    diagstream.ts   # long-lived `rc --diagnostics`, NDJSON parser, fixit cache
  sample-outputs/   # ground-truth captured rc outputs (see README.md)
```

## LSP method → `rc` mapping (implemented)

| LSP method                       | `rc` invocation                                                                                                                | Output       | Notes                                                        |
|----------------------------------|---------------------------------------------------------------------------------------------------------------------------------|--------------|--------------------------------------------------------------|
| `textDocument/definition`        | `rc -f FILE:LINE:COL --absolute-path`                                                                                          | TEXT         | Parse `loc:line:col:\tcontext`. `--json` ignored.            |
| `textDocument/declaration`       | `rc -U LOC --symbol-info-include-targets --json`; pick targets where `definition: false`                                        | JSON         | Falls back to all targets if no decl-only target exists.     |
| `textDocument/typeDefinition`    | `rc -U LOC --symbol-info-include-targets --json`; pick targets whose `kind` is a type-decl. Fallback: parse `type` and `rc -F TYPENAME` | JSON + TEXT  | Returns null for primitives.                                 |
| `textDocument/implementation`    | `rc -f FILE:LINE:COL --all-targets --absolute-path`                                                                            | TEXT         | Same parser as definition.                                   |
| `textDocument/references`        | `rc -r FILE:LINE:COL --absolute-path --json`                                                                                   | JSON         | `--all-references` when `context.includeDeclaration`.        |
| `textDocument/documentHighlight` | `rc -r FILE:LINE:COL --path-filter FILE --all-references --absolute-path --json`                                                | JSON         | Same shape as references; restricted to current file.        |
| `textDocument/hover`             | `rc -U LOC --symbol-info-include-parents --json`                                                                                | JSON         | Renders symbolName, type, kind, linkage, container, parent, size/align as markdown. |
| `workspace/symbol`               | `rc -F QUERY --absolute-path --display-name --cursor-kind --max 200 [--path-filter FOLDER ...]`                                  | TEXT         | Parse 3-column `loc\tcontext\tdisplayName\tkind`. One `--path-filter` is appended per LSP `workspaceFolders` entry; `rc` treats multiple `--path-filter` flags as OR (any match). |
| `textDocument/documentSymbol`    | Three-pass. (1) `rc --list-symbols --path-filter FILE --path FILE --kind-filter <K>` → names. (2) per-name `rc -F NAME --path-filter FILE --display-name --cursor-kind --no-context --absolute-path` → locations. (3) per-location `rc -U LOC --json` for accurate `range` (body span) and `selectionRange` (symbolLength). | TEXT + TEXT + JSON | No `--imenu` flag exists; mirrors `rtags-imenu` from `rtags.el:4109`. The third pass is what gives a non-zero selectionRange and a body-spanning range; without it both collapse to a zero-width point. Closed-file results are cached LRU-by-mtime (cap 32). |
| `textDocument/prepareRename`     | `rc -U LOC --json`                                                                                                              | JSON         | Range from `startLine/Column`/`endLine/Column`.              |
| `textDocument/rename`            | `rc -r LOC --rename --all-references --absolute-path --json`                                                                    | JSON         | `--rename` is a NoValue modifier on `--references`. Build `WorkspaceEdit` keyed by URI; size each edit by `--symbol-info` symbolLength. |
| `textDocument/completion`        | `rc --code-complete-at LOC --synchronous-completions --code-complete-no-wait --absolute-path --json`                            | JSON         | Acceptable UX given rtags' synchronous-completion model.     |
| `textDocument/codeAction`        | Read fixits from diag-stream cache (`type:"fixit"`); fetch replacement text via `rc --fixits FILE` (TEXT: `line:col len replacement`); pair by `(line, col)`. | mixed | One `CodeAction { kind: "quickfix" }` per pair.              |
| `textDocument/publishDiagnostics`| Long-lived `rc --diagnostics --json --silent-query`                                                                             | NDJSON stream | One `{checkStyle: {file: Diagnostic[]}}` JSON object per line. Empty array per file = clear. Auto-restart with backoff on stream death. |
| `callHierarchy/prepare`          | `rc -U LOC --symbol-info-include-targets --json`                                                                                | JSON         | Returns target definitions as call-hierarchy items.          |
| `callHierarchy/incomingCalls`    | `rc -r LOC --containing-function --containing-function-location --absolute-path --json` then group by `cfl`                     | JSON         | Each caller becomes one `from` item; ref locations become `fromRanges`. |
| `callHierarchy/outgoingCalls`    | (returns `[]`)                                                                                                                  | -            | rtags has no inverse calls-from-this-function query. Implementing this would require walking function tokens and resolving each call-expression — feasible but deferred. |
| `textDocument/prepareTypeHierarchy` | `rc -U LOC --json`                                                                                                          | JSON         |                                                              |
| `typeHierarchy/supertypes`       | `rc --class-hierarchy LOC --absolute-path` (text), parse indented tree                                                          | TEXT         | `--json` is ignored. Parser splits `Superclasses:` / `Subclasses:` sections, indent-based. |
| `typeHierarchy/subtypes`         | same as supertypes                                                                                                              | TEXT         |                                                              |

`mcp-server/src/tools.ts` already had rc-arg construction for some
of these; cribbed for the LSP impl.

### Verified gotchas in `rc` JSON output

- `--json` is **only honored** by `--references`, `--symbol-info`,
  `--diagnose`, `--code-complete-at`, and `--diagnostics`. For
  `-f`, `-F`, `--list-symbols`, `--all-targets`, `--fixits`,
  `--class-hierarchy` it is silently ignored — those tools emit
  text. Each handler has its own parser.
- Locations in `rc` output have a trailing colon: `file:line:col:`.
  Strip when parsing.
- `--list-symbols` with `--path-filter` appends a trailing line
  with the project-relative file path. Discard.
- `--symbol-info` returns end-column exclusive
  (`startCol:23, endCol:35` for a 12-char identifier).
- Streaming diagnostic `type` values: `error`, `warning`, `note`,
  `fixit`. `note` entries appear in a `children` array on the
  parent diagnostic; mapped to `relatedInformation`. `fixit`
  entries carry `column`/`line`/`length`/`message` but NOT the
  replacement — paired with `rc --fixits FILE` text output.

## Coordinate translation

**Settled:** rtags uses `clang_getSpellingLocation`
(`src/ClangIndexer.cpp:57`), which returns column as 1-indexed
**byte offset**. `Location.cpp:128` confirms `column() - 1` is
used as a byte index.

| System          | Origin     | Line                | Column unit                      |
|-----------------|------------|---------------------|----------------------------------|
| rtags `rc`      | 1-indexed  | line `:`-separated  | bytes (confirmed)                |
| LSP `Position`  | 0-indexed  | `{line, character}` | UTF-16 code units (default)      |

`transforms.ts` converts both directions using line content from
the LSP-synced `TextDocument`. If the editor advertises `utf-8`
during `initialize`, the conversion collapses to a byte-shift.
For files the editor hasn't synced (e.g. `workspace/symbol`
returns a Location in an unopened file) we fall back to reading
the file from disk to perform byte→UTF-16 conversion.

## Unsaved-buffer support

Every query handler passes `--unsaved-file PATH:LEN` for **all**
documents currently open in the LSP session, with the bytes piped
on stdin. This means queries always see the editor's live buffer
state, not last-saved disk state. The streaming diag process is
NOT influenced by this — it gets the live state via rdm's own
buffer tracking, populated by `didChange` reindexes (out of
scope: rdm doesn't accept unsaved buffers from rc, so streaming
remains last-saved-state). Document this asymmetry in the README
when shipping.

## Diagnostics streaming

`rc --diagnostics --json --silent-query` is spawned in
`onInitialized` and read continuously:

1. Frame on `\n`. Each line is one `{checkStyle: {file: Diagnostic[]}}`
   JSON. Empty `[]` per file = clear.
2. Severity mapping:

   | rtags `type` | LSP `DiagnosticSeverity` |
   |--------------|--------------------------|
   | `error`/`fatal` | `Error` (1)           |
   | `warning`    | `Warning` (2)            |
   | `fixit`      | (cached for `codeAction`, not emitted as diagnostic) |
   | `note`       | rendered as `relatedInformation` on the parent |

3. Fixit entries are cached in `FixitCache` keyed by URI. The
   `codeAction` handler reads from this cache and pairs with
   `rc --fixits` text to get replacement strings.
4. On stream death, the server restarts the process with
   exponential backoff capped at 30s, reset on first successful
   parse. rdm restart should be transparent.

## Out of scope (with reason)

- **`textDocument/semanticTokens`, `inlayHint`, `codeLens`** —
  these fire on every visible-region change or every keystroke.
  At ~10-50ms per `rc` startup, this would degrade editor
  responsiveness. Skip.
- **`textDocument/formatting`** — rtags is not a formatter.
- **Inline completion / Copilot-style** — irrelevant for an
  index-based server.
- **`callHierarchy/outgoingCalls`** — rtags has no direct query
  for "what does this function call." Implementable by walking
  `--tokens FILE:START-END --tokens-include-symbols` and
  resolving call-expression tokens; deferred until a user
  actually wants it. Currently returns `[]`.
- **`workspace/configuration`** — env vars (`RTAGS_RC_PATH`,
  `RTAGS_SOCKET_FILE`, `RTAGS_TIMEOUT_MS`) cover the
  configuration surface.

## Workspace folders

Per-file operations (`definition`, `references`, `hover`,
`documentSymbol`, etc.) don't need folder awareness because rdm
routes per absolute path. `workspace/symbol` DOES filter by the
LSP `workspaceFolders` list — each open folder is appended as a
`--path-filter` (rc treats multiple `--path-filter` flags as OR).
This matters when rdm has multiple projects indexed: without the
filter, querying "withinRadius" in editor folder A would also
return matches from project B.

The server advertises
`workspace.workspaceFolders.changeNotifications` and updates the
filter list on `workspace/didChangeWorkspaceFolders` when the
client supports it. If `workspaceFolders` is absent the server
falls back to `rootUri`. If both are absent the filter list is
empty and `workspace/symbol` returns all matches across all
indexed projects (legacy behavior).

## Verified end-to-end

Smoke-tested by driving the server with a JSON-RPC harness against
an indexed C++ project. All of: definition, declaration,
typeDefinition, implementation, references (with and without
`includeDeclaration`), documentHighlight, hover, workspace/symbol,
documentSymbol, prepareRename, rename, completion,
callHierarchy/prepare, callHierarchy/incomingCalls,
typeHierarchy/prepare, typeHierarchy/supertypes,
typeHierarchy/subtypes — all returned correct, expected results.
publishDiagnostics streamed real diagnostics during the test
window.

## Risks

1. **`rc --json` lies about half the time.** Mitigation: per-method
   parsers, captured samples in `sample-outputs/`. No generic
   `execRcJson` is used for the affected commands.
2. **`rc --json` output drift across rtags versions.** Pin the
   rtags version this server is tested against. Document in README.
3. **rdm not running.** `rc.ts` detects "Connection refused" /
   "Can't seem to connect" and surfaces a clear error.
4. **`rc` startup cost (~10-50ms per call).** Fine for explicit
   user actions (definition, references, hover). The cursor-following
   features that would matter (inlay hints, semantic tokens) are
   excluded.
5. **`documentSymbol` three-pass cost.** `2N+1` `rc` invocations
   per file (one `--list-symbols` + one `-F` per symbol name + one
   `--symbol-info` per resolved location). The third pass is
   needed for accurate LSP `range`/`selectionRange`; without it
   both fields collapse to zero-width points which violates LSP
   spec ("selectionRange must be contained by range" only holds
   trivially) and breaks editor outline behavior. Mitigated by an
   LRU mtime-keyed cache (cap 32) when the file is not open. For
   an open file that's edited, the full cost is paid each time.
   Revisit if users complain — possible alternative is to derive a
   range from the `--cursor-kind`-augmented find-symbols output
   alone, accepting a less accurate body span in exchange for
   `N+1` calls.
6. **Streaming diag emission size.** Observed 47 KB single-line
   JSON. Parser uses an accumulating buffer; doesn't assume one
   read = one line.
7. **Unsaved-buffer payload size.** Every query ships all open
   buffers' contents on stdin. Acceptable for normal projects;
   could become large with many huge buffers. Restrict to the
   queried file or to dirty buffers if it bites.

## What's already done

- `src/index.ts` — connection, capabilities, position-encoding
  negotiation, diag-stream lifecycle.
- `src/rc.ts` — execRc + execRcJson + execRcWithUnsaved (stdin
  pipe) + execRcJsonWithUnsaved.
- `src/transforms.ts` — location parsers, byte/UTF-16/UTF-32
  conversions, URI helpers.
- `src/handlers.ts` — all handlers in the mapping table above.
- `src/diagstream.ts` — long-lived process, NDJSON parser, fixit
  cache, severity mapping, restart backoff.
- `sample-outputs/` — captured `rc` outputs for every command
  the handlers issue, plus a README legend.

## Reference

- `../mcp-server/src/rc.ts` — original of the rc.ts base.
  Diverged: this server's adds stdin piping.
- `../mcp-server/src/tools.ts` — argument shapes for several
  rtags queries.
- `sample-outputs/` — ground-truth `rc` outputs.
- `../src/RClient.cpp` — authoritative flag list.
- `../src/rtags.el:4109` — reference impl of `documentSymbol`
  composition.
- `../src/Location.cpp`, `../src/ClangIndexer.cpp` — confirm
  byte-indexed columns via `clang_getSpellingLocation`.
- `rc --help` for full flag reference.
- LSP spec: <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.18/specification/>
- `vscode-languageserver` API docs:
  <https://github.com/microsoft/vscode-languageserver-node>

## Notes for future work (only if a user actually asks)

- `callHierarchy/outgoingCalls` via `--tokens` walking.
- Hierarchical `documentSymbol` (currently flat) — would need to
  reconstruct parent/child links via `cf`/`cfl` per symbol.
- Streaming-diag awareness of unsaved buffers (would require rdm
  changes).
