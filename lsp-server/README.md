# rtags-lsp-server

A Language Server Protocol wrapper around rtags' `rc` CLI. Lets
any LSP-aware editor use rtags for C/C++ navigation, references,
hover, rename, diagnostics, code actions, and call/type
hierarchies.

Implementation details, architecture, and rationale live in
[PLAN.md](./PLAN.md).

## Requirements

- Node.js ≥ 20
- A running `rdm` daemon with at least one project indexed
- `rc` on `PATH` (or `RTAGS_RC_PATH` set; see below)

## Build

```sh
cd lsp-server
npm install
npm run build
```

Produces `dist/index.js`. Re-run `npm run build` after edits.

## Run

The server speaks LSP over stdio. The `--stdio` flag is required:

```sh
node /path/to/lsp-server/dist/index.js --stdio
```

Editors typically launch this themselves — see [Editor
configuration](#editor-configuration) below.

## Environment variables

| Variable             | Default                              | Purpose                                                          |
|----------------------|--------------------------------------|------------------------------------------------------------------|
| `RTAGS_RC_PATH`      | `rc`                                 | Path to the `rc` binary if not on `PATH`.                        |
| `RTAGS_SOCKET_FILE`  | rc default (`$XDG_RUNTIME_DIR/rdm.socket` or `~/.rdm`) | Custom rdm socket path; passed to `rc` as `--socket-file`.       |
| `RTAGS_TIMEOUT_MS`   | `30000`                              | Per-`rc`-invocation timeout in milliseconds. Set 0 for none.     |

## Editor configuration

### Helix (`~/.config/helix/languages.toml`)

```toml
[language-server.rtags]
command = "node"
args = ["/absolute/path/to/lsp-server/dist/index.js", "--stdio"]

[[language]]
name = "cpp"
language-servers = ["rtags"]
```

You can list multiple servers (e.g. clangd + rtags) and Helix will
merge their results.

### Neovim (with `nvim-lspconfig` or directly)

```lua
vim.lsp.start({
  name = "rtags",
  cmd = { "node", "/absolute/path/to/lsp-server/dist/index.js", "--stdio" },
  root_dir = vim.fs.root(0, { "compile_commands.json", ".git" }),
})
```

### VSCode

VSCode does not launch arbitrary stdio LSP servers without a
wrapper extension. Easiest: install a generic LSP client
extension (e.g. "Generic LSP Client") and point it at the same
`node ... dist/index.js --stdio` command.

### fresh / others

Any LSP client that can spawn an stdio server works. Point it at
`node /path/to/lsp-server/dist/index.js --stdio`.

## Capabilities

Provided:

- `textDocument/definition`, `declaration`, `typeDefinition`, `implementation`
- `textDocument/references` (honors `includeDeclaration`)
- `textDocument/documentHighlight`
- `textDocument/hover`
- `workspace/symbol`, `textDocument/documentSymbol`
- `textDocument/prepareRename`, `textDocument/rename`
- `textDocument/completion`
- `textDocument/codeAction` (quickfix from rtags fixits)
- `textDocument/publishDiagnostics` (live stream from `rc --diagnostics`)
- `callHierarchy/prepare` + `incomingCalls`
- `typeHierarchy/prepare` + `supertypes` + `subtypes`

Not provided (deliberately — see PLAN.md "Out of scope" for
reasons): semantic tokens, inlay hints, code lens, formatting,
`callHierarchy/outgoingCalls` (returns `[]`).

## Unsaved buffers

Every query passes `--unsaved-file PATH:LEN` for **all** documents
the editor has open in the LSP session, with the bytes piped on
stdin. Queries reflect live buffer state, not last-saved disk
state.

The streaming diagnostics process (`rc --diagnostics`) does NOT
see unsaved buffers — diagnostics only update after a save and
rdm reindex. This is an rdm limitation, not a wrapper one.

## Troubleshooting

- **"rdm is not running"**: start it with `rdm --daemon`.
- **"rc binary not found"**: set `RTAGS_RC_PATH=/abs/path/to/rc`.
- **Connection input stream is not set**: you launched the server
  without `--stdio`. The flag is mandatory.
- **Stale results**: queries see the editor's open buffer state.
  Diagnostics see last-saved disk state. If you've edited a file
  and diagnostics look stale, save the file.
- **Slow `documentSymbol`**: this handler issues `2N+1` `rc` calls
  for `N` symbols in the file; results for unmodified files are
  cached by mtime. For large open-and-edited headers, expect
  noticeable latency on first invocation.

## License

BSD (matches rtags itself).
