# AGENTS.md — RTags

Notes for future LLM sessions working in this repo. Read this before spelunking.

## What this project is

RTags is a **C/C++/ObjC code indexer** built on libclang. It is a client/server
system with a persistent, file-based database of symbols, references,
declarations, definitions, and diagnostics. Primary consumer is Emacs, but the
`rc` CLI is language-agnostic and there is now a Model Context Protocol
(MCP) server that exposes rtags to LLM agents.

Upstream: https://github.com/Andersbakken/rtags — GPLv3. Authored/maintained
by Anders Bakken (this checkout's owner). Do not add license headers of other
projects; every C++ file already carries the GPLv3 header.

## Top-level layout

```
src/            C++ sources for the three binaries + Emacs Lisp packages
src/rct/        Vendored 'rct' utility library (git submodule; separate LICENSE)
mcp-server/     TypeScript MCP server wrapping the `rc` CLI (new component)
bin/            Built binaries (rdm, rc, rp) + gcc wrapper shell scripts
tests/automated Python/pytest + Node/vitest driven integration tests
tests/lisp      Emacs Lisp unit tests
tests/manual    Manual repro cases
scripts/        Release + tooling helpers
man/            Man pages (generated via scripts/gen-man-pages.sh)
share/, web/    Auxiliary assets
CMakeLists.txt  Build entry point; also honors CMAKE_EXPORT_COMPILE_COMMANDS
compile_commands.json  Present at repo root — this repo indexes itself
```

## The three binaries

- **`rdm`** (`src/rdm.cpp` → `src/Server.cpp`) — the daemon. Owns the index,
  watches files, drives libclang indexer jobs (`IndexerJob`, `ClangIndexer`,
  `JobScheduler`), persists per-project DBs via `FileMap` / `DataFile`, and
  serves queries over a Unix socket.
- **`rc`** (`src/rc.cpp` → `src/RClient.cpp`) — the thin client. Parses
  command-line flags via `CommandLineParser.h`, builds a `QueryMessage`, and
  talks to `rdm`. This is the surface every editor integration (and the MCP
  server) drives.
- **`rp`** (built from `ClangIndexer.cpp` main path) — the out-of-process
  indexer. `rdm` spawns one `rp` per translation unit so a crashing libclang
  parse does not take down the daemon.

Key server-side classes to know before making changes:

- `Server` / `ServerMessageHandlers.cpp` — request dispatch.
- `Project` — one indexed project; owns symbols, targets, references, files.
- `Source` / `IndexParseData` — the parsed compile command for a TU.
- `Location` — packed (fileId, line, column); heavily used, don't change
  layout casually.
- `QueryJob` subclasses (`FollowLocationJob`, `ReferencesJob`,
  `FindSymbolsJob`, `ListSymbolsJob`, `ClassHierarchyJob`,
  `SymbolInfoJob`, `IncludePathJob`, `TokensJob`, `StatusJob`, …) — each
  `rc` query maps to one of these.
- `CompletionThread` — code completion path.
- `Sandbox` — path remapping for portable/relocatable databases.
- `FileManager`, `ScanThread`, `FileSystemWatcher` (in `rct/`) — file
  watching and enumeration.

## Emacs Lisp packages (also live in `src/`)

`rtags.el` is the main package. The satellite packages (`ac-rtags.el`,
`company-rtags.el`, `helm-rtags.el`, `ivy-rtags.el`, `flycheck-rtags.el`,
`rtags-xref.el`) all shell out to `rc`. Elisp tests live in
`tests/lisp/`. All of these ship on MELPA.

## `mcp-server/` — MCP wrapper (TypeScript, ESM, Node)

Standalone Node package, not built by CMake. Layout:

```
mcp-server/src/index.ts   Boots @modelcontextprotocol/sdk stdio server
mcp-server/src/rc.ts      execFile('rc', …) wrapper + env config
mcp-server/src/tools.ts   Registers the MCP tools (see list below)
mcp-server/package.json   `npm run build` → tsc, `npm run start` → node dist/index.js
```

Env vars honored by `rc.ts`:

- `RTAGS_RC_PATH` — path to the `rc` binary (default: `rc` on PATH)
- `RTAGS_SOCKET_FILE` — passed through as `--socket-file`
- `RTAGS_TIMEOUT_MS` — per-call timeout (default 30000)

Registered tools (all prefixed `rtags_`): `symbol_info`, `follow_location`,
`references`, `references_by_name`, `find_symbols`, `list_symbols`,
`code_complete`, `class_hierarchy`, `diagnose`. Each is a thin shim over
one or two `rc` flags — read `tools.ts` before adding another, and match
the existing style (return `rc` stdout, surface stderr as an error).

Failure modes surfaced with user-friendly messages: `ENOENT` on `rc`,
`Connection refused` / `Can't seem to connect` → tells the caller to
start `rdm --daemon`.

## Building

C++ side (out-of-tree build recommended, but this checkout builds in-source):

```sh
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
make          # or: ninja  (build.ninja is present)
```

Requires libclang + LLVM development headers. `scripts/getclang.sh` and
`scripts/llvm-osx` fetch/build clang locally if needed. rct is a submodule
(`git clone --recursive` when cloning fresh).

MCP server:

```sh
cd mcp-server
npm install
npm run build
```

## Tests

- `tests/automated/` — pytest + a newer Node/vitest suite (`rtags.test.mjs`,
  `vitest.config.mjs`). Fixtures are actual mini-projects under
  `tests/automated/<Name>/`. `conftest.py` and `utils.py` spin up an `rdm`
  per test.
- `tests/lisp/` — ERT-style Emacs Lisp tests.
- `tests/manual/` — human-driven repros; not run in CI.

Running the fast path:

```sh
cd tests/automated
pytest -x
# or:
npm test        # runs the vitest suite
```

## Style / conventions

- `.clang-format` exists (added in commit `5f81a368`) — **run clang-format
  on any C++ edit**. The recent history contains a repo-wide reformat, so
  diffs against that are the reference.
- Prefer `nullptr` over `0` / `NULL` for pointers (see commit `9a452df1`).
- C++ standard is what libclang + rct require; check `CMakeLists.txt`
  before bumping. No exceptions/RTTI assumptions beyond what rct uses.
- Do not touch `src/rct/` casually — it is a submodule with its own
  upstream. Bumping it is a deliberate commit (`Bump rct.`).
- Emacs Lisp: follow the existing `rtags-` prefix and defcustom style;
  user-visible defaults should be conservative (see `41b1390c` — mouse
  bindings default off).

## Working effectively in this repo

- The repo has `compile_commands.json` at the root and rtags itself is the
  intended tool for navigating it. If `rdm` is running and this project
  is indexed, `rc -f`, `rc -r`, `rc -F`, `rc -S` are faster and more
  accurate than grep for C++ navigation. The `rtags-rc` skill (if
  available in your agent) knows the flags.
- If `rdm` is not up, prefer `codegraph_explore` / codegraph node lookups
  or ast-grep over blind grep for the C++ tree — it is large and
  header-heavy.
- The MCP server is the natural place to add new agent-facing capabilities.
  New tool = new entry in `tools.ts` + one `execRc` call. Do not
  reimplement query logic in TypeScript; delegate to `rc`.
- When adding a new `rc` query, the pattern is: add a `QueryMessage` flag
  → handle it in `ServerMessageHandlers.cpp` → implement a `QueryJob`
  subclass → wire the flag into `RClient.cpp` / `CommandLineParser.h`.

## Things that will bite you

- `Location` is a packed 64-bit id, not a struct of ints — do not
  serialize it directly across versions.
- `FileMap` / `DataFile` on-disk formats are versioned; changing a struct
  serialized to disk requires bumping the version or projects will fail
  to load. Grep for `Version` / `sVersion` in the touched header.
- `rp` runs as a subprocess and communicates via `Connection` — do not
  assume shared globals with `rdm`.
- Path handling goes through `Sandbox` for portability; use it instead of
  hardcoding absolute paths.
- The Travis badge in README.org is stale; CI is not currently green
  upstream on all platforms. Don't panic if you see historical red.

## When making changes

1. Read the touched header + its `.cpp` fully — this codebase leans on
   header-declared inline helpers and friend classes.
2. clang-format the diff.
3. If you changed `rc` output, update `mcp-server/src/tools.ts` if the
   affected command is exposed there, and the tests under
   `tests/automated/`.
4. Do not commit unless explicitly asked.
