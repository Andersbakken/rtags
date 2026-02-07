# RTags Changelog

## 2.44
- Fix `FindLibClang.cmake` regex to match define values with `=` and
  digits (e.g. `-D_FILE_OFFSET_BITS=64`), fixing build failure on
  Debian with LLVM 18 (PR #1439)
- Blacklist additional ARM code generation flags (`-fconserve-stack`,
  `-fno-allow-store-data-races`, `-fno-ipa-sra`, `-mno-fdpic`,
  `-ftrivial-auto-var-init=`) that don't affect parsing (PR #1440)
- Fix code completion over TRAMP with unsaved buffers by creating temp
  files on the remote host with `make-nearby-temp-file` (PR #1441)
- Bump `cmake_minimum_required` to 3.5 in remaining CMakeLists files
  for CMake 4.0 compatibility (PR #1443)

## 2.43
- Add Vitest integration test suite (59 tests) replacing the old
  Python/pytest tests. Tests cover follow-location, find-references,
  code completion, templates, namespaces, enums, macros, overloads,
  multi-TU indexing, inheritance, typedefs, operators, and more.
- Add GitHub Actions CI replacing Travis CI. Builds and tests on
  Linux (gcc) and macOS (clang) in both Debug and Release. Releases
  with source tarballs are created automatically on tag push.
- Fix synchronous `Process::exec()` hanging indefinitely.
  `ProcessThread` failed to call `finish()` for sync processes because
  the event loop was null, leaving the caller's `select()` blocked
  forever. This caused rdm to hang on startup when `isCompiler()` ran
  a trivial compile test.
- Fix `Process` destructor asserting when child is still running;
  kill the child instead.
- Make `bump-version.sh` and `gen-man-pages.sh` work with macOS BSD
  sed (no longer requires `gsed`)
- Fix `readlink -f` portability in `bump-version.sh`
- Replace deprecated `exec_program()` with `execute_process()` in
  rct CMake
- Fix CPack source tarball excluding `node_modules` and build
  directories
- Clean up `.gitignore`

## 2.42

### Core / Indexing
- Move to C++17
- Major rework of project handling for repositories with multiple
  compile_commands.json files. Projects sharing the same source
  directory are now handled more correctly, avoiding unnecessary
  project switching and duplicate diagnostics. Much of the
  initialization logic has been consolidated into `Project::init()`.
- Refactor `processParseData` to be more correct about detecting
  changed compile_commands.json and re-indexing accordingly
- Remove projects whose compile_commands.json is missing during startup
- Delay saving file IDs until the whole project is loaded, improving
  load reliability
- Delete project nodes on failure in `loadDependencies` instead of
  leaving partial state
- Fix `removeProject` and handle removed sources better
- Handle removed projects in job scheduler without crashing
- Fix fixits: update fixits and dependencies before validation check
- Fix a crash when the file is not indexed
- Discard pch files during indexing
- Remove deprecated `unary_function` usage

### Template Handling
- Significant improvements to find-references and follow-symbol for
  C++ template specializations. Previously, template member functions
  (e.g. `vector<int>::push_back`) could get different USRs depending
  on whether they were processed as a declaration or reference,
  breaking navigation.
- Register resolved template USR in `handleCursor` so specialization
  members are reachable from both their specialized and primary
  template USRs
- Apply `resolveTemplateUsr` to function templates in `handleReference`,
  making USR resolution consistent across all code paths
- Collect template specializations from blocked file references so
  cross-file template references aren't silently lost
- Merge targets from unvisited (blocked) units instead of dropping
  them, fixing missing references in multi-TU scenarios
- Add USR mismatch detection logging (set `RTAGS_VERIFY_USR=1` to
  enable)
- Improve typedef resolving so `typedef struct s s;` references point
  to the struct definition rather than the typedef

### Include Path / Compiler Handling
- Place default includes at the beginning of the include list to avoid
  picking up gcc headers (e.g. `smmintrin.h`) instead of clang's
- Add `-cxx-isystem` support
- Add `-nostdinc++` when CompilerManager provides C++ stdlib paths
- Make it possible to disable `-nostdinc` and `-nostdinc++` with new
  flags
- Force-include `limits.h` on Linux for `PATH_MAX` availability. GCC's
  header chain transitively provides this but clang's does not, causing
  spurious errors during indexing. Disable with
  `--no-force-limits-include`.
- Attempt to use Xcode command line tools stdc++ include path on macOS
- Handle Emscripten's different compiler flags
- Source argument parsing fixes

### Suspend / Project Management
- Add `--suspend=project` and `--suspend=clear-project` commands for
  per-project suspend control
- Fix project suspend/unsuspend behavior
- Check after unsuspending unless rdm was started with
  `--no-unsuspend-check`
- Improvements in current project handling and
  `rtags-set-current-project`
- Don't send diagnostics for a project if it's not the current one and
  the current one indexes the same path

### Navigation / Symbols
- Support `make_unique` jump-to-constructor in the same way as
  `make_shared`
- Store and print mangled name for symbols (Fix #1346)
- Fix regex search
- Update refKind and refLoc after possible mutation in handler function
- Fix thread safety: use locked accessor for `mVisitedFiles` in
  `findDeadFunctions`

### Build System
- Disable RTTI to fix incompatibility between gcc's `cxxabi.h` and
  clang's on LLVM 18
- Check for ninja during CMake configuration
- Fix CMake configuration on macOS

### Emacs / Elisp
- Support Xref renaming via `xref-query-replace-in-results` by
  creating `xref-match-items` instead of `xref-items`
- Fix Xref column number (point was off by one when iterating results)
- Allow native-compile (`.eln`) compilation of elisp files and track
  `.eln` timestamps
- Use `pos-bol`/`pos-eol` instead of deprecated
  `point-at-bol`/`point-at-eol`
- Add `when` argument to `define-obsolete-function-alias` (mandatory
  since recent Emacs)
- Fix Emacs 29 byte-compile warnings
- Better quoting of single quotes in elisp
- Correct typos and syntax errors in elisp code
- Refactor `company-rtags` ignored parameter for clarity

### rct
- Multiple rct submodule updates including a process shutdown crash fix

## 2.41
- Use `RULE_LAUNCH_COMPILE` to enable ccache when available
- Bump rct

## 2.40
- No user-visible changes (version bump for build/packaging)

## 2.39
- Make compiler-manager enabled by default
- Split message handlers for Server into a separate file for cleaner code
- No fixits in system headers
- Fix trailing colon handling so case statements don't include it in the token
- Fix `clang_Type_getSizeOf` crash when it returns a negative value
- Fix template argument USR naming in clang
- Don't remove source files on filesystem changes; rely on
  compile_commands.json regeneration instead
- Partial support for `make_shared` to jump directly to the appropriate
  constructor
- Add vast, a Qt-based GUI tool to display the AST for a translation unit
- Improve `find-dead-callers`: print when there are 0 or 1 callers
- Default to enable tooltip for errors
- Widen narrowed buffer before `set-buffer-multibyte`
- Fix `rtags-references-tree` with a single match
- Fix `rtags-call-rc` argument handling (Fix #1370)
- Fix preprocessing for gcc (clang started adding arguments gcc doesn't understand)
- Bump LibClang major version to 11
- Various rct bumps and include cleanups

## 2.38
- Remove `--man` option from rc
- Port automated tests from nose to pytest
- Remove cotire.cmake
- Prepare rtags-xref.el for MELPA release
- Enable `lexical-binding` in elisp files
- Fix byte-compile errors and warnings under Emacs head (Fix #1359)
- Use `cl-defun` directly instead of creating a defalias
- Fix issue #1347
- Add clang 9 support

## 2.37
- Remove dependency on `seq` library which is only available in Emacs 25+
- Fix protocol version vs database version mismatch

## 2.36
- No user-visible changes (version bump for build/packaging)

## 2.35
- Add Xref support for rtags (`rtags-xref.el`)
- Add CUDA file extensions
- Kill `SocketClient::Synchronous`
- Use `switch-to-buffer` for `rtags-find-file` to achieve intended behavior
- Insert default compiler arguments right after the compiler
- Actually kill jobs when reducing job count
- Fix `(thing-at-point 'symbol)` cursor position accuracy
- Fix `rtags-rdm-includes` type to be a list of strings (Fix #1353)
- Fix curl download progress display
- Improve macOS LLVM prefix detection (use `brew --prefix`)
- Fix issue #1302
- Fix issue #1342
- Fix issue #1343

## 2.34
- Implement `--symbol-info-include-source-code` to include source code
  in symbol info output
- Introduce `QueryMessage::HasMatch`
- Make it possible to push/pop the job count
- Don't set `BUILD_TESTING` to `True` by default (Fix #1328)
- Enable `-Wzero-as-null-pointer-constant`
- Block `-save-temps` and similar flags (Fix #1323)
- Fix issue #1324
- Fix issue #1330
- Fix issue #1269
- Fix issue #1274
- Fix `ivy-rtags-collection` result ordering (Fix #1336)
- Improve error messages

## 2.33
- Rework how daemons work. Make it possible to separately configure
  how many daemons you have.

## 2.32
- Try to reduce memory usage with `--rp-daemon`

## 2.31
- Remove experimental lua support. It never worked correctly and is
  unlikely the right language for the job if such a feature were to
  be offered.

## 2.30
- A couple of fixes regarding references in template functions/classes
  which previously did not work correctly
- Introduce `--rp-daemon` mode which keeps rp processes alive and
  caches their translation units. This can markedly speed up indexing
  times when repeatedly dirtying the same files.
- Kill old translation unit cache concept which was never fully
  functional

## 2.22
- Make diagnostics from completions work and enable them by default.
  They seem to work well and are very fast.
- Fix some warnings and build issues
- Fix #1299 by killing the `isTemplateDiagnostic` concept. It can be
  determined by ClangIndexer way faster.
- Fix #1286: Do not explicitly validate the project while it's still
  indexing.
- Various contributions regarding completions and unsaved files
- Add support to `--is-indexing` for checking against a single project
- Support regex matching in the argument to `--is-indexing`
- Do not remove characters from the display name of FieldDecl symbols
- Add support to `--remove` for removing all files in
  compile_commands.json
- Introduce `rtags-asm-file` which runs the compile command with `-S`
  to produce assembler code in a buffer

## 2.21
- Don't enable diagnostics automatically in `ac-init`
- Don't include private members on code completion, if not appropriate
- Fix warnings
- RTags can now be installed to a tramp location
- Fix `rtags-find-file` for tramp locations

## 2.20
- Diagnostics from completions can be enabled with
  `--completion-diagnostics`
- Fix a nasty bug with the file system watcher and inotify that would
  cause us to stop getting modifications from compile_commands.json.
  This has been an absolute thorn in RTags' side for years.
- Fix a weird edge-case bug when running rdm with `--no-realpath` and
  re-running without this switch (and vice-versa)

## 2.19
- Seemingly improved performance for completions due to better
  handling of unsaved files. Tempfiles are significantly faster than
  stdin/processes in Emacs.
- Make skipped ranges work again
- Add `rtags-find-dead-functions` (Issue #1152)
- Various contributions that improve code completion
- Fix an issue when jumping to macro definition from a macro expansion
- Fix issue #1214: Socket file doesn't need to be resolved
- Various other fixes, code cleanup and infrastructure work

## 2.18
- Fix #1137: Avoid `strcasestr` for portability
- Support `RTAGS_NO_INSTALL` to skip installation
- Fix #1130
- Fix #1125: Add a note about RHEL
- Prevent slicing when catching exceptions
- Update Travis CI to newer clang versions

## 2.17
- Fix #1131: Diagnostics filtering when the intersection of the
  filter and the active set is empty
- Make diagnostics in mode-line clickable
- Error message handling, diagnostics, mode-line, key bindings, and
  face improvements in rtags.el
- Fix #1121: Don't erase buffer when output is nil
- Fix #1118
- Add `rtags-results-buffer-other-window` and
  `rtags-other-window-function` defcustoms
- Fix auto-complete issue
- Only update buffer list when it changes
- Build fixes for libc 2.5

## 2.16
- Fix clang-tidy performance issues
- Fix asan issue in StringTokenizer
- Fix #1089: Kill checksum validation since release tarballs are
  available on HTTPS
- Fix #1101: Make parseable output work
- Fix #1071
- Repurpose the `onJobFinished::Scope`
- Allow setting max write buffer size
- Various rct bumps and fixes

## 2.15
- Fix memory leak
- Implement "rename with multiple cursors"
- Diagnostics from code completion (initial work)
- Move `ClangIndexer::diagnose` to `RTags::diagnose` for reuse in
  completion thread
- Fix #1054, #1055, #1056, #1060
- Pass `CXTranslationUnit_KeepGoing` when available
- Optimize diagnostics: ignore things that aren't in visible buffers
- Clean up recovery of projects. Store file id separately for compile
  commands.
- Support `ASAN` build option with different sanitizer types
- ObjC method expression improvements
- Various rct bumps

## 2.14
- Fix #1040
- Add `[[gnu::fallthrough]]` to cases which fall through

## 2.13
- Fix #1035
- Fix function pointer field bug
- Fix #1031: Detect if json.h is supported and turn off json output
  for completions if it isn't
- Fix #1023: Don't scan for files synchronously when switching project
- Fix #1027
- Add switch to disable the libclang include path (Fix #989)
- Add example scripts for running rdm as a service using System V or
  systemd
- Correct the order of candidates returned by `helm-rtags`
- Better version detection for clang
- Require GCC >= 4.9

## 2.12
- Fix #978
- Fix #1008: Make a warning about `/tmp` into an error

## 2.11
- Add smarter completion to rtags (#584)
- Support regex (`-Z`) for `--find-symbols` (Fix #1004)
- Support `-idirafter` (Fix #994)
- Fix #1005: Don't rely on `subr-x`; steal the two needed functions
- Fix #1000: Don't overwrite contents of buffer in `find-file-hook`
- Fix #1003: Bump-version script for releases
- Fix #983: Compile a small test app with libclang to verify headers
- Fix #975: Make `--all-targets` work better with multibuilds
- Fix #977
- Recalculate priorities when buffer list changes and when header
  errors change
- Don't start jobs for suspended files
- Blacklist emacs as a compiler
- Add coding style section to documentation
- Delay updating buffers until later (avoid work in
  `kill-buffer-hook`)

## 2.10
- Fix a dependency node issue with incorrect re-indexing
- Fix #967
- Fix #945: Let `.projectile` take precedence over other project roots
- Fix #946: Allow setting the rtags install directory with
  `rtags-install-path`
- Fix #944: `-DFOO` is different from `-DFOO=`
- Fix #959: Empty file case for diagnostics XML
- Fix #956: Make company-mode completion asynchronous
- Fix #951: Fix serialization when using sandbox
- Fix #953: Make `rtags-modeline` work again with `rdm --progress`
- Fix #954: Syntax highlighting in `rtags-imenu`
- Fix #958: Don't use newish `package.el` features
- Add clang includes even when compiler manager is on
- Fix preprocessor: remove `-o` option, add missing `-E` in banner
- Add `rtags-tracking` functionality to `ivy-rtags`
- Make it possible to install rtags inside ELPA install directory
- Prepare for MELPA package split
- Add `rtags-display-result-backend` variable
- Fix #928: `c-mode-common-hook` includes Java; use proper hooks
- Helm output highlighting and documentation
- Use CPack for generating releases

## 2.9
- Fix #908, #918, #923, #924
- Store unsaved file contents in the database (Fix #911)
- Man page generation improvements
- Fix #895: Allow passing arbitrary extra arguments with
  `--default-argument`
- Add ivy support
- Handle `--current-file` for `-w`
- Fix regex crash with GCC 4.8
- Improve validation and recovery when database goes bad
- Support response files in command line arguments
- Fix #881
- Fix #880: Make `--references` support `--json`
- Add containing function and containing function location to
  `--symbol-info` output
- Support XDG basedir specification
- Set an error handler for mismatched rc connections
- Add `expand-all` and `collapse-all` for `rtags-references-tree-mode`
- Make sort stable to avoid crashes

## 2.8
- Major rework of how multiple source builds are handled. Sources are
  now stored as a list instead of a set, with better handling of
  `--allow-multiple-sources`
- Work on template references and template specialization dump
- Handle multiple targets better
- Follow typedefs for subclass detection
- Fix #875, #870, #872, #866, #867, #842, #837
- Fix constructors called from class/struct initializer lists
- Resolve template for constructors
- Add an `RTAGS` preprocessor define that is set when indexing
- Performance optimizations driven by Instruments profiling
- Store when compile_commands.json was modified
- Make `--dump-file-maps` and `--dump-file` work again
- Make rtags work from indirect buffers

## 2.7
- Partially fix #623: Introduce a defcustom to decide how to bury
  rtags buffers
- Add short-option for often-used options
- Keep track of time in `-s` jobs
- Somewhat fix `rtags-check-includes`

## 2.6
- Update WantedBy target for user systemd
- Fix #853: Check if valid regex before using input as regexp
- Fix #811: Set `LIBCLANG_LLVM_CONFIG_EXECUTABLE` explicitly when
  building clang as part of rtags
- Accept `-` as an argument
- Filter out results on the C++ side
- Present renames in the right order

## 2.5
- Fix #826: Don't put `int => int` etc for autos; don't mess up the
  symbolname for `decltype`
- Fix #830: `idx` points at the current argument
- Fix #829
- Fix #804: Move helm integration into its own file (`rtags-helm.el`)
- Fix #707: Avoid recursive load of rtags-helm
- Use `llvm-config` to find info about clang
- Resolve autos using `clang_getCanonicalType`
- Avoid TypeRef refs to classes/structs
- Add `rtags-include-file` interactive function with smarter `#include`
  completion and configurable recurse depth
- Handle multiple compile_commands.json files per project
- Add `rtags-eldoc`
- Make `rtags-error-warning-count` work again
- Fix fixits
- Various elisp optimizations

## 2.3
- Add default include paths to header completion
- Introduce `rtags-include-file` for `#include` completion
- Tokenize indexed source files and make tokens available
- Fix #675: Give preference to include completion
- Handle multiple compile_commands.json files per project
- Add `rtags-eldoc`
- Various fixes (#673, #674, #677)

## 2.2
- Initial Flycheck support (`flycheck-rtags`)
- Store whether functions are inline
- Add `->` return type display
- Fix #624: Make it possible to not use bookmarks
- Fix #619, #612, #617, #597, #598
- Don't include macros on `.|->|::` completion points
- More work on macro indexing
- Refactoring and option to print file names relative to project root
- Mark `rtags-start-process-maybe` as obsolete

## 2.1
- Fix #579: Taglist close-on-selection and close-on-focus-lost options
- Fix #575: Only use default exclude filters if none has been set
- Fix #572: launchd compatibility fix for 10.9
- Fix #571: Better default face for light theme
- Fix #570
- Fix #559: Add `rtags-jump-hook`
- Disable COTIRE by default
- Completion improvements
- Make file map validation opt-in
- Helm candidate buffer appearance improvements
