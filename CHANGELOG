2.33
=========================
- Rework how daemons work. Make it possible to separately configure
  how many daemons you have

2.32
=========================
- Try to reduce memory usage with --rp-daemon

2.31
=========================
- Remove experimental lua support. It never worked correctly and I
  doubt many people feel like it's the right language for the job if
  we were to offer such a feature

2.30
=========================
- A couple of fixes regarding references in template functions/classes
  which previously did not work correctly.
- Introduce --rp-daemon mode which keeps rp processes alive and caches
  their translation units. This can markedly speed up indexing times
  when repeatedly dirtying the same files
- Kill old translation unit cache concept which was never fully
  functional

2.22
=========================

- Make diagnostics from completions work and enable them by
  default. They seem to work well and are very fast.
- Fix some warnings and build issues
- Fix https://github.com/Andersbakken/rtags/issues/1299 by killing the
  isTemplateDiagnostic concept. It can be determined by ClangIndexer
  way faster.
- Fix https://github.com/Andersbakken/rtags/issues/1286
  Do not explicitly validate the project while it's still indexing.
- Various contributions regarding completions and unsaved files
- Add support to --is-indexing for checking against a single project
- Support regex matching in the argument to --is-indexing
- Do not remove characters from the display name of FieldDecl symbols
- Add support to --remove for removing all files in
  compile_commands.json
- Documentation is now available in the wiki
- Introduce rtags-asm-file which runs the compile command with -S to
  produce assembler code in a buffer.

2.21
=========================

- Don't enable diagnostics automatically in `ac-init`
- Don't include private members on code completion, if not appropriate
- Fix warnings
- rtags can now installed to a tramp location
- Fix `rtags-find-file` for tramp locations.

2.20
=========================

- Diagnostics from completions can be enabled with
  --completion-diagnostics. It seems quite fast but may have
  bugs. We'll keep working on it.
- A nasty bug with our file system watcher and inotify would cause us
  to stop getting modifications from compile_commands.json. This has
  been an absolute thorn in RTags' side for years. I'm extremely
  pleased to finally have found this bug. This very severely
  impacted my personal rtags experience and likely has for lots of
  other people as well.
- Fix a weird edge-case bug when running rdm with --no-realpath and
  re-running without this switch (and vice-verse)

2.19
=========================

- Seemingly improved performance for completions due to better
  handling of unsaved files. Apparently stdin and processes in emacs
  are not our friends. Tempfiles on the other hand seem to be
  significantly faster.
- Make skipped ranges work again.
- Add a new feature, rtags-find-dead-functions (Issue #1152)
- Various contributions that improve code completion
- Fix an issue when jumping to macro definition from a macro
  expansion.
- Fix issue #1214. Socket file doesn't need to be resolved.
- Various other fixes, code cleanup and infrastructure work.
