/* This file is part of RTags.

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#include "RTagsClang.h"
#include "Server.h"
#include <rct/StopWatch.h>
#include "Cpp.h"
#include "Project.h"
#include <iostream>
#include <zlib.h>
#include "Token.h"

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#ifdef HAVE_BACKTRACE
#undef HAVE_BACKTRACE
#endif

#include <clang/Driver/Util.h>
#include <llvm/Config/config.h>
#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/TextDiagnosticBuffer.h>
#if CLANG_VERSION_MINOR < 4
#include <clang/Driver/ArgList.h>
using clang::driver::InputArgList;
#else
#include <llvm/Option/ArgList.h>
using llvm::opt::InputArgList;
#endif
#include <clang/Driver/Compilation.h>
#include <clang/Driver/Driver.h>
#include <clang/Driver/ToolChain.h>
#include <clang/Frontend/Utils.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Lex/PreprocessingRecord.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Host.h>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringRef.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Lex/HeaderSearch.h>
#include <clang/Lex/HeaderSearchOptions.h>
#if CLANG_VERSION_MINOR >= 5
#include <clang/Basic/LangOptions.h>
#endif

namespace RTags {
String eatString(CXString str)
{
    const String ret(clang_getCString(str));
    clang_disposeString(str);
    return ret;
}

String cursorToString(CXCursor cursor, unsigned flags)
{
    const CXCursorKind kind = clang_getCursorKind(cursor);
    String ret;
    ret.reserve(256);
    ret += eatString(clang_getCursorKindSpelling(kind));
    if (clang_isInvalid(kind))
        return ret;

    switch (RTags::cursorType(kind)) {
    case Reference:
        ret += " r";
        break;
    case Cursor:
        ret += " c";
        break;
    case Other:
        ret += " o";
        break;
    case Include:
        ret += " i";
        break;
    }

    const String name = eatString(clang_getCursorDisplayName(cursor));
    const String other = eatString(clang_getCursorSpelling(cursor));
    if (!name.isEmpty())
        ret += " " + name;
    if (other != name && !other.isEmpty())
        ret += " " + other;

    if (clang_isCursorDefinition(cursor))
        ret += " def";

    if (flags & IncludeUSR)
        ret += " " + eatString(clang_getCursorUSR(cursor));

    CXString file;
    unsigned line, col;
    for (int pieceIndex = 0;; ++pieceIndex) {
        CXSourceRange range = clang_Cursor_getSpellingNameRange(cursor, pieceIndex, 0);
        if (clang_Range_isNull(range))
            break;
        CXSourceLocation rangeStart = clang_getRangeStart(range);
        clang_getPresumedLocation(rangeStart, &file, &line, &col);

        const char *data = clang_getCString(file);
        if (data && *data) {
            ret += ' ';
            ret += data;
            ret += ':';
            ret += String::number(line);
            ret += ':';
            ret += String::number(col);
        }
        clang_disposeString(file);
    }
    return ret;
}

static inline bool matchContext(const String &symbolName, const String &context)
{
    const Map<Token, int> tokens = Token::tokenize(symbolName.constData(), symbolName.size());
    for (const auto &token : tokens) {
        if (token.first.length == context.length() && !strncmp(token.first.data, context.constData(), token.first.length))
            return true;
    }

    return true;
}

SymbolMap::const_iterator findCursorInfo(const SymbolMap &map, const Location &location)
{
    SymbolMap::const_iterator it = map.lower_bound(location);
    if (it != map.end() && it->first == location) {
        return it;
    } else if (it != map.begin()) {
        --it;
        if (it->first.fileId() == location.fileId() && location.line() == it->first.line()) {
            const int off = location.column() - it->first.column();
            if (it->second->symbolLength > off)
                return it;
        }
    }
    return map.end();
}

void parseTranslationUnit(const Path &sourceFile, const List<String> &args,
                          CXTranslationUnit &unit, CXIndex index,
                          CXUnsavedFile *unsaved, int unsavedCount,
                          unsigned int translationUnitFlags,
                          String *clangLine)

{
    if (clangLine)
        *clangLine = "clang ";

    int idx = 0;
    List<const char*> clangArgs(args.size() + 2, 0);

    const int count = args.size();
    for (int j=0; j<count; ++j) {
        String arg = args.at(j);
        clangArgs[idx++] = args.at(j).constData();
        if (clangLine) {
            arg.replace("\"", "\\\"");
            *clangLine += arg;
            *clangLine += ' ';
        }
    }
    // clangArgs[idx++] = "-disable-free";
    // clangArgs[idx++] = "-disable-llvm-verifier";

    if (clangLine)
        *clangLine += sourceFile;

    StopWatch sw;
    unit = clang_parseTranslationUnit(index, sourceFile.constData(),
                                      clangArgs.data(), idx, unsaved, unsavedCount,
                                      translationUnitFlags);
    // error() << sourceFile << sw.elapsed();
}

void reparseTranslationUnit(CXTranslationUnit &unit, CXUnsavedFile *unsaved, int unsavedCount)
{
    assert(unit);
    if (clang_reparseTranslationUnit(unit, 0, unsaved, clang_defaultReparseOptions(unit)) != 0) {
        clang_disposeTranslationUnit(unit);
        unit = 0;
    }
}

class StringOStream : public llvm::raw_ostream
{
public:
    StringOStream(String *str, bool compress)
        : llvm::raw_ostream(true), mString(str), mCompress(compress), mBufferUsed(0) // non-buffered
    {
        if (compress) {
            memset(&mStream, 0, sizeof(mStream));
            if (::deflateInit(&mStream, Z_BEST_COMPRESSION) != Z_OK)
                assert(0 && "Couldn't init compression");
        }
    }
    ~StringOStream()
    {
        if (mCompress) {
            ::deflateEnd(&mStream);
        }
    }
    void finish()
    {
        if (mCompress) {
            flush(mBuffer, mBufferUsed, Z_FINISH);
            mBufferUsed = 0;
        }
    }
    void flush(const char *data, size_t size, int flush)
    {
        mStream.next_in = const_cast<Bytef*>(reinterpret_cast<const Bytef*>(data));
        mStream.avail_in = size;
        do {
            char buffer[sizeof(mBuffer) * 2];
            mStream.next_out = reinterpret_cast<Bytef *>(buffer);
            mStream.avail_out = sizeof(buffer);

            const int error = ::deflate(&mStream, flush);
            if (error != Z_OK && error != Z_STREAM_END) {
                assert(0 && "compression error");
            }

            const int processed = sizeof(buffer) - mStream.avail_out;
            mString->append(buffer, processed);
        } while (!mStream.avail_out);
    }

    virtual void write_impl(const char *data, size_t size)
    {
        if (mCompress) {
            if (mBufferUsed + size < sizeof(mBuffer)) {
                memcpy(mBuffer + mBufferUsed, data, size);
                mBufferUsed += size;
            } else {
                if (mBufferUsed) {
                    flush(mBuffer, mBufferUsed, Z_NO_FLUSH);
                    mBufferUsed = 0;
                }
                if (size >= sizeof(mBuffer)) {
                    flush(data, size, Z_NO_FLUSH);
                } else {
                    memcpy(mBuffer, data, size);
                    mBufferUsed = size;
                }
            }
        } else {
            mString->append(data, size);
        }
    }
    virtual uint64_t current_pos() const
    {
        return mString->size();
    }
private:
    z_stream mStream;
    String *mString;
    const bool mCompress;
    char mBuffer[1024 * 64];
    size_t mBufferUsed;
};

static inline void processArgs(clang::HeaderSearchOptions &headerSearchOptions,
                               const clang::driver::ArgStringList &args)
{
    enum Type {
        Pending,
        SystemInclude
    } type = Pending;
    for (const char *arg : args) {
        switch (type) {
        case Pending:
            if (!strcmp(arg, "-internal-isystem") || !strcmp(arg, "-internal-externc-isystem")) {
                type = SystemInclude;
            } else if (!strncmp("-l", arg, 2)) {

            } else {
                error() << "Unknown arg type" << arg;
            }
            break;
        case SystemInclude: {
            const Path path = Path::resolved(arg);
            // error() << "Adding include" << path;
            headerSearchOptions.AddPath(clang::StringRef(path.constData(), path.size()), clang::frontend::System,
#if CLANG_VERSION_MINOR < 3
                                        false,
#endif
                                        false, false);
            type = Pending;
            break; }
        }
        // printf("got clang cxx arg %s\n", arg);
    }
}

static String toString(const Source::Define &def)
{
    String ret;
    ret += "#define ";
    ret += def.define;
    if (!def.value.isEmpty()) {
        ret += ' ';
        ret += def.value;
    }
    return ret;
}

bool compile(const Path& output, const Source &source, const String& preprocessed)
{
    (void)output;
    (void)source;
    (void)preprocessed;
#if CLANG_VERSION_MINOR < 4
    StopWatch sw;

    LLVMInitializeAllTargetInfos();
    LLVMInitializeAllTargets();
    LLVMInitializeAllTargetMCs();
    LLVMInitializeAllAsmPrinters();
    LLVMInitializeAllAsmParsers();

    clang::CompilerInstance compilerInstance;
    compilerInstance.createFileManager();
    assert(compilerInstance.hasFileManager());

#if CLANG_VERSION_MINOR >= 3
    compilerInstance.createDiagnostics();
#else
    compilerInstance.createDiagnostics(0, 0);
#endif
    assert(compilerInstance.hasDiagnostics());

    clang::FileManager &fm = compilerInstance.getFileManager();
    compilerInstance.createSourceManager(fm);
    assert(compilerInstance.hasSourceManager());
    clang::SourceManager &sm = compilerInstance.getSourceManager();

    const Path& sourceFile = source.sourceFile();
    clang::StringRef src(sourceFile.constData(), sourceFile.size());
    clang::StringRef pre(preprocessed.constData(), preprocessed.size());
    llvm::MemoryBuffer* premem = llvm::MemoryBuffer::getMemBuffer(pre, src);
    const clang::FileEntry* preent = fm.getVirtualFile(src, preprocessed.size(), time(0));
    sm.overrideFileContents(preent, premem);

    clang::TargetOptions &targetOptions = compilerInstance.getTargetOpts();
    String triple = llvm::sys::getDefaultTargetTriple();
    if (source.flags & Source::M32) {
        if (triple.startsWith("x86_64"))
            triple.replace(0, 6, "i386");
    } else if (source.flags & Source::M64 && triple.startsWith("i386")) {
        triple.replace(0, 4, "x86_64");
    }

    targetOptions.Triple = static_cast<std::string>(triple);
    clang::DiagnosticsEngine& diags = compilerInstance.getDiagnostics();
    compilerInstance.setTarget(clang::TargetInfo::CreateTargetInfo(diags,
#if CLANG_VERSION_MINOR < 3
                                                                   targetOptions
#else
                                                                   &targetOptions
#endif
                                   ));
    clang::LangOptions &langOpts = compilerInstance.getLangOpts();

    clang::InputKind ik;

    switch (source.language) {
    case Source::CPlusPlus11:
    case Source::CPlusPlus:
        ik = clang::IK_PreprocessedCXX;
        break;
    default:
        ik = clang::IK_PreprocessedC;
        break;
    }
    compilerInstance.getInvocation().setLangDefaults(langOpts, ik);
#if CLANG_VERSION_MINOR >= 3
    if (source.language == Source::CPlusPlus11)
        langOpts.CPlusPlus11 = true;
#endif

    clang::FrontendInputFile input(src, ik);

    clang::FrontendOptions& feopts = compilerInstance.getFrontendOpts();
    feopts.ProgramAction = clang::frontend::EmitObj;
    feopts.Inputs.push_back(input);

    compilerInstance.setTarget(clang::TargetInfo::CreateTargetInfo(compilerInstance.getDiagnostics(),
#if CLANG_VERSION_MINOR < 3
                                                                   compilerInstance.getTargetOpts()
#else
                                                                   &compilerInstance.getTargetOpts()
#endif
                                   ));
    compilerInstance.getTarget().setForcedLangOptions(langOpts);

    // ### ???
#if CLANG_VERSION_MINOR < 5
    compilerInstance.createPreprocessor();
#else
    compilerInstance.createPreprocessor(clang::TU_Complete);
#endif
    clang::InitializePreprocessor(compilerInstance.getPreprocessor(),
                                  compilerInstance.getPreprocessorOpts(),
                                  compilerInstance.getHeaderSearchOpts(),
                                  feopts);

    //compilerInstance.getDiagnosticClient().BeginSourceFile(compilerInstance.getLangOpts(), &compilerInstance.getPreprocessor());

    compilerInstance.clearOutputFiles(false);
    clang::StringRef out(output.constData(), output.size());
    //llvm::raw_fd_ostream* ostrm = compilerInstance.createDefaultOutputFile(true, out);
    llvm::raw_fd_ostream* ostrm = compilerInstance.createOutputFile(out, true, true, "", "o");
    ostrm->SetUnbuffered();

    clang::EmitObjAction emitact;
    emitact.BeginSourceFile(compilerInstance, input);
    emitact.Execute();
    emitact.EndSourceFile();

#if CLANG_VERSION_MINOR >= 3
    LLVMShutdown();
#endif

#endif
    return true;
}

static inline uint32_t visitFile(const Path &path,
                                 const std::shared_ptr<Cpp> &cpp,
                                 const std::shared_ptr<Project> &project,
                                 bool *blocked)
{
    assert(blocked);
    *blocked = false;
    if (!project)
        return Location::insertFile(path);

    const Map<Path, uint32_t>::const_iterator it = cpp->visited.find(path);
    if (it != cpp->visited.end())
        return it->second;

    const uint32_t fileId = Location::insertFile(path);
    if (!project->visitFile(fileId, path, 0)) {
        *blocked = true;
    } else {
        cpp->visited[path] = fileId;
        // ### We should find something nicer than this
        if (cpp->visited.size() % 10 == 0) {
            usleep(50000);
        }
    }
    return fileId;
}

class Compiler : public clang::CompilerInstance
{
public:
    ~Compiler()
    {
        resetAndLeakSourceManager();
    }
};

std::shared_ptr<Cpp> preprocess(const Source &source,
                                const std::shared_ptr<Project> &project,
                                unsigned int preprocessFlags)
{
    StopWatch sw;
    Compiler compilerInstance;
    compilerInstance.createFileManager();
    assert(compilerInstance.hasFileManager());
#if CLANG_VERSION_MINOR >= 3
    compilerInstance.createDiagnostics();
#else
    compilerInstance.createDiagnostics(0, 0);
#endif
    assert(compilerInstance.hasDiagnostics());
    clang::DiagnosticsEngine& diags = compilerInstance.getDiagnostics();
    clang::FileManager &fm = compilerInstance.getFileManager();
    clang::SourceManager sm(diags, fm, true);
    compilerInstance.setSourceManager(&sm);
    assert(compilerInstance.hasSourceManager());
    const Path sourceFile = source.sourceFile();
    uint64_t now = Rct::currentTimeMs();
    const clang::FileEntry *file = 0;
    for (int i=0; i<4; ++i) {
        file = fm.getFile(sourceFile.constData(), true); // pass openfile?
        if (file)
            break;
        usleep(100);
    }
    if (!file) {
        return std::shared_ptr<Cpp>();
    }
    sm.createMainFileID(file);
    clang::TargetOptions &targetOptions = compilerInstance.getTargetOpts();
    String triple = llvm::sys::getDefaultTargetTriple();
    if (source.flags & Source::M32) {
        if (triple.startsWith("x86_64"))
            triple.replace(0, 6, "i386");
    } else if (source.flags & Source::M64 && triple.startsWith("i386")) {
        triple.replace(0, 4, "x86_64");
    }

    //targetOptions.Triple = LLVM_HOST_TRIPLE;
    targetOptions.Triple = static_cast<std::string>(triple);
    clang::TextDiagnosticBuffer diagnosticsClient;
    diags.setClient(&diagnosticsClient, false);
    compilerInstance.setTarget(clang::TargetInfo::CreateTargetInfo(diags,
#if CLANG_VERSION_MINOR < 3
                                                                   targetOptions
#else
                                                                   &targetOptions
#endif
                                   ));
    clang::LangOptions &langOpts = compilerInstance.getLangOpts();
    switch (source.language) {
    case Source::CPlusPlus11:
#if CLANG_VERSION_MINOR >= 3
        langOpts.CPlusPlus11 = true;
#endif
        langOpts.CPlusPlus = true;
        break;
    case Source::CPlusPlus:
        langOpts.CPlusPlus = true;
        break;
    default:
        break;
    }

    if (source.flags & Source::NoRtti)
        langOpts.RTTI = false;

    const Server::Options &options = Server::instance()->options();
    clang::HeaderSearchOptions &headerSearchOptions = compilerInstance.getHeaderSearchOpts();
    Path sysRoot = source.sysRoot();
    if (sysRoot != "/") {
        assert(sysRoot.endsWith('/'));
        sysRoot.chop(1);
    }

    headerSearchOptions.Sysroot = sysRoot;
    {
        clang::driver::Driver driver("clang", static_cast<std::string>(triple), "a.out",
#if CLANG_VERSION_MINOR < 3
                                     true, // is_production, no idea what it means
#endif
                                     diags);
        std::vector<String> copies; // not cool
        std::vector<const char*> args;
        args.reserve(100);
        const Path compiler = source.compiler();
        args.push_back(compiler.constData());
        args.push_back("-c");
        args.push_back(sourceFile.constData());
        for (const Source::Include &inc : source.includePaths)
            copies.push_back(inc.toString());
        for (const Path &path : options.includePaths)
            copies.push_back("-I" + path);
        for (const Source::Define &def : source.defines)
            copies.push_back(def.toString());
        for (const Source::Define &def : options.defines)
            copies.push_back(def.toString());
        for (const String &str : copies)
            args.push_back(str.constData());

        std::unique_ptr<clang::driver::Compilation> compilation(driver.BuildCompilation(llvm::ArrayRef<const char*>(&args[0], args.size())));
        const clang::driver::ToolChain& toolChain = compilation->getDefaultToolChain();
        const InputArgList inputArgs(&args[0], &args[0] + args.size());
        clang::driver::ArgStringList outputArgs;
        toolChain.AddClangCXXStdlibIncludeArgs(inputArgs, outputArgs);
        processArgs(headerSearchOptions, outputArgs);
        toolChain.AddClangSystemIncludeArgs(inputArgs, outputArgs);
        processArgs(headerSearchOptions, outputArgs);
        toolChain.AddCXXStdlibLibArgs(inputArgs, outputArgs);
        processArgs(headerSearchOptions, outputArgs);
    }

    for (const auto &inc : source.includePaths) {
        // error() << "Adding -I" << *it;
        headerSearchOptions.AddPath(clang::StringRef(inc.path.constData(), inc.path.size()),
                                    inc.type == Source::Include::Type_Include ? clang::frontend::Angled : clang::frontend::System,
#if CLANG_VERSION_MINOR < 3
                                    false,
#endif
                                    false, true);
    }
    for (const auto &inc : options.includePaths) {
        // error() << "Adding -I" << *it;
        headerSearchOptions.AddPath(clang::StringRef(inc.constData(), inc.size()),
                                    clang::frontend::Angled,
#if CLANG_VERSION_MINOR < 3
                                    false,
#endif
                                    false, true);
    }

#if CLANG_VERSION_MINOR < 5
    compilerInstance.createPreprocessor();
#else
    compilerInstance.createPreprocessor(clang::TU_Complete);
#endif
    std::string predefines = compilerInstance.getPreprocessor().getPredefines();
    for (const auto &def : source.defines) {
        predefines += toString(def);
        predefines += '\n';
        // error() << "Got define" << it->define << it->value;
    }
    for (const auto &def : options.defines) {
        predefines += toString(def);
        predefines += '\n';
        // error() << "Got define" << it->define << it->value;
    }
    predefines += "#define __STRICT_ANSI__\n";

    // error() << "predefines" << compilerInstance.getPreprocessor().getPredefines();
    compilerInstance.getPreprocessor().setPredefines(predefines);
    clang::PreprocessorOutputOptions preprocessorOptions;
    preprocessorOptions.ShowCPP = 1;

    compilerInstance.getDiagnosticClient().BeginSourceFile(compilerInstance.getLangOpts(), &compilerInstance.getPreprocessor());

    std::shared_ptr<Cpp> cpp(new Cpp);
    StringOStream out(&cpp->preprocessed, preprocessFlags & Cpp::Preprocess_Compressed);
    cpp->time = now;
    cpp->flags = preprocessFlags;
    clang::Preprocessor &preprocessor = compilerInstance.getPreprocessor();
    preprocessor.createPreprocessingRecord(
#if CLANG_VERSION_MINOR < 3
        true
#endif
        );
    clang::DoPrintPreprocessedInput(preprocessor, &out, preprocessorOptions);
    out.finish();
    struct {
        const Cpp::Diagnostic::Type type;
        const clang::TextDiagnosticBuffer::const_iterator begin, end;
    } const diagnostics[] = {
        { Cpp::Diagnostic::Note, diagnosticsClient.note_begin(), diagnosticsClient.note_end() },
        { Cpp::Diagnostic::Warning, diagnosticsClient.warn_begin(), diagnosticsClient.warn_end() },
        { Cpp::Diagnostic::Error, diagnosticsClient.err_begin(), diagnosticsClient.err_end() }
    };
    for (size_t i=0; i<sizeof(diagnostics) / sizeof(diagnostics[0]); ++i) {
        for (auto it = diagnostics[i].begin; it != diagnostics[i].end; ++it) {
            const clang::PresumedLoc presumedLocation = sm.getPresumedLoc(it->first);
            bool ok;
            const Path path = Path::resolved(presumedLocation.getFilename(), Path::RealPath, Path(), &ok);
            if (!ok)
                continue;
            bool blocked;
            const uint32_t fileId = visitFile(path, cpp, project, &blocked);
            if (blocked)
                continue;

            cpp->diagnostics.append(Cpp::Diagnostic());
            Cpp::Diagnostic &d = cpp->diagnostics.last();
            d.location = Location(fileId, presumedLocation.getLine(), presumedLocation.getColumn());
            d.type = diagnostics[i].type;
            d.text = it->second;
        }
    }
    clang::PreprocessingRecord *record = preprocessor.getPreprocessingRecord();
    assert(record);

    // We need this extra map because macros might be defined, then undef'ed and
    // then redefined. In that case we need to know the "active" macro
    // definition so the set in cpp->macroNames doesn't help us.

    Hash<String, Location> macroLocations;
    for (clang::PreprocessingRecord::iterator it = record->begin(); it != record->end(); ++it) {
        const clang::PreprocessedEntity *entity = *it;
        switch (entity->getKind()) {
        case clang::PreprocessedEntity::MacroDefinitionKind: {
            const clang::MacroDefinition *def = static_cast<const clang::MacroDefinition*>(entity);
            const clang::SourceRange range = def->getSourceRange();
            if (!range.isValid())
                break;

            const clang::SourceLocation begin = range.getBegin();
            const clang::PresumedLoc presumedLocation = sm.getPresumedLoc(begin);
            const char *path = presumedLocation.getFilename();
            if (!strcmp(path, "<built-in>"))
                break;

            Path resolved = path;
            if (!resolved.resolve()) {
                // printf("Resolved didn't %s/%s\n", path, resolved.constData());
                break;
            }

            bool blocked;
            const uint32_t fileId = visitFile(resolved, cpp, project, &blocked);
            const clang::IdentifierInfo *name = def->getName();
            const String macroName(name->getNameStart(), name->getLength());
            const Location loc(fileId, presumedLocation.getLine(), presumedLocation.getColumn());
            if (!blocked) {
                std::shared_ptr<CursorInfo> &cursor = cpp->macroCursors[loc];
                if (!cursor)
                    cursor = std::make_shared<CursorInfo>();
                cursor->symbolName = macroName;
                cursor->symbolLength = cursor->symbolName.size();
                cursor->kind = CXCursor_MacroDefinition;
                cpp->macroNames[cursor->symbolName].insert(loc);
            }
            macroLocations[macroName] = loc;
            // error() << "Got definition" << String(name->getNameStart(), name->getLength()) << loc;
            break; }
        case clang::PreprocessedEntity::MacroExpansionKind: {
            const clang::MacroExpansion *exp = static_cast<const clang::MacroExpansion*>(entity);
            if (exp->isBuiltinMacro())
                break;

            const clang::SourceRange range = exp->getSourceRange();
            if (!range.isValid())
                break;

            const clang::PresumedLoc presumedLocation = sm.getPresumedLoc(range.getBegin());
            const char *path = presumedLocation.getFilename();
            if (!strcmp(path, "<built-in>"))
                break;

            Path resolved = path;
            if (!resolved.resolve()) {
                // printf("Resolved didn't %s/%s\n", path, resolved.constData());
                break;
            }

            bool blocked;
            const uint32_t fileId = visitFile(resolved, cpp, project, &blocked);
            if (blocked)
                break;

            const clang::IdentifierInfo *name = exp->getName();
            const String macroName(name->getNameStart(), name->getLength());
            const Location defLocation = macroLocations.value(macroName);
            if (defLocation.isNull()) {
                // error() << "Bailing on" << macroName << exp->getDefinition();
                break;
            }
            const Location loc(fileId, presumedLocation.getLine(), presumedLocation.getColumn());
            std::shared_ptr<CursorInfo> &cursor = cpp->macroCursors[loc];
            if (!cursor)
                cursor = std::make_shared<CursorInfo>();
            cursor->symbolName = macroName;
            cursor->symbolLength = cursor->symbolName.size();
            cursor->kind = CXCursor_MacroExpansion;
            std::shared_ptr<CursorInfo> &def = cpp->macroCursors[defLocation];
            if (!def)
                def = std::make_shared<CursorInfo>();
            // ### do I have to fill in def here? Do I need to in ClangIndexer?
            def->references.insert(loc);
            cursor->targets.insert(defLocation);
            // error() << "Got expansion" << String(name->getNameStart(), name->getLength()) << loc;
            break; }
        default:
            break;
        }
    }
    const char *dumpCpp = getenv("RTAGS_DUMP_CPP");
    if (dumpCpp && (!strcmp(dumpCpp, "1") || strstr(dumpCpp, sourceFile.fileName()))) {
        Path out = "/tmp/";
        out += sourceFile.fileName();
        FILE *f = fopen(out.constData(), "w");
        // fwrite(sourceFile.constData(), 1, sourceFile.size(), f);

        fwrite(cpp->preprocessed.constData(), 1, cpp->preprocessed.size(), f);
        fprintf(f, "// %s\n", sourceFile.constData());
        fclose(f);
    }
    cpp->preprocessDuration = sw.elapsed();
    warning() << "preprocessing" << sourceFile << "took" << cpp->preprocessDuration << "ms";

    return cpp;
}

static CXChildVisitResult findFirstChildVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    *reinterpret_cast<CXCursor*>(data) = cursor;
    return CXChildVisit_Break;
}

CXCursor findFirstChild(CXCursor parent)
{
    CXCursor ret = clang_getNullCursor();
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findFirstChildVisitor, &ret);
    return ret;
}

struct FindChildVisitor
{
    CXCursorKind kind;
    String name;
    CXCursor cursor;
};

static CXChildVisitResult findChildVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    FindChildVisitor *u = reinterpret_cast<FindChildVisitor*>(data);
    if (u->name.isEmpty()) {
        if (clang_getCursorKind(cursor) == u->kind) {
            u->cursor = cursor;
            return CXChildVisit_Break;
        }
    } else {
        CXStringScope str = clang_getCursorSpelling(cursor);
        if (str.data() && u->name == str.data()) {
            u->cursor = cursor;
            return CXChildVisit_Break;
        }
    }
    return CXChildVisit_Continue;
}

CXCursor findChild(CXCursor parent, CXCursorKind kind)
{
    FindChildVisitor u = { kind, String(), clang_getNullCursor() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChildVisitor, &u);
    return u.cursor;
}

CXCursor findChild(CXCursor parent, const String &name)
{
    FindChildVisitor u = { CXCursor_FirstInvalid, name, clang_getNullCursor() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChildVisitor, &u);
    return u.cursor;
}

struct ChildrenVisitor
{
    const Filter &in;
    const Filter &out;
    List<CXCursor> children;
};

static CXChildVisitResult childrenVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    ChildrenVisitor *u = reinterpret_cast<ChildrenVisitor*>(data);
    if ((u->out.isNull() || !u->out.match(cursor)) && (u->in.isNull() || u->in.match(cursor))) {
        u->children.append(cursor);
    }
    return CXChildVisit_Continue;
}

List<CXCursor> children(CXCursor parent, const Filter &in, const Filter &out)
{
    ChildrenVisitor userData = { in, out, List<CXCursor>() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, childrenVisitor, &userData);
    return userData.children;
}

struct FindChainVisitor
{
    const List<CXCursorKind> &kinds;
    List<CXCursor> ret;
};

static CXChildVisitResult findChainVisitor(CXCursor cursor, CXCursor, CXClientData data)
{
    FindChainVisitor *u = reinterpret_cast<FindChainVisitor*>(data);
    if (clang_getCursorKind(cursor) == u->kinds.at(u->ret.size())) {
        u->ret.append(cursor);
        if (u->ret.size() < u->kinds.size())
            return CXChildVisit_Recurse;

        return CXChildVisit_Break;
    }
    return CXChildVisit_Break;
}

List<CXCursor> findChain(CXCursor parent, const List<CXCursorKind> &kinds)
{
    assert(!kinds.isEmpty());
    FindChainVisitor userData = { kinds, List<CXCursor>() };
    if (!clang_isInvalid(clang_getCursorKind(parent)))
        clang_visitChildren(parent, findChainVisitor, &userData);
    if (userData.ret.size() != kinds.size()) {
        userData.ret.clear();
    }
    return userData.ret;
}

String typeName(const CXCursor &cursor)
{
    String ret;
    switch (clang_getCursorKind(cursor)) {
    case CXCursor_FunctionTemplate:
        // ### If the return value is a template type we get an empty string here
    case CXCursor_FunctionDecl:
    case CXCursor_CXXMethod:
        ret = typeString(clang_getResultType(clang_getCursorType(cursor)));
        break;
    case CXCursor_ClassTemplate:
    case CXCursor_ClassDecl:
    case CXCursor_StructDecl:
    case CXCursor_UnionDecl:
    case CXCursor_TypedefDecl:
    case CXCursor_EnumDecl:
        ret = RTags::eatString(clang_getCursorSpelling(cursor));
        break;
    case CXCursor_VarDecl: {
        const CXCursor initType = RTags::findFirstChild(cursor);
        if (clang_getCursorKind(initType) == CXCursor_InitListExpr) {
            ret = typeString(clang_getCursorType(initType));
        } else {
            ret = typeString(clang_getCursorType(cursor));
        }
        break; }
    case CXCursor_FieldDecl: // ### If the return value is a template type we get an empty string here
    case CXCursor_ParmDecl:
        ret = typeString(clang_getCursorType(cursor));
        break;
    default:
        return String();
    }
    if (!ret.isEmpty() && !ret.endsWith('*') && !ret.endsWith('&'))
        ret.append(' ');
    return ret;
}

String typeString(const CXType &type)
{
    String ret;
    if (clang_isConstQualifiedType(type))
        ret = "const ";

    const char *builtIn = builtinTypeName(type.kind);
    if (builtIn) {
        ret += builtIn;
        return ret;
    }

    if (char pointer = (type.kind == CXType_Pointer ? '*' : (type.kind == CXType_LValueReference ? '&' : 0))) {
        const CXType pointee = clang_getPointeeType(type);
        ret += typeString(pointee);
        if (ret.endsWith('*') || ret.endsWith('&')) {
            ret += pointer;
        } else {
            ret += ' ';
            ret += pointer;
        }
        return ret;
    }

    if (type.kind == CXType_ConstantArray) {
        ret += typeString(clang_getArrayElementType(type));
#if CLANG_VERSION_MAJOR > 3 || (CLANG_VERSION_MAJOR == 3 && CLANG_VERSION_MINOR >= 1)
        const int64_t count = clang_getNumElements(type);
        ret += '[';
        if (count >= 0)
            ret += String::number(count);
        ret += ']';
#endif
        return ret;
    }
    ret += typeName(clang_getTypeDeclaration(type));
    if (ret.endsWith(' '))
        ret.chop(1);
    return ret;
}

}
