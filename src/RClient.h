/* This file is part of RTags (http://rtags.net).

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

#ifndef RClient_h
#define RClient_h

#include "QueryMessage.h"
#include "rct/List.h"
#include "rct/Message.h"
#include "rct/Path.h"
#include "rct/Set.h"
#include "rct/String.h"
#include "CommandLineParser.h"
#include "rct/rct-config.h"

class RCCommand;
class QueryCommand;
class Connection;
class RClient
{
public:
    enum OptionType {
        None = 0,
        AbsolutePath,
        AllDependencies,
        AllReferences,
        AllTargets,
        Autotest,
        BuildIndex,
        CheckIncludes,
        CheckReindex,
        ClassHierarchy,
        Clear,
        CodeCompleteAt,
        CodeCompleteIncludeMacros,
        CodeCompleteIncludes,
        CodeCompleteNoWait,
        CodeCompletePrefix,
        CodeCompletionEnabled,
        CompilationFlagsOnly,
        CompilationFlagsSplitLine,
        Compile,
        ConnectTimeout,
        ContainingFunction,
        ContainingFunctionLocation,
        CurrentFile,
        CurrentProject,
        CursorKind,
        DebugLocations,
        DeclarationOnly,
        DefinitionOnly,
        DeleteProject,
        Dependencies,
        DependencyFilter,
        Diagnose,
        DiagnoseAll,
        Diagnostics,
        DisplayName,
        DumpCompileCommands,
        DumpCompletions,
        DumpFile,
        DumpFileMaps,
        DumpIncludeHeaders,
        Elisp,
        FilterSystemHeaders,
        FindFile,
        FindFilePreferExact,
        FindProjectBuildRoot,
        FindProjectRoot,
        FindSymbols,
        FindVirtuals,
        FixIts,
        FollowLocation,
        GenerateTest,
        GuessFlags,
        HasFileManager,
        Help,
        IncludeFile,
        IsIndexed,
        IsIndexing,
        JSON,
        JobCount,
        KindFilter,
        ListBuffers,
        ListCursorKinds,
        ListSymbols,
        LoadCompileCommands,
        LogFile,
        Man,
        MatchCaseInsensitive,
        MatchRegex,
        Max,
        NoColor,
        NoContext,
        NoRealPath,
        NoSortReferencesByInput,
        NoSpellCheckinging,
        PathFilter,
        PreprocessFile,
        Project,
        ProjectRoot,
        QuitRdm,
        RTagsConfig,
        RangeFilter,
        RdmLog,
        ReferenceLocation,
        ReferenceName,
        Reindex,
        ReloadFileManager,
        RemoveFile,
        Rename,
        ReverseSort,
        SendDiagnostics,
        SetBuffers,
        Silent,
        SilentQuery,
        SocketAddress,
        SocketFile,
        Sources,
        Status,
        StripParen,
        Suspend,
        SymbolInfo,
        SymbolInfoIncludeBaseClasses,
        SymbolInfoIncludeParents,
        SymbolInfoIncludeReferences,
        SymbolInfoIncludeTargets,
        SynchronousCompletions,
        SynchronousDiagnostics,
        TargetUsrs,
        Timeout,
        Tokens,
        TokensIncludeSymbols,
        UnsavedFile,
        Verbose,
        Version,
#ifdef RTAGS_HAS_LUA
        VisitAST,
        VisitASTScript,
#endif
        Wait,
        WildcardSymbolNames,
        XML,
        Noop,
        NumOptions
    };

    enum Flag {
        Flag_None = 0x0,
        Flag_Autotest = 0x1
    };

    RClient();
    ~RClient();
    int exec();
    CommandLineParser::ParseStatus parse(size_t argc, char **argv);

    Flags<Flag> flags() const { return mFlags; }

    int max() const { return mMax; }
    LogLevel logLevel() const { return mLogLevel; }
    int timeout() const { return mTimeout; }
    int buildIndex() const { return mBuildIndex; }

    const Set<QueryMessage::PathFilter> &pathFilters() const { return mPathFilters; }
    int minOffset() const { return mMinOffset; }
    int maxOffset() const { return mMaxOffset; }

    const QueryMessage::KindFilters &kindFilters() const { return mKindFilters; }

    const UnsavedFiles &unsavedFiles() const { return mUnsavedFiles; }

    const List<String> &rdmArgs() const { return mRdmArgs; }
    const Path &currentFile() const { return mCurrentFile; }

    String socketFile() const { return mSocketFile; }
    String tcpHost() const { return mTcpHost; }
    uint16_t tcpPort() const { return mTcpPort; }
    Path projectRoot() const { return mProjectRoot; }
    Flags<QueryMessage::Flag> queryFlags() const { return mQueryFlags; }
    int terminalWidth() const { return mTerminalWidth; }

    String commandLine() const { return mCommandLine; }
    void onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &);
    List<String> environment() const;
    String codeCompletePrefix() const { return mCodeCompletePrefix; }
#ifdef RTAGS_HAS_LUA
    List<String> visitASTScripts() const { return mVisitASTScripts; }
#endif
private:
    void addQuery(QueryMessage::Type t, String &&query = String(),
                  Flags<QueryMessage::Flag> extraQueryFlags = Flags<QueryMessage::Flag>());
    void addQuitCommand(int exitCode);

    void addLog(LogLevel level);
    void addCompile(String &&args, const Path &cwd);
    void addCompile(Path &&compileCommands);

    Flags<Flag> mFlags;
    Flags<QueryMessage::Flag> mQueryFlags;
    int mMax, mTimeout, mMinOffset, mMaxOffset, mConnectTimeout, mBuildIndex;
    LogLevel mLogLevel;
    Set<QueryMessage::PathFilter> mPathFilters;
    QueryMessage::KindFilters mKindFilters;
    UnsavedFiles mUnsavedFiles;
    List<std::shared_ptr<RCCommand> > mCommands;
    List<String> mRdmArgs;
    Path mSocketFile;
    Path mCurrentFile;
    String mTcpHost;
    String mCodeCompletePrefix;
    uint16_t mTcpPort;
    bool mGuessFlags;
    Path mProjectRoot;
    int mTerminalWidth;
#ifdef RTAGS_HAS_LUA
    List<String> mVisitASTScripts;
#endif
    mutable List<String> mEnvironment;

    String mCommandLine;
    friend class CompileCommand;
};

RCT_FLAGS(RClient::Flag);

#endif
