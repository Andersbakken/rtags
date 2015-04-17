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

#include <rct/List.h>
#include <rct/String.h>
#include <rct/Hash.h>
#include <rct/Path.h>
#include "QueryMessage.h"

class RCCommand;
class QueryCommand;
class Connection;
class RClient
{
public:
    enum OptionType {
        None = 0,
        AbsolutePath,
        AllReferences,
        AllTargets,
        BuildIndex,
        CheckReindex,
        ClassHierarchy,
        Clear,
        CodeCompleteAt,
        CompilationFlagsOnly,
        CompilationFlagsSplitLine,
        Compile,
        ConnectTimeout,
        ContainingFunction,
        CurrentFile,
        CursorKind,
        DeclarationOnly,
        DeleteProject,
        Dependencies,
        Diagnostics,
        DisplayName,
        DumpCompilationDatabase,
        DumpCompletions,
        DumpFile,
        DumpIncludeHeaders,
        ElispList,
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
        IMenu,
        IsIndexed,
        IsIndexing,
        JobCount,
        ListBuffers,
        ListSymbols,
        LoadCompilationDatabase,
        LogFile,
        Man,
        MatchCaseInsensitive,
        MatchRegex,
        Max,
        NoColor,
        NoContext,
        NoSortReferencesByInput,
        NoUnescapeCompileCommands,
        PathFilter,
        PrepareCodeCompleteAt,
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
        ReloadProjects,
        RemoveFile,
        Rename,
        ReverseSort,
        SendDiagnostics,
        SetBuffers,
        Silent,
        SilentQuery,
        SocketFile,
        Sources,
        Status,
        StripParen,
        Suspend,
        SymbolInfo,
        SymbolInfoIncludeParents,
        SymbolInfoExcludeReferences,
        SymbolInfoExcludeTargets,
        SynchronousCompletions,
        Timeout,
        UnescapeCompileCommands,
        UnsavedFile,
        Verbose,
        Version,
        WildcardSymbolNames,
        XmlDiagnostics,
        NumOptions
    };

    RClient();
    ~RClient();
    int exec();
    enum ParseStatus {
        Parse_Exec,
        Parse_Ok,
        Parse_Error
    };
    ParseStatus parse(int &argc, char **argv);

    int max() const { return mMax; }
    int logLevel() const { return mLogLevel; }
    int timeout() const { return mTimeout; }
    int buildIndex() const { return mBuildIndex; }

    const Set<String> &pathFilters() const { return mPathFilters; }
    int minOffset() const { return mMinOffset; }
    int maxOffset() const { return mMaxOffset; }

    const UnsavedFiles &unsavedFiles() const { return mUnsavedFiles; }

    const List<String> &rdmArgs() const { return mRdmArgs; }
    const Path &currentFile() const { return mCurrentFile; }

    String socketFile() const { return mSocketFile; }
    Path projectRoot() const { return mProjectRoot; }
    Flags<QueryMessage::Flag> queryFlags() const { return mQueryFlags; }

    int argc() const { return mArgc; }
    char **argv() const { return mArgv; }
    void onNewMessage(const std::shared_ptr<Message> &message, const std::shared_ptr<Connection> &);
private:
    void addQuery(QueryMessage::Type t, const String &query = String(),
                  Flags<QueryMessage::Flag> extraQueryFlags = Flags<QueryMessage::Flag>());
    void addQuitCommand(int exitCode);

    void addLog(int level);
    enum EscapeMode {
        Escape_Auto,
        Escape_Do,
        Escape_Dont
    };

    void addCompile(const Path &cwd, const String &args, EscapeMode escapeMode);
    void addCompile(const Path &dir, EscapeMode escapeMode);

    Flags<QueryMessage::Flag> mQueryFlags;
    int mMax, mLogLevel, mTimeout, mMinOffset, mMaxOffset, mConnectTimeout, mBuildIndex;
    Set<String> mPathFilters;
    UnsavedFiles mUnsavedFiles;
    List<std::shared_ptr<RCCommand> > mCommands;
    List<String> mRdmArgs;
    String mSocketFile;
    Path mCurrentFile;
    EscapeMode mEscapeMode;
    bool mGuessFlags;
    Path mProjectRoot;

    int mArgc;
    char **mArgv;

    friend class CompileCommand;
};

#endif

