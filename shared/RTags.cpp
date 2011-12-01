#include "RTags.h"

namespace RTags {
bool parseLocation(const std::string &string,
                   std::string &file, unsigned &line, unsigned &col)
{
    assert(!string.empty());
    file = string;
    size_t colon = file.find_last_of(':');
    if (colon == string.size() - 1)
        colon = file.find_last_of(':', colon - 1);
    if (colon == std::string::npos)
        return false;
    col = atoi(string.c_str() + colon + 1);
    if (!col)
        return false;
    file.resize(colon);
    colon = file.find_last_of(':');
    if (colon == std::string::npos)
        return false;
    line = atoi(string.c_str() + colon + 1);
    if (!line)
        return false;
    file.resize(colon);
    return true;
}

Path findRtagsDb(const char *path)
{
    char buffer[1024];
    if (path) {
        strncpy(buffer, path, 1023);
        buffer[1023] = '\0';
    } else if (!getcwd(buffer, 1024)) {
        return Path();
    }

    char *slash;
    while ((slash = strrchr(buffer, '/'))) {
        // ### this is awful
        struct ::stat s;
        std::string path(buffer);
        path += "/.rtags.db";
        // printf("Testing [%s]\n", path.c_str());
        if (stat(path.c_str(), &s) >= 0)
            return QByteArray(path.c_str(), path.size());
        *slash = '\0';
    }
    return Path();
}

QByteArray kindToString(CXCursorKind kind)
{
    return eatString(clang_getCursorKindSpelling(static_cast<CXCursorKind>(kind)));
}

const char *completionChunkKindToString(int kind)
{
    switch (kind) {
    case CXCompletionChunk_Optional: return "Optional";
    case CXCompletionChunk_TypedText: return "TypedText";
    case CXCompletionChunk_Text: return "Text";
    case CXCompletionChunk_Placeholder: return "Placeholder";
    case CXCompletionChunk_Informative: return "Informative";
    case CXCompletionChunk_CurrentParameter: return "CurrentParameter";
    case CXCompletionChunk_LeftParen: return "LeftParen";
    case CXCompletionChunk_RightParen: return "RightParen";
    case CXCompletionChunk_LeftBracket: return "LeftBracket";
    case CXCompletionChunk_RightBracket: return "RightBracket";
    case CXCompletionChunk_LeftBrace: return "LeftBrace";
    case CXCompletionChunk_RightBrace: return "RightBrace";
    case CXCompletionChunk_LeftAngle: return "LeftAngle";
    case CXCompletionChunk_RightAngle: return "RightAngle";
    case CXCompletionChunk_Comma: return "Comma";
    case CXCompletionChunk_ResultType: return "ResultType";
    case CXCompletionChunk_Colon: return "Colon";
    case CXCompletionChunk_SemiColon: return "SemiColon";
    case CXCompletionChunk_Equal: return "Equal";
    case CXCompletionChunk_HorizontalSpace: return "HorizontalSpace";
    case CXCompletionChunk_VerticalSpace: return "VerticalSpace";
    }
    return "";
}


bool locationFromString(const QByteArray &string, Path *path, int *line, int *column)
{
    QRegExp locationRegExp = QRegExp("(.*):([0-9]+):([0-9]+)");
    if (!locationRegExp.exactMatch(QString::fromLocal8Bit(string)))
        return false;

    if (path)
        *path = Path::resolved(locationRegExp.cap(1).toLocal8Bit());
    if (line)
        *line = locationRegExp.cap(2).toInt();
    if (column)
        *column = locationRegExp.cap(3).toInt();
    return true;
}

QDebug operator<<(QDebug dbg, const std::string &str)
{
    dbg << str.c_str();
    return dbg;
}

QDebug operator<<(QDebug dbg, const leveldb::Slice &slice)
{
    dbg << QByteArray::fromRawData(slice.data(), slice.size());
    return dbg;
}

QDebug operator<<(QDebug dbg, CXCursor cursor)
{
    CXFile file;
    unsigned int line, col, off;
    CXSourceLocation loc = clang_getCursorLocation(cursor);
    clang_getInstantiationLocation(loc, &file, &line, &col, &off);
    CXString name = clang_getCursorDisplayName(cursor);
    CXString filename = clang_getFileName(file);
    CXString kind = clang_getCursorKindSpelling(clang_getCursorKind(cursor));
    dbg << clang_getCString(name) << clang_getCString(kind)
        << QString("%1:%2:%3").arg(clang_getCString(filename)).arg(line).arg(col);
    clang_disposeString(name);
    clang_disposeString(kind);
    clang_disposeString(filename);
    return dbg;
}

QList<QByteArray> systemIncludes()
{
    static QList<QByteArray> sSystemIncludes;
    if (sSystemIncludes.isEmpty()) {
        QProcess proc;
        proc.start(QLatin1String("cpp"), QStringList() << QLatin1String("-v"));
        proc.closeWriteChannel();
        proc.waitForFinished();
        QList<QByteArray> lines = proc.readAllStandardError().split('\n');
        bool seenInclude = false;
        QByteArray gxxIncludeDir, target;
        foreach(const QByteArray& line, lines) {
            if (gxxIncludeDir.isEmpty()) {
                int idx = line.indexOf("--with-gxx-include-dir=");
                if (idx != -1) {
                    const int space = line.indexOf(' ', idx);
                    gxxIncludeDir = line.mid(idx + 23, space - idx - 23);
                }
                idx = line.indexOf("--target=");
                if (idx != -1) {
                    const int space = line.indexOf(' ', idx);
                    target = line.mid(idx + 9, space - idx - 9);
                }
            } else if (!seenInclude && line.startsWith("#include ")) {
                seenInclude = true;
            } else if (seenInclude && line.startsWith(" /")) {
                Path path = Path::resolved(line.mid(1));
                if (path.isResolved()) {
                    sSystemIncludes.append("-I" + path);
                }
            }
        }
        if (!gxxIncludeDir.isEmpty()) {
            sSystemIncludes.append("-I" + gxxIncludeDir);
            if (!target.isEmpty()) {
                sSystemIncludes.append("-I" + gxxIncludeDir + "/" + target);
            }
        }
    }
    return sSystemIncludes;
}
}
