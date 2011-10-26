#include "RTags.h"
#include "CursorKey.h"

namespace RTags {
QDataStream &operator<<(QDataStream &ds, time_t t)
{
    return (ds << quint64(t));
}

QDataStream &operator>>(QDataStream &ds, time_t &t)
{
    qint64 tmp;
    ds >> tmp;
    t = tmp;
    return ds;
}

bool parseLocation(const std::string &string,
                   std::string &file, unsigned &line, unsigned &col)
{
    file = string;
    size_t colon = file.find_last_of(':');
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

Path findRtagsDb()
{
    char buffer[500];
    if (getcwd(buffer, 500)) {
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

QDebug operator<<(QDebug dbg, CXCursor cursor)
{
    dbg << CursorKey(cursor);
    return dbg;
}
bool cursorDefinitionFor(const CursorKey &d, const CursorKey &c)
{
    switch (c.kind) {
    case CXCursor_CallExpr:
        return false;
    default:
        break;
    }
    return d.isDefinition();
}

}
