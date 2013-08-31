#include "RTagsClang.h"
#include "Server.h"

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

    CXFile file;
    unsigned off, line, col; //presumedLine, presumedCol, instantiationLoc, expansionLoc;
    CXSourceLocation location = clang_getCursorLocation(cursor);
    clang_getSpellingLocation(location, &file, &line, &col, &off);
    // clang_getPresumedLocation(location, 0, &presumedLine, &presumedCol);
    // clang_getInstantiationLocation(location, 0, 0, 0, &instantiationLoc);
    // clang_getExpansionLocation(location, 0, 0, 0, &expansionLoc);

    const Str fileName(clang_getFileName(file));
    if (fileName.data() && *fileName.data()) {
        ret += ' ';
        ret += fileName.data();
        ret += ',';
        ret += String::number(off);

        if (flags & IncludeRange) {
            ret += " (";
            CXSourceRange range = clang_getCursorExtent(cursor);
            unsigned start, end;
            clang_getSpellingLocation(clang_getRangeStart(range), 0, 0, 0, &start);
            clang_getSpellingLocation(clang_getRangeEnd(range), 0, 0, 0, &end);
            ret += String::number(start);
            ret += '-';
            ret += String::number(end);
            ret += ')';
        }

        // if (presumedLine != line || presumedCol != col)
        //     ret += String::snprintf<32>("presumed: %d:%d", presumedLine, presumedCol);
        // if (instantiationLoc != off)
        //     ret += String::snprintf<32>("instantiation: %d", instantiationLoc);
        // if (expansionLoc != off)
        //     ret += String::snprintf<32>("expansion: %d", expansionLoc);

    }
    return ret;
}

static inline SymbolMap::const_iterator findCursorInfo(const SymbolMap &map, const Location &location, const String &context, bool scan)
{
    if (context.isEmpty() || !scan) {
        SymbolMap::const_iterator it = map.lower_bound(location);
        if (it != map.end() && it->first == location) {
            return it;
        } else if (it != map.begin()) {
            --it;
            if (it->first.fileId() == location.fileId()) {
                const int off = location.offset() - it->first.offset();
                if (it->second.symbolLength > off && (context.isEmpty() || it->second.symbolName.contains(context))) {
                    return it;
                }
            }
        }
        return map.end();
    }

    SymbolMap::const_iterator f = map.lower_bound(location);
    if (f != map.begin() && (f == map.end() || f->first != location))
        --f;
    SymbolMap::const_iterator b = f;

    enum { Search = 32 };
    for (int j=0; j<Search; ++j) {
        if (f != map.end()) {
            if (location.fileId() != f->first.fileId()) {
                if (b == map.begin())
                    break;
                f = map.end();
            } else if (f->second.symbolName.contains(context)) {
                // error() << "found it forward" << j;
                return f;
            } else {
                ++f;
            }
        }

        if (b != map.begin()) {
            --b;
            if (location.fileId() != b->first.fileId()) {
                if (f == map.end())
                    break;
                b = map.begin();
            } else if (b->second.symbolName.contains(context)) {
                // error() << "found it backward" << j;
                return b;
            }
        }
    }
    return map.end();
}

SymbolMap::const_iterator findCursorInfo(const SymbolMap &map, const Location &location, const String &context,
                                         const SymbolMap *errors, bool *foundInErrors)
{
    if (foundInErrors)
        *foundInErrors = false;
    if (map.isEmpty() && !errors)
        return map.end();
    if (errors) {
        SymbolMap::const_iterator ret = findCursorInfo(map, location, context, false);
        if (ret != map.end()) {
            return ret;
        }
        ret = findCursorInfo(*errors, location, context, false);
        if (ret != errors->end()) {
            if (foundInErrors)
                *foundInErrors = true;
            return ret;
        }
        // ret = findCursorInfo(*errors, location, context, true);
        // if (ret != errors->end()) {
        //     if (foundInErrors)
        //         *foundInErrors = true;
        //     return ret;
        // }
        // ret = findCursorInfo(map, location, context, true);
        // if (ret != map.end()) {
        //     return ret;
        // }

        return map.end();
    } else {
        const SymbolMap::const_iterator ret = findCursorInfo(map, location, context, true);
        return ret;
    }
}

void parseTranslationUnit(const Path &sourceFile, const List<String> &args,
                          CXTranslationUnit &unit, CXIndex index, String &clangLine,
                          uint32_t fileId, DependencyMap *dependencies,
                          CXUnsavedFile *unsaved, int unsavedCount)

{
    clangLine = "clang ";

    int idx = 0;
    const List<String> &defaultArguments = Server::instance()->options().defaultArguments;
    List<const char*> clangArgs(args.size() + defaultArguments.size(), 0);

    const List<String> *lists[] = { &args, &defaultArguments };
    for (int i=0; i<2; ++i) {
        const int count = lists[i]->size();
        for (int j=0; j<count; ++j) {
            String arg = lists[i]->at(j);
            if (arg.isEmpty())
                continue;
            if (dependencies && arg == "-include" && j + 1 < count) {
                const uint32_t fileId = Location::fileId(lists[i]->at(j + 1));
                if (fileId) {
                    (*dependencies)[fileId].insert(fileId);
                }
            }

            clangArgs[idx++] = lists[i]->at(j).constData();
            arg.replace("\"", "\\\"");
            clangLine += arg;
            clangLine += ' ';
        }
    }

    clangLine += sourceFile;

    StopWatch sw;
    unsigned int flags = CXTranslationUnit_DetailedPreprocessingRecord;
    if (Server::instance()->options().completionCacheSize)
        flags |= CXTranslationUnit_PrecompiledPreamble|CXTranslationUnit_CacheCompletionResults;

    unit = clang_parseTranslationUnit(index, sourceFile.constData(),
                                      clangArgs.data(), idx, unsaved, unsavedCount, flags);
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
}
