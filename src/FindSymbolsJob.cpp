#include "FindSymbolsJob.h"
#include "Server.h"
#include "Log.h"
#include "RTags.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::QuoteOutput : Job::None;
}

FindSymbolsJob::FindSymbolsJob(const QueryMessage &query, const shared_ptr<Project> &proj)
    : Job(query, ::jobFlags(query.flags()), proj), string(query.query())
{
}

struct Node
{
    Node(const Location &loc = Location())
        : location(loc), isDefinition(false), kind(CXCursor_FirstInvalid)
    {}

    Location location;
    bool isDefinition;
    CXCursorKind kind;

    int rank() const
    {
        int val = 0;
        switch (kind) {
        case CXCursor_VarDecl:
        case CXCursor_FieldDecl:
            val = 2;
            break;
        case CXCursor_FunctionDecl:
        case CXCursor_CXXMethod:
        case CXCursor_Constructor:
            val = 5;
            break;
        case CXCursor_ClassDecl:
        case CXCursor_StructDecl:
            val = 10;
            break;
        default:
            val = 1;
            break;
        }
        if (isDefinition)
            val += 100;
        return val;
    }

    bool operator<(const Node &other) const
    {
        const int me = rank();
        const int him = other.rank();
        // error() << "comparing<" << location << "and" << other.location
        //         << me << him << isDefinition << other.isDefinition
        //         << RTags::eatString(clang_getCursorKindSpelling(kind))
        //         << RTags::eatString(clang_getCursorKindSpelling(other.kind));
        if (me != him)
            return me > him;
        return location < other.location;
    }
    bool operator>(const Node &other) const
    {
        const int me = rank();
        const int him = other.rank();
        // error() << "comparing>" << location << "and" << other.location
        //         << me << him << isDefinition << other.isDefinition
        //         << RTags::eatString(clang_getCursorKindSpelling(kind))
        //         << RTags::eatString(clang_getCursorKindSpelling(other.kind));
        if (me != him)
            return me > him;
        return location > other.location;
    }
};

void FindSymbolsJob::run()
{
    Map<Location, bool> out;
    if (project()->indexer) {
        Scope<const SymbolNameMap&> scope = project()->lockSymbolNamesForRead(lockTimeout());
        if (scope.isNull())
            return;
        const SymbolNameMap &map = scope.data();
        const SymbolNameMap::const_iterator it = map.find(string);
        if (it != map.end()) {
            const Set<Location> &locations = it->second;
            for (Set<Location>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                out[*i] = true;
            }
        }
    }
    if (!(queryFlags() & QueryMessage::DisableGRTags) && project()->grtags) {
        Scope<const GRMap &> scope = project()->lockGRForRead();
        const GRMap &map = scope.data();
        GRMap::const_iterator it = map.find(string);
        if (it != map.end()) {
            const Map<Location, bool> &locations = it->second;
            for (Map<Location, bool>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                if (!i->second)
                    out[i->first] = false;
            }
        }
    }

    if (out.size()) {
        Scope<const SymbolMap&> scope = project()->lockSymbolsForRead(lockTimeout());
        const SymbolMap *map = &scope.data();
        List<Node> sorted;
        sorted.reserve(out.size());
        for (Map<Location, bool>::const_iterator it = out.begin(); it != out.end(); ++it) {
            Node node(it->first);
            if (it->second && map) {
                const SymbolMap::const_iterator found = map->find(it->first);
                if (found != map->end()) {
                    node.isDefinition = found->second.isDefinition;
                    node.kind = found->second.kind;
                }
            }
            sorted.push_back(node);
        }

        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<Node>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        const uint32_t keyFlags = QueryMessage::keyFlags(queryFlags());
        const int count = sorted.size();
        for (int i=0; i<count; ++i) {
            write(sorted.at(i).location.key(keyFlags));
        }
    }
}
