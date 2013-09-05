#include "FindSymbolsJob.h"
#include "Server.h"
#include <rct/Log.h>
#include "RTagsClang.h"
#include "Project.h"

static inline unsigned jobFlags(unsigned queryFlags)
{
    return (queryFlags & QueryMessage::ElispList) ? Job::QuoteOutput|Job::QuietJob : Job::None|Job::QuietJob;
}

FindSymbolsJob::FindSymbolsJob(const QueryMessage &query, const std::shared_ptr<Project> &proj)
    : Job(query, ::jobFlags(query.flags()), proj), string(query.query())
{
}

void FindSymbolsJob::execute()
{
    Map<Location, bool> out;
    std::shared_ptr<Project> proj = project();
    if (proj) {
        const SymbolNameMap &map = proj->symbolNames();
        SymbolNameMap::const_iterator it = map.lower_bound(string);
        while (it != map.end() && it->first.startsWith(string)) {
            bool ok = false;
            if (it->first.size() == string.size()) {
                ok = true;
            } else if ((it->first.at(string.size()) == '<' || it->first.at(string.size()) == '(')
                       && it->first.indexOf(")::", string.size()) == -1) { // we don't want to match foobar for void foobar(int)::parm
                ok = true;
            }
            if (ok) {
                const Set<Location> &locations = it->second;
                for (Set<Location>::const_iterator i = locations.begin(); i != locations.end(); ++i) {
                    out[*i] = true;
                }
            }
            ++it;
        }
    }

    if (out.size()) {
        const SymbolMap &map = proj->symbols();
        List<RTags::SortedCursor> sorted;
        sorted.reserve(out.size());
        const bool declarationOnly = queryFlags() & QueryMessage::DeclarationOnly;
        for (Map<Location, bool>::const_iterator it = out.begin(); it != out.end(); ++it) {
            RTags::SortedCursor node(it->first);
            if (it->second) {
                const SymbolMap::const_iterator found = map.find(it->first);
                if (found != map.end()) {
                    node.isDefinition = found->second.isDefinition();
                    if (declarationOnly && node.isDefinition) {
                        CursorInfo decl = found->second.bestTarget(map);
                        if (!decl.isNull())
                            continue;
                    }
                    node.kind = found->second.kind;
                }
            }
            sorted.push_back(node);
        }

        if (queryFlags() & QueryMessage::ReverseSort) {
            std::sort(sorted.begin(), sorted.end(), std::greater<RTags::SortedCursor>());
        } else {
            std::sort(sorted.begin(), sorted.end());
        }
        const int count = sorted.size();
        for (int i=0; i<count; ++i) {
            write(sorted.at(i).location);
        }
    }
}
