#include "RunTestJob.h"
#include "StatusJob.h"
#include "Server.h"
#include "RTags.h"
#include "FindSymbolsJob.h"
#include "ListSymbolsJob.h"
#include "CursorInfoJob.h"
#include "FollowLocationJob.h"
#include "ReferencesJob.h"
#include "QueryMessage.h"

RunTestJob::RunTestJob(const Path &p, const QueryMessage &query)
    : Job(query, WriteUnfiltered), path(p)
{
}

static bool inline endsWith(const char *haystack, int haystackLength, const char *needle)
{
    const int len = strlen(needle);
    if (haystackLength < len)
        return false;
    return !strncmp(haystack + haystackLength - len, needle, len);
}

void RunTestJob::execute()
{
    FILE *f = fopen(path.constData(), "r");
    char buf[1024];
    enum State {
        None,
        SymbolNames,
        Symbols
    } state = None;
    // symbolNames
    ByteArray symbolName;
    Set<ByteArray> expectedLocations;
    Set<ByteArray> allSymbolNames;
    {
        const QueryMessage msg(QueryMessage::ListSymbols);
        expectedLocations = runJob(new ListSymbolsJob(msg));
    }

    // symbols
    ByteArray location, targetLocation;
    List<ByteArray> references;
    if (f) {
        int read;
        while ((read = RTags::readLine(f, buf, sizeof(buf))) != -1) {
            const ByteArray line(buf, read);
            switch (state) {
            case None:
                if (line.endsWith("symbols.db")) {
                    state = Symbols;
                } else if (line.endsWith("symbolnames.db")) {
                    state = SymbolNames;
                }
                break;
            case SymbolNames:
                if (line.endsWith(":") && line.startsWith("  ")) {
                    if (!expectedLocations.isEmpty()) {
                        testSymbolNames(symbolName, expectedLocations);
                        symbolName.clear();
                        expectedLocations.clear();
                    } else if (!symbolName.isEmpty()) {
                        write("Unexpected line [" + line + "]. Got new symbolName without any locations for the previous one " + symbolName);
                        return;
                    }
                    symbolName = line.mid(2, line.size() - 3);
                    allSymbolNames.remove(symbolName);
                } else if (line.startsWith("    /")) {
                    if (symbolName.isEmpty()) {
                        write("Unexpected line [" + line + "]. Got new location without symbolName");
                        return;
                    }
                    expectedLocations.insert(line.mid(4));
                } else if (line == StatusJob::delimiter) {
                    if (!expectedLocations.isEmpty()) {
                        testSymbolNames(symbolName, expectedLocations);
                        symbolName.clear();
                        expectedLocations.clear();
                    }
                    if (!allSymbolNames.isEmpty())
                        write("Missing symbolNames: " + ByteArray::join(allSymbolNames.toList(), ", "));
                    state = None;
                } else {
                    write("Unexpected line [" + line + "] while parsing symbol name tests");
                    return;
                }
                break;
            case Symbols:
                if (line.startsWith("  /")) {
                    const int idx = line.indexOf(" symbolName: ");
                    if (idx == -1) {
                        write("Can't parse line [" + line + "] during symbol tests");
                        return;
                    }
                    const Location loc = Location::fromPathAndOffset(ByteArray(line.constData() + 2, idx - 2));
                    if (!loc.isValid()) {
                        write("Can't parse line [" + line + "] during symbol tests");
                        return;
                    }
                    const ByteArray cursorInfo = runJob(new CursorInfoJob(loc, QueryMessage())).toList().value(0);
                    if (strncmp(cursorInfo.constData(), line.constData() + 2, cursorInfo.size())) {
                        write("Failed test, something's different here");
                    }
                } else if (line.startsWith("    /")) {


                } else if (line == StatusJob::delimiter) {


                } else {
                    write("Unexpected line [" + line + "] while parsing symbol name tests");
                    return;
                }
                break;
            }
        }

        fclose(f);
    }
}

void RunTestJob::testSymbolNames(const ByteArray &symbolName, const Set<ByteArray> &expectedLocations)
{
    const QueryMessage msg(QueryMessage::FindSymbols, symbolName, QueryMessage::NoContext);
    const Set<ByteArray> actual = runJob(new FindSymbolsJob(msg));
    Set<ByteArray> missing = expectedLocations - actual;
    Set<ByteArray> unexpected = actual - expectedLocations;
    if (!missing.isEmpty() || !unexpected.isEmpty()) {
        write("symbolnames: [" + symbolName + "] failed ("
              + ByteArray::number(missing.size() + unexpected.size()) + " failures)");
        for (Set<ByteArray>::const_iterator it = missing.begin(); it != missing.end(); ++it)
            write("--- " + *it);
        for (Set<ByteArray>::const_iterator it = unexpected.begin(); it != unexpected.end(); ++it)
            write("+++ " + *it);
    } else {
        write("symbolnames: [" + symbolName + "] passed (" + ByteArray::number(expectedLocations.size()) + " locations)");
    }
    // error() << "testSymbolNames" << symbolName << "got" << expectedLocations << "wanted" << actual;
}
