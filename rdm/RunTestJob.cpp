#include "RunTestJob.h"
#include "StatusJob.h"
#include "Server.h"
#include "Rdm.h"
#include "FindSymbolsJob.h"

RunTestJob::RunTestJob(const Path &p, int i)
    : Job(i, QueryJobPriority, WriteUnfiltered), path(p)
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
    QByteArray symbolName;
    QSet<QByteArray> expectedLocations;
    if (f) {
        int read;
        while ((read = RTags::readLine(f, buf, sizeof(buf))) != -1) {
            const QByteArray line = QByteArray::fromRawData(buf, read);
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
                    state = None;
                } else {
                    write("Unexpected line [" + line + "] while parsing symbol name tests");
                    return;
                }
                break;
            case Symbols:
                if (!strcmp(StatusJob::delimiter, line)) {
                    state = None;
                    break;
                }
                break;
            }
        }

        fclose(f);
    }
}

void RunTestJob::testSymbolNames(const QByteArray &symbolName, const QSet<QByteArray> &expectedLocations)
{
    const QueryMessage msg(QueryMessage::FindSymbols, symbolName, QueryMessage::NoContext);
    JobRunner runner;
    const QSet<QByteArray> actual = runner.runJob(new FindSymbolsJob(-1, msg));
    QSet<QByteArray> missing = expectedLocations - actual;
    QSet<QByteArray> unexpected = actual - expectedLocations;
    if (!missing.isEmpty() || !unexpected.isEmpty()) {
        write("symbolnames: [" + symbolName + "]");
        foreach(const QByteArray &m, missing)
            write("---  " + m);
        foreach(const QByteArray &u, unexpected)
            write("+++ " + u);
    } else {
        write("symbolnames: [" + symbolName + "] passed (" + QByteArray::number(expectedLocations.size()) + " locations)");
    }
    // qDebug() << "testSymbolNames" << symbolName << "got" << expectedLocations << "wanted" << actual;
}
