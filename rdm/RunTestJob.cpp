#include "RunTestJob.h"
#include "StatusJob.h"
#include "Server.h"
#include "Rdm.h"

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
    char line[1024];
    enum State {
        None,
        SymbolNames,
        Symbols
    } state = None;
    if (f) {
        int read;
        while ((read = RTags::readLine(f, line, sizeof(line))) != -1) {
            switch (state) {
            case None:
                if (endsWith(line, read, "symbols.db")) {
                    state = Symbols;
                } else if (endsWith(line, read, "symbolnames.db")) {
                    state = SymbolNames;
                }
                break;
            case SymbolNames:
                if (!strcmp(StatusJob::delimiter, line)) {
                    state = None;
                    break;
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
QList<QByteArray> RunTestJob::runJob(Job *job)
{

}
