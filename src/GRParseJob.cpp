#include "GRParseJob.h"
#include "GRParser.h"

GRParseJob::GRParseJob(const Path &path, unsigned flags)
    : mPath(path), mFlags(flags), mParseTime(0)
{
}

void GRParseJob::run()
{
    Timer timer;
    GRParser parser;
    const char *extension = mPath.extension();
    const unsigned flags = extension && strcmp("c", extension) ? GRParser::CPlusPlus : GRParser::None;
    mParseTime = time(0);
    parser.parse(mPath, flags, mEntries);
    mFinished(this, mEntries);
}
