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
    const int count = parser.parse(mPath, flags, mEntries);
    error() << "Parsed" << (mPath + ":") << count << "items in" << timer.elapsed() << "ms";
    if (count)
        mFinished(this, mEntries);
}
