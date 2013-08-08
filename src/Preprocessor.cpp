#include "Preprocessor.h"
#include <rct/Connection.h>
#include <rct/Process.h>
#include <rct/Log.h>
#include "RTags.h"

Preprocessor::Preprocessor(const SourceInformation &args, Connection *connection)
    : mArgs(args), mConnection(connection), mProc(0)
{
    mProc = new Process;
    mProc->finished().connect(std::bind(&Preprocessor::onProcessFinished, this));
}

Preprocessor::~Preprocessor()
{
    delete mProc;
}

void Preprocessor::preprocess()
{
    List<String> args = mArgs.args;
    args.append("-E");
    args.append(mArgs.sourceFile);

    List<String> environ;
    environ << "PATH=/usr/local/bin:/usr/bin";
    mProc->start(mArgs.compiler, args, environ);
}

void Preprocessor::onProcessFinished()
{
    mConnection->write<256>("// %s %s", mArgs.compiler.constData(),
                            String::join(mArgs.args, ' ').constData());
    mConnection->write(mProc->readAllStdOut());
    const String err = mProc->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    EventLoop::deleteLater(this);
}
