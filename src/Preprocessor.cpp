#include "Preprocessor.h"
#include "Connection.h"
#include "Process.h"
#include "Log.h"
#include "RTags.h"

Preprocessor::Preprocessor(const CompileArgs &args, Connection *connection)
    : mArgs(args), mConnection(connection), mProc(0), mWrittenArguments(false)
{
}

Preprocessor::~Preprocessor()
{
    delete mProc;
}

void Preprocessor::preprocess()
{
    if (!mProc) {
        mProc = new Process;
        mProc->readyReadStdOut().connect(this, &Preprocessor::onProcessReadyRead);
        mProc->readyReadStdErr().connect(this, &Preprocessor::onProcessReadyRead);
        mProc->finished().connect(this, &Preprocessor::onProcessFinished);
    }
    mArgs.args.append("-E");
    mArgs.args.append(mArgs.sourceFile);
    mProc->start(mArgs.compiler, mArgs.args);
}

void Preprocessor::onProcessReadyRead()
{
    if (!mWrittenArguments) {
        mConnection->write<256>("// %s %s", mArgs.compiler.constData(),
                                ByteArray::join(mArgs.args, ' ').constData());
        mWrittenArguments = true;
    }
    mConnection->write(mProc->readAllStdOut());
    const ByteArray err = mProc->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
}

void Preprocessor::onProcessFinished()
{
    mConnection->finish();
    deleteLater();
}
