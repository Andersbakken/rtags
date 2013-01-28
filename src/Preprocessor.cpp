#include "Preprocessor.h"
#include "Connection.h"
#include "Process.h"
#include "Log.h"
#include "RTags.h"

Preprocessor::Preprocessor(const SourceInformation &args, Connection *connection)
    : mArgs(args), mConnection(connection), mProc(0)
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
        mProc->finished().connect(this, &Preprocessor::onProcessFinished);
    }
    List<ByteArray> args = mArgs.args;
    const int idx = args.indexOf("-fspell-checking");
    if (idx != -1)
        args.removeAt(idx);
    args.append("-E");
    args.append(mArgs.sourceFile);
    mProc->start(mArgs.compiler, args);
}

void Preprocessor::onProcessFinished()
{
    mConnection->write<256>("// %s %s", mArgs.compiler.constData(),
                            ByteArray::join(mArgs.args, ' ').constData());
    mConnection->write(mProc->readAllStdOut());
    const ByteArray err = mProc->readAllStdErr();
    if (!err.isEmpty()) {
        mConnection->write<1024>("/* %s */", err.constData());
    }
    mConnection->finish();
    deleteLater();
}
