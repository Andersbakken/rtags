#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include "ResponseMessage.h"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QDebug>
#include <Log.h>
#include <unistd.h>

Client::Client(const QByteArray &name, unsigned flags, const QList<QByteArray> &extraFlags,
               const QList<QByteArray> &rdmArgs, QObject* parent)
    : QObject(parent), mConn(0), mFlags(flags), mMakeDone(false), mExtraFlags(extraFlags),
      mSourceFileCount(0), mPchCount(0), mRdmArgs(rdmArgs),
      mName(name)
{
    if ((mFlags & (RestartRdm|AutostartRdm)) == (RestartRdm|AutostartRdm)) {
        mFlags &= ~AutostartRdm; // this is implied and would upset connectToServer
    }
    Messages::init();
    const bool ret = connectToServer();
    if (mFlags & RestartRdm) { // ### something about this is buggy
        if (ret) {
            QueryMessage msg(QueryMessage::Shutdown);
            query(&msg);
            delete mConn;
            mConn = 0;
        }
        mFlags |= AutostartRdm;
        connectToServer();
        mFlags &= ~AutostartRdm;
    }
}

void Client::query(const QueryMessage *message)
{
    if (!mConn && !connectToServer() && !(mFlags & (RestartRdm|AutostartRdm))) {
        return;
    }

    connect(mConn, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    connect(mConn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
    mConn->send(message);
    mLoop.exec();
}

void Client::onSendComplete()
{
    Q_ASSERT(mConn == sender());

    if (mMakeDone) {
        mLoop.quit();
    }
}

void Client::onNewMessage(Message *message)
{
    Q_ASSERT(mConn == sender());
    if (message->messageId() == ResponseMessage::MessageId) {
        const QByteArray response = static_cast<ResponseMessage*>(message)->data();
        if (!response.isEmpty()) {
            printf("%s\n", response.constData());
        }
    } else {
        qFatal("Unexpected message: %d", message->messageId());
    }
    message->deleteLater();
}

bool Client::parseMakefile(const Path &path, bool wait)
{
    if (!mConn && !connectToServer()) {
        fprintf(stderr, "Can't connect to server\n");
        return false;
    }

    connect(mConn, SIGNAL(sendComplete()), this, SLOT(onSendComplete()));

    mSourceFileCount = mPchCount = 0;
    MakefileParser* parser = new MakefileParser(mExtraFlags, this);

    if (wait) {
        connect(mConn, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    } else {
        connect(parser, SIGNAL(done()), this, SLOT(onMakefileDone()));
    }
    connect(parser, SIGNAL(fileReady(GccArguments)),
            this, SLOT(onMakefileReady(GccArguments)));
    parser->run(path);
    mMakeDone = false;
    mLoop.exec();
    return true;
}

void Client::onMakefileDone()
{
    mMakeDone = true;
    if (!mConn || !mConn->pendingWrite()) {
        mLoop.quit();
    }
    sender()->deleteLater();
}

QList<QByteArray> Client::mapPchToInput(const QList<QByteArray> &input)
{
    QList<QByteArray> output;
    QHash<QByteArray, QByteArray>::const_iterator pchit;
    const QHash<QByteArray, QByteArray>::const_iterator pchend = mPchs.end();
    foreach (const QByteArray &in, input) {
        pchit = mPchs.find(in);
        if (pchit != pchend)
            output.append(pchit.value());
    }
    return output;
}

void Client::onMakefileReady(const GccArguments &args)
{
    if (args.inputFiles().isEmpty()) {
        warning("no input file?");
        return;
    } else if (args.outputFile().isEmpty()) {
        warning("no output file?");
        return;
    } else if (args.type() == GccArguments::NoType || args.lang() == GccArguments::NoLang) {
        return;
    }

    if (args.type() == GccArguments::Pch) {
        QByteArray output = args.outputFile();
        Q_ASSERT(!output.isEmpty());
        const int ext = output.lastIndexOf(".gch/c");
        if (ext != -1) {
            output = output.left(ext + 4);
        } else if (!output.endsWith(".gch")) {
            error("couldn't find .gch in pch output");
            return;
        }
        const QByteArray input = args.inputFiles().front();

        RTags::UnitType type = (args.lang() == GccArguments::C) ? RTags::PchC : RTags::PchCPlusPlus;
        // using input for both input and output is correct here
        AddMessage message(type, input, input, args.clangArgs(),
                           mapPchToInput(args.explicitIncludes()));
        warning() << "sending" << "input:" << input << "output:" << output
                  << "args:" << args.clangArgs() << "incs:" << mapPchToInput(args.explicitIncludes());
        mConn->send(&message);

        mPchs[output] = input;
        ++mPchCount;
    } else {
        const QByteArray input = args.inputFiles().front();
        const QByteArray output = args.outputFile();
        RTags::UnitType type = (args.lang() == GccArguments::C) ? RTags::CompileC : RTags::CompileCPlusPlus;
        AddMessage message(type, input, output, args.clangArgs(),
                           mapPchToInput(args.explicitIncludes()));
        if (testLog(Warning)) {
            warning() << "sending" << "input:" << input << "output:" << output
                      << "args:" << args.clangArgs() << "incs:" << mapPchToInput(args.explicitIncludes());
        }
        mConn->send(&message);
        ++mSourceFileCount;
    }
}

void Client::onDisconnected()
{
    if (sender() == mConn) {
        mConn->deleteLater();
        mConn = 0;
        mLoop.quit();
    }
}
bool Client::connectToServer()
{
    Q_ASSERT(!mConn);
    mConn = new Connection(this);
    error("About to connect to server");
    if (!mConn->connectToServer(mName)) {
        error("Failed to connect to server");
        if (mFlags & AutostartRdm) {
            QString cmd = QCoreApplication::arguments().value(0);
            const int lastSlash = cmd.lastIndexOf('/');
            if (lastSlash != -1) {
                cmd.replace(lastSlash + 1, cmd.size() - lastSlash - 1, "rdm");
            } else {
                cmd = "rdm";
            }
            error("trying to start rdm %s [%s]", qPrintable(cmd), RTags::join(mRdmArgs, " ").constData());
            if (RTags::startProcess(cmd.toLocal8Bit(), mRdmArgs)) {
                error("Started successfully");
                for (int i=0; i<5; ++i) {
                    if (mConn->connectToServer(mName)) {
                        return true;
                    }
                    sleep(1);
                }
            } else {
                error("Couldn't start");
            }

        }

        warning("Can't connect to host");
        delete mConn;
        mConn = 0;
        return false;
    }
    return true;
}
