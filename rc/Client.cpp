#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QDebug>
#include <Log.h>

Client::Client(unsigned flags, const QList<QByteArray> &extraFlags, QObject* parent)
    : QObject(parent), mConn(0), mFlags(flags), mMakeDone(false), mExtraFlags(extraFlags),
      mSourceFileCount(0), mPchCount(0)
{
    Messages::init();
}

void Client::parseMakefile(const Path& path)
{
    mSourceFileCount = mPchCount = 0;
    MakefileParser* parser = new MakefileParser(mExtraFlags, this);
    connect(parser, SIGNAL(done()), this, SLOT(onMakefileDone()));
    connect(parser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(onMakefileReady(const GccArguments&)));
    parser->run(path);
    mMakeDone = false;
    qApp->exec();
}

void Client::query(const QueryMessage &message)
{
    mConn = new Connection(this);
    if (!mConn->connectToHost("localhost", Connection::Port)) {
        warning("Can't connect to host");
        delete mConn;
        mConn = 0;
        return;
    }
    connect(mConn, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    connect(mConn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
    mConn->send(&message);
    qApp->exec();
}

void Client::onSendComplete()
{
    Q_ASSERT(mConn == sender());

    if (mMakeDone)
        qApp->quit();
}

void Client::onNewMessage(Message* message)
{
    Q_ASSERT(mConn == sender());
    if (message->messageId() == QueryMessage::MessageId) {
        foreach (const QByteArray& r, static_cast<QueryMessage*>(message)->query()) {
            if (!r.isEmpty()) {
                printf("%s\n", r.constData());
            }
        }
    } else {
        qFatal("Unexpected message: %d", message->messageId());
    }
    message->deleteLater();
}

void Client::onMakefileDone()
{
    if (mMakeDone)
        return;
    mMakeDone = true;
    if (!mConn || !mConn->pendingWrite())
        qApp->quit();
    sender()->deleteLater();
}

QList<QByteArray> Client::mapPchToInput(const QList<QByteArray>& input)
{
    QList<QByteArray> output;
    QHash<QByteArray, QByteArray>::const_iterator pchit;
    const QHash<QByteArray, QByteArray>::const_iterator pchend = mPchs.end();
    foreach (const QByteArray& in, input) {
        pchit = mPchs.find(in);
        if (pchit != pchend)
            output.append(pchit.value());
    }
    return output;
}

void Client::onMakefileReady(const GccArguments& args)
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
    if (!mConn) {
        mConn = new Connection(this);
        if (!mConn->connectToHost("localhost", Connection::Port)) {
            error("Can't connect to host");
            sender()->deleteLater();
            return;
        }
        connect(mConn, SIGNAL(sendComplete()), this, SLOT(onSendComplete()));
    }

    if (args.type() == GccArguments::Pch) {
        QByteArray output = args.outputFile();
        Q_ASSERT(!output.isEmpty());
        const int ext = output.lastIndexOf(".gch/c");
        if (ext <= 0) {
            warning("couldn't find .gch in pch output");
            return;
        }
        output = output.left(ext + 4);
        const QByteArray input = args.inputFiles().front();

        RTags::UnitType type = (args.lang() == GccArguments::C) ? RTags::PchC : RTags::PchCPlusPlus;
        // using input for both input and output is correct here
        AddMessage message(type, input, input, args.clangArgs(),
                           mapPchToInput(args.explicitIncludes()));
        if (logLevel()) {
            warning() << "sending" << "input:" << input << "output:" << output
                      << "args:" << args.clangArgs() << "incs:" << mapPchToInput(args.explicitIncludes());
        }
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
        qApp->quit();
    }
}
