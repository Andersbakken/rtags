#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QDebug>
#include <Log.h>

Client::Client(int flags, QObject* parent)
    : QObject(parent), mConn(0), mFlags(flags), mMakeDone(false)
{
    Messages::init();
}

void Client::parseMakefile(const Path& path)
{
    MakefileParser* parser = new MakefileParser(this);
    connect(parser, SIGNAL(done()), this, SLOT(onMakefileDone()));
    connect(parser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(onMakefileReady(const GccArguments&)));
    parser->run(path);
    mMakeDone = false;
    qApp->exec();
}

void Client::query(QueryMessage::Type type, const QByteArray& msg, const QHash<Path, QByteArray> &unsavedFiles)
{
    mConn = new Connection(this);
    if (mConn->connectToHost("localhost", Connection::Port)) {
        QueryMessage message(msg, type, mFlags, unsavedFiles);
        connect(mConn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
        mConn->send(&message);
        qApp->exec();
    } else {
        warning("Can't connect to host");
    }
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
                if ((mFlags & SkipParen) && r.contains("("))
                    continue;
                printf("%s\n", r.constData());
            }
        }
        qApp->quit();
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

    if (args.type() == GccArguments::NoType
        || args.lang() == GccArguments::NoLang)
        return;
    else if (args.type() == GccArguments::Pch) {
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
            log(1) << "sending" << "input:" << input << "output:" << output
                   << "args:" << args.clangArgs() << "incs:" << mapPchToInput(args.explicitIncludes());
        }
        mConn->send(&message);

        mPchs[output] = input;

        return;
    }

    const QByteArray input = args.inputFiles().front();
    const QByteArray output = args.outputFile();
    RTags::UnitType type = (args.lang() == GccArguments::C) ? RTags::CompileC : RTags::CompileCPlusPlus;
    AddMessage message(type, input, output, args.clangArgs(),
                       mapPchToInput(args.explicitIncludes()));
    if (logLevel()) {
        log(1) << "sending" << "input:" << input << "output:" << output
               << "args:" << args.clangArgs() << "incs:" << mapPchToInput(args.explicitIncludes());
    }
    mConn->send(&message);
}
