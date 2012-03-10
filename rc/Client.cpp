#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QDebug>

Client::Client(int flags, QObject* parent)
    : QObject(parent), m_conn(0), m_flags(flags), m_makeDone(false)
{
    Messages::init();
}

void Client::parseMakefile(const Path& path)
{
    MakefileParser::Verbosity v = MakefileParser::Silent;
    if (m_flags & Verbose)
        v = MakefileParser::Verbose;
    MakefileParser* parser = new MakefileParser(v, this);
    connect(parser, SIGNAL(done()), this, SLOT(onMakefileDone()));
    connect(parser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(onMakefileReady(const GccArguments&)));
    parser->run(path);
    m_makeDone = false;
    qApp->exec();
}

void Client::query(QueryMessage::Type type, const QByteArray& msg, const QHash<Path, QByteArray> &unsavedFiles)
{
    m_conn = new Connection(this);
    if (m_conn->connectToHost("localhost", Connection::Port)) {
        QueryMessage message(msg, type, unsavedFiles);
        m_conn->send(&message);
        connect(m_conn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
        qApp->exec();
    } else {
        qWarning("Can't connect to host");
    }
}

void Client::onSendComplete()
{
    Q_ASSERT(m_conn == sender());

    if (m_makeDone)
        qApp->quit();
}

void Client::onNewMessage(Message* message)
{
    Q_ASSERT(m_conn == sender());
    if (message->messageId() == QueryMessage::MessageId) {
        foreach(const QByteArray& r, static_cast<QueryMessage*>(message)->query()) {
            if (!r.isEmpty()) {
                if ((m_flags & SkipParen)
                    && r.contains("("))
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
    if (m_makeDone)
        return;
    m_makeDone = true;
    if (!m_conn || !m_conn->pendingWrite())
        qApp->quit();
}

void Client::onMakefileReady(const GccArguments& args)
{
    if (args.inputFiles().isEmpty()) {
        qWarning("no input file?");
        return;
    }

    if (!m_conn) {
        m_conn = new Connection(this);
        if (!m_conn->connectToHost("localhost", Connection::Port)) {
            qWarning("Can't connect to host");
            QCoreApplication::quit();
            return;
        }
        connect(m_conn, SIGNAL(sendComplete()), this, SLOT(onSendComplete()));
    }

    if (args.type() == GccArguments::Unknown)
        return;
    else if (args.type() == GccArguments::Pch) {
        QByteArray output = args.outputFile();
        if (output.isEmpty()) {
            qWarning("no output file for pch");
            return;
        }
        const int ext = output.lastIndexOf(".gch/c");
        if (ext <= 0) {
            qWarning("couldn't find .gch in pch output");
            return;
        }
        output = output.left(ext + 4);
        const QByteArray input = args.inputFiles().front();

        AddMessage message(AddMessage::Pch, input, output, args.clangArgs(),
                           args.explicitIncludes());
        if (m_flags & Verbose)
            qDebug() << "sending" << "input:" << input << "output:" << output << "args:" << args.clangArgs() << "incs:" << args.explicitIncludes();
        m_conn->send(&message);
        return;
    }

    const QByteArray input = args.inputFiles().front();
    const QByteArray output = args.outputFile();
    AddMessage message(AddMessage::Compile, input, output, args.clangArgs(),
                       args.explicitIncludes());
    if (m_flags & Verbose)
        qDebug() << "sending" << "input:" << input << "output:" << output << "args:" << args.clangArgs() << "incs:" << args.explicitIncludes();
    m_conn->send(&message);
}
