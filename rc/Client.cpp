#include "Client.h"
#include "Messages.h"
#include "Connection.h"
#include "MakefileParser.h"
#include <QCoreApplication>
#include <QDir>
#include <QFileInfo>
#include <QDebug>

Client::Client(Verbosity verbosity, QObject* parent)
    : QObject(parent), m_conn(0), m_verbosity(verbosity), m_makeDone(false)
{
    Messages::init();
}

void Client::parseMakefile(const QByteArray& makefile)
{
    MakefileParser::Verbosity v = MakefileParser::Silent;
    if (m_verbosity == Verbose)
        v = MakefileParser::Verbose;
    MakefileParser* parser = new MakefileParser(v, this);
    Path path = Path::resolved(makefile);
    connect(parser, SIGNAL(done()), this, SLOT(onMakefileDone()));
    connect(parser, SIGNAL(fileReady(const GccArguments&)),
            this, SLOT(onMakefileReady(const GccArguments&)));
    parser->run(path);
    m_makeDone = false;
    qApp->exec();
}

void Client::query(QueryType type, const QByteArray& msg)
{
    QueryMessage::Type qmt = QueryMessage::FollowLocation; // make the compiler not complain
    switch (type) {
    case FollowLocation:
        qmt = QueryMessage::FollowLocation;
        break;
    case Recompile:
        qmt = QueryMessage::Recompile;
        break;
    case ReferencesLocation:
        qmt = QueryMessage::ReferencesLocation;
        break;
    case ReferencesName:
        qmt = QueryMessage::ReferencesName;
        break;
    case Match:
        qmt = QueryMessage::Match;
        break;
    }

    m_conn = new Connection(this);
    m_conn->connectToHost("localhost", Connection::Port);
    QueryMessage message(msg, qmt);
    m_conn->send(&message);
    connect(m_conn, SIGNAL(newMessage(Message*)), this, SLOT(onNewMessage(Message*)));
    qApp->exec();
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
            if (!r.isEmpty())
                printf("%s\n", r.constData());
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
    if (args.type() == GccArguments::Unknown)
        return;

    if (args.inputFiles().isEmpty()) {
        qWarning("no input file?");
        return;
    }

    if (!m_conn) {
        m_conn = new Connection(this);
        m_conn->connectToHost("localhost", Connection::Port);
        connect(m_conn, SIGNAL(sendComplete()), this, SLOT(onSendComplete()));
    }

    const QByteArray input = args.inputFiles().front();
    const QByteArray output = args.outputFile();
    AddMessage message((args.type() == GccArguments::Compile ? AddMessage::Compile : AddMessage::Pch),
                       input, output, args.clangArgs());
    if (m_verbosity == Verbose)
        qDebug() << "sending" << "input:" << input << "output:" << output << "args:" << args.clangArgs();
    m_conn->send(&message);
}
