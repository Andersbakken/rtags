#include <QCoreApplication>
#include <QString>
#include <QList>
#include <stdio.h>
#include "Daemon.h"
#include "Client.h"
#include "Utils.h"
#include <syslog.h>

#define CLIENT_CONNECT_ATTEMPTS 5
#define CLIENT_CONNECT_DELAY 1

void syslogMsgHandler(QtMsgType t, const char* str)
{
    int priority = LOG_WARNING;
    static const char *names[] = { "DEBUG", "WARNING", "CRITICAL", "FATAL" };
    const bool noColors = getenv("RTAGS_CONSOLE_NO_COLOR");
    const char *colorStart = "";
    const char *colorEnd = "";

    switch (t) {
    case QtDebugMsg:
        colorStart = "\x1b[36m"; // cyan
        priority = LOG_DEBUG;
        break;
    case QtWarningMsg:
        colorStart = "\x1b[31m"; // red
        priority = LOG_WARNING;
        break;
    case QtCriticalMsg:
        colorStart = "\x1b[31m";
        priority = LOG_CRIT;
        break;
    case QtFatalMsg:
        colorStart = "\x1b[41;37m";
        priority = LOG_CRIT;
        break;
    }
    if (noColors) {
        colorStart = "";
    } else if (colorStart) {
        colorEnd = "\x1b[0m";
    }
    fprintf(stderr, "%s%s (%s): %s%s\n", colorStart,
            qPrintable(QDateTime::currentDateTime().toString()),
            qPrintable(QThread::currentThread()->objectName()),
            str, colorEnd);
    QFile file("/tmp/rtags.log");
    file.open(QIODevice::WriteOnly|QIODevice::Append);
    char buf[1024];
    const int s = snprintf(buf, 1023, "%s (%s)\n", str, names[t]);
    file.write(buf, s);
    syslog(priority, "%s (%s)\n", str, names[t]);
}

class ArgParser
{
public:
    ArgParser(int argc, char** argv);

    bool isValid() const;

    QHash<QByteArray, QVariant> dashArguments() const;
    QList<QByteArray> freeArguments() const;

private:
    bool parse(int argc, char** argv);
    void addValue(const QByteArray& key, const QByteArray& value);
private:
    bool m_valid;
    QHash<QByteArray, QVariant> m_dash;
    QList<QByteArray> m_free;
};

ArgParser::ArgParser(int argc, char **argv)
{
    m_valid = parse(argc, argv);
}

bool ArgParser::isValid() const
{
    return m_valid;
}

QHash<QByteArray, QVariant> ArgParser::dashArguments() const
{
    return m_dash;
}

QList<QByteArray> ArgParser::freeArguments() const
{
    return m_free;
}

void ArgParser::addValue(const QByteArray &key, const QByteArray &value)
{
    if (key == "command") { // don't want to resolve makefile to /some/path/Makefile on mac
        m_dash[key] = value;
        return;
    }
    bool ok;
    int intvalue = value.toInt(&ok);
    if (ok) {
        m_dash[key] = intvalue;
        return;
    }
    double doublevalue = value.toDouble(&ok);
    if (ok) {
        m_dash[key] = doublevalue;
        return;
    }

    Path copy = value;
    if (!copy.isResolved() && copy.resolve()) {
        m_dash[key] = copy;
    } else {
        m_dash[key] = value;
    }
}

bool ArgParser::parse(int argc, char **argv)
{
    Q_ASSERT(argc > 0);
    ++argv; // skip the application name

    m_dash.clear();
    m_free.clear();

    QByteArray current;
    const char** end = const_cast<const char**>(argv + argc);
    for (; argv != end; ++argv) {
        current = *argv;
        if (current.startsWith('-')) {
            const int eqpos = current.indexOf('=');
            if (eqpos == -1) { // no '=' present, just add the argument
                while (!current.isEmpty() && current.at(0) == '-')
                    current = current.mid(1);
                if (current.isEmpty())
                    return false;
                m_dash[current] = QVariant();
            } else { // use everything past '='
                QByteArray value = current.mid(eqpos + 1);
                current = current.left(eqpos);
                while (!current.isEmpty() && current.at(0) == '-')
                    current = current.mid(1);
                if (value.isEmpty() || current.isEmpty())
                    return false;
                addValue(current, value);
            }
        } else { // doesn't start with a '-', add as a free argument
            if (!current.isEmpty()) {
                Path copy = current;
                if (!copy.isResolved() && copy.resolve()) {
                    m_free.append(copy);
                } else {
                    m_free.append(current);
                }
            }
        }
    }
    return true;
}

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);
    Path::initStaticData();
    QThread::currentThread()->setObjectName("main");
    qRegisterMetaType<QList<QByteArray> >();
    qRegisterMetaType<ByteArrayHash>();
    qRegisterMetaTypeStreamOperators<QList<QByteArray> >("QList<QByteArray>");
    qRegisterMetaTypeStreamOperators<ByteArrayHash>("ByteArrayHash");
    ArgParser args(argc, argv);
    QHash<QByteArray, QVariant> argsmap = args.dashArguments();
    if (argsmap.contains("verbose")) {
        Options::s_verbose = true;
        argsmap.remove("verbose");
    }
    if (argsmap.contains("v")) {
        argsmap.remove("v");
        Options::s_verbose = true;
    }

    if (argsmap.contains("trace-function-calls")) {
        argsmap.remove("trace-function-calls");
        Options::s_traceFunctionCalls = true;
    }

    if (argsmap.isEmpty())
        argsmap.insert("command", "syntax");

    QByteArray cmd = argsmap.value("command").toByteArray();
    FUNC;
    QCoreApplication::setOrganizationDomain("www.rtags.com");
    QCoreApplication::setOrganizationName("RTags");
    QCoreApplication::setApplicationName("rtags");

    if (Options::s_verbose)
        qDebug() << argsmap;

    if (cmd == "daemonize") {
        Daemon daemon;
        qInstallMsgHandler(syslogMsgHandler);
        if (daemon.start())
            return app.exec();
        else
            return -2;
    } else {
        Client client;
        if (!client.connect()) {
            if (cmd == "quit") {
                qWarning("Can't connect to rtags daemon");
                return 0;
            }
            if (!getenv("RTAGS_NO_AUTOSTART")) {
                client.startDaemon(app.arguments());
                for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
                    if (client.connect()) {
                        break;
                    }
                    sleep(CLIENT_CONNECT_DELAY);
                }
                qWarning("Can't connect ot rtags daemon");
                return 1;
            }
        }
        for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
            if (client.connected()) {
                QHash<QByteArray, QVariant> replymap = client.exec(argsmap, args.freeArguments());
                QString reply = replymap.value("result").toString();
                if (!reply.isEmpty())
                    printf("%s\n", qPrintable(reply));
                return 0;
            }
            sleep(CLIENT_CONNECT_DELAY);
        }
    }
    qWarning("Couldn't connect to daemon");

    return -1;
}
