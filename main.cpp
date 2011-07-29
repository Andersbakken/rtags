#include <QCoreApplication>
#include <QString>
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
    fprintf(stderr, "%s%s%s\n", colorStart, str, colorEnd);
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

    QHash<QByteArray, QVariant> arguments() const;

private:
    bool parse(int argc, char** argv);
    void addValue(const QByteArray& key, const QByteArray& value);

private:
    bool m_valid;
    QHash<QByteArray, QVariant> m_args;
};

ArgParser::ArgParser(int argc, char **argv)
{
    m_valid = parse(argc, argv);
}

bool ArgParser::isValid() const
{
    return m_valid;
}

QHash<QByteArray, QVariant> ArgParser::arguments() const
{
    return m_args;
}

void ArgParser::addValue(const QByteArray &key, const QByteArray &value)
{
    if (key == "command") { // don't want to resolve makefile to /some/path/Makefile on mac
        m_args[key] = value;
        return;
    }
    bool ok;
    int intvalue = value.toInt(&ok);
    if (ok) {
        m_args[key] = intvalue;
        return;
    }
    double doublevalue = value.toDouble(&ok);
    if (ok) {
        m_args[key] = doublevalue;
        return;
    }

    QByteArray copy = value;
    if (resolvePath(copy)) { // make all valid paths absolute
        m_args[key] = copy;
    } else {
        m_args[key] = value;
    }
}

bool ArgParser::parse(int argc, char **argv)
{
    m_args.clear();

    QByteArray current;
    const char** end = const_cast<const char**>(argv + argc);
    for (; argv != end; ++argv) {
        current = *argv;
        if (current.startsWith('-')) {
            const int eqpos = current.indexOf('=');
            if (eqpos == -1) { // add argument and take the next if it doesn't start with a '-'
                ++argv;
                while (!current.isEmpty() && current.at(0) == '-')
                    current = current.mid(1);
                if (current.isEmpty())
                    return false;
                if (argv == end) {
                    m_args[current] = QVariant();
                    return true;
                }
                QByteArray value = *argv;
                if (value.startsWith('-')) {
                    // next argument is an option as well, don't take it
                    --argv;
                    m_args[current] = QVariant();
                } else {
                    addValue(current, value);
                }
            } else { // use everything past '='
                QByteArray value = current.mid(eqpos + 1);
                current = current.left(eqpos);
                while (!current.isEmpty() && current.at(0) == '-')
                    current = current.mid(1);
                if (value.isEmpty() || current.isEmpty())
                    return false;
                addValue(current, value);
            }
        } else { // doesn't start with a '-', ignore for now
        }
    }
    return true;
}

int main(int argc, char** argv)
{
    // QByteArray path = "/Users/abakken/dev";
    // resolvePath(path);
    // qDebug() << path;
    // path = "/Users/abakken/.bashrc";
    // resolvePath(path);
    // qDebug() << path;
    // path = "./Makefile";
    // resolvePath(path);
    // qDebug() << path;
    // path = "Makefile";
    // resolvePath(path);
    // qDebug() << path;
    // path = "../../.bashrc";
    // qDebug() << resolvePath(path);
    // qDebug() << path;
    // path = "../../.bashrc2";
    // qDebug() << resolvePath(path);
    // qDebug() << path;
    // qDebug() << resolvePath(path);
    // qDebug() << path;
    // return 0;

    QCoreApplication app(argc, argv);
    QThread::currentThread()->setObjectName("main");
    ArgParser args(argc, argv);
    QHash<QByteArray, QVariant> argsmap = args.arguments();
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
            client.startDaemon(app.arguments());
            for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
                if (client.connect()) {
                    break;
                }
                sleep(CLIENT_CONNECT_DELAY);
            }
        }
        for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
            if (client.connected()) {
                QHash<QByteArray, QVariant> replymap = client.exec(argsmap);
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
