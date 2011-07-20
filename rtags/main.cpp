#include <QCoreApplication>
#include <QString>
#include <stdio.h>
#include "daemon.h"
#include "client.h"

#define CLIENT_CONNECT_ATTEMPTS 5
#define CLIENT_CONNECT_DELAY 1

static QStringList buildArgs(const QStringList& args, const QString& cmd)
{
    QStringList ret = args;
    if (ret.size() == 1)
        ret << cmd;
    else if (ret.size() > 1)
        ret[1] = cmd;
    return ret;
}

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);

    QByteArray cmd;
    if (argc == 1) {
        cmd = "syntax";
    } else {
        cmd = argv[1];
    }

    if (cmd == "daemonize") {
        Daemon daemon;
        if (daemon.start())
            return app.exec();
        else
            return -2;
    } else {
        Client client;
        if (!client.connect()) {
            client.startDaemon(app.arguments());
            for (int i = 0; i < CLIENT_CONNECT_ATTEMPTS; ++i) {
                if (client.connect())
                    break;
                sleep(CLIENT_CONNECT_DELAY);
            }
        }
        if (client.connected()) {
            QString reply = client.exec(buildArgs(app.arguments(), cmd));
            if (!reply.isEmpty())
                printf("%s\n", qPrintable(reply));
            return 0;
        }
    }

    return -1;
}
