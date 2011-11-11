#include "SystemInformation.h"
#include "Path.h"
#include <QProcess>
#include <QEventLoop>
#include <QDebug>

SystemInformation::SystemInformation(QObject *parent)
    : QObject(parent)
{
}

void SystemInformation::init()
{
    mSystemIncludes.clear();

    QProcess* proc = new QProcess(this);
    connect(proc, SIGNAL(finished(int)), this, SLOT(parseSystemIncludes()));
    proc->start(QLatin1String("cpp"), QStringList() << QLatin1String("-v"));
    proc->closeWriteChannel();
    proc->waitForFinished();
}

QList<QByteArray> SystemInformation::systemIncludes() const
{
    return mSystemIncludes;
}

void SystemInformation::parseSystemIncludes()
{
    QProcess* proc = qobject_cast<QProcess*>(sender());
    Q_ASSERT(proc);

    QList<QByteArray> lines = proc->readAllStandardError().split('\n');
    bool seenInclude = false;
    foreach(const QByteArray& line, lines) {
        if (!seenInclude && line.startsWith("#include ")) {
            seenInclude = true;
            continue;
        }
        if (seenInclude && line.startsWith(" /")) {
            Path path = Path::resolved(line.mid(1));
            if (path.isResolved()) {
                mSystemIncludes.append("-I" + path);
            }
        }
    }

    proc->deleteLater();

    emit done();
}
