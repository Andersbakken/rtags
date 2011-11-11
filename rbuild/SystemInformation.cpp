#include "SystemInformation.h"
#include "Path.h"
#include <QProcess>
#include <QEventLoop>
#include <QDebug>

SystemInformation::SystemInformation()
{
}

void SystemInformation::init()
{
    mSystemIncludes.clear();

    QProcess proc;
    proc.start(QLatin1String("cpp"), QStringList() << QLatin1String("-v"));
    proc.closeWriteChannel();
    proc.waitForFinished();
    QList<QByteArray> lines = proc.readAllStandardError().split('\n');
    bool seenInclude = false;
    foreach(const QByteArray& line, lines) {
        const int idx = line.indexOf("--with-gxx-include-dir=");
        if (!seenInclude && idx != -1) {
            const int space = line.indexOf(' ', idx);
            mSystemIncludes.append("-I" + line.mid(idx + 23, space - idx - 23));
            continue;
        }
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
}

QList<QByteArray> SystemInformation::systemIncludes() const
{
    return mSystemIncludes;
}
