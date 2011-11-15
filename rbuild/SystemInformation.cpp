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
    QByteArray gxxIncludeDir, target;
    foreach(const QByteArray& line, lines) {
        if (gxxIncludeDir.isEmpty()) {
            int idx = line.indexOf("--with-gxx-include-dir=");
            if (idx != -1) {
                const int space = line.indexOf(' ', idx);
                gxxIncludeDir = line.mid(idx + 23, space - idx - 23);
            }
            idx = line.indexOf("--target=");
            if (idx != -1) {
                const int space = line.indexOf(' ', idx);
                target = line.mid(idx + 9, space - idx - 9);
            }
        } else if (!seenInclude && line.startsWith("#include ")) {
            seenInclude = true;
        } else if (seenInclude && line.startsWith(" /")) {
            Path path = Path::resolved(line.mid(1));
            if (path.isResolved()) {
                mSystemIncludes.append("-I" + path);
            }
        }
    }
    if (!gxxIncludeDir.isEmpty()) {
        mSystemIncludes.append("-I" + gxxIncludeDir);
        if (!target.isEmpty()) {
            mSystemIncludes.append("-I" + gxxIncludeDir + "/" + target);
        }
    }
}

QList<QByteArray> SystemInformation::systemIncludes() const
{
    return mSystemIncludes;
}
