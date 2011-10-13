#include "RBuild.h"
#include "Path.h"
#include <QCoreApplication>
#include <QDir>

int main(int argc, char** argv)
{
    QCoreApplication app(argc, argv);

    bool ok;
    Path appPath = Path::resolved(QDir::currentPath().toLocal8Bit(), Path(), &ok);
    if (!ok)
        qFatal("Unable to resolve initial path");

    RBuild build;
    build.init(Path::resolved("Makefile", appPath));

    return app.exec();
}
