#ifndef DAEMON_H
#define DAEMON_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QHash>
#include <QThreadPool>
#include <QFileSystemWatcher>
#include <clang-c/Index.h>
#include "Utils.h"
#include "GccArguments.h"
#include "Path.h"
#include "Location.h"
#include "ParseThread.h"
#include "VisitThread.h"
#include "FileManager.h"

struct Node;
class Daemon : public QObject
{
    Q_OBJECT;
public:
    Daemon(QObject* parent = 0);
    ~Daemon();

    void addMakefile(Path makefile);
public slots:
    void quit();
    void onMaybeDone();
private:
    ParseThread mParseThread;
    VisitThread mVisitThread;
    FileManager mFileManager;
};

#endif
