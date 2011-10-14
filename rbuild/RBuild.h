#ifndef RBUILD_H
#define RBUILD_H

#include "MakefileParser.h"
#include "Path.h"
#include "GccArguments.h"
#include "SystemInformation.h"
#include <QObject>

class RBuild : public QObject
{
    Q_OBJECT
public:
    RBuild(QObject *parent = 0);

    void init(const Path& makefile);

    struct Entry
    {
        Entry();
        ~Entry();

        Path file;
        unsigned int line, column;
        QByteArray field;

        int cxKind;

        QList<Entry*> children;
        Entry* parent, *container;
    };

private slots:
    void makefileDone();
    void makefileFileReady(const MakefileItem& file);
    void startParse();

private:
    void compile(const GccArguments& arguments);

private:
    Path mMakefile;
    MakefileParser mParser;
    QList<Entry*> mEntries;
    QHash<QByteArray, RBuild::Entry*> mSeen;
    SystemInformation mSysInfo;
};

#endif // RBUILD_H
