#ifndef RBUILD_H
#define RBUILD_H

#include "MakefileParser.h"
#include "Path.h"
#include "GccArguments.h"
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
        Entry* parent;
    };

private slots:
    void makefileDone();
    void makefileFileReady(const MakefileItem& file);

private:
    void compile(const GccArguments& arguments);

private:
    MakefileParser mParser;
    QList<Entry*> mEntries;
    QHash<QByteArray, RBuild::Entry*> mSeen;
};

#endif // RBUILD_H
