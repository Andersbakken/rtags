#ifndef ARGS_H
#define ARGS_H

#include <QHash>
#include <QByteArray>
#include <QDataStream>
#include <QSharedData>
#include <QSharedDataPointer>
#include <QDebug>
#include "Path.h"

class GccArguments
{
public:
    enum Language { LangUndefined, LangC, LangCPlusPlus, LangObjC, LangObjCPlusPlus };

    GccArguments();

    bool parse(const QByteArray& args, const Path &resolvedPath);
    QByteArray raw() const;
    Path dir() const;
    QString errorString() const;

    QList<QByteArray> arguments() const;
    QList<QByteArray> arguments(const QByteArray& prefix) const;
    QList<QByteArray> includePaths() const;

    void setPreprocess(bool pre);
    void setLanguage(Language language);
    bool setReplaceInput(const QByteArray& input = QByteArray());
    void setReplaceOutput(const QByteArray& output = QByteArray());

    QByteArray compiler() const;
    QList<Path> input() const;
    QByteArray firstInput() const;
    QByteArray output() const;
    Language language() const;

    bool hasInput() const;
    bool hasOutput() const;

    bool isCompile() const;

private:
    class Data : public QSharedData
    {
    public:
        Data();

        GccArguments::Language guessLanguage() const;
        QByteArray languageString() const;

        struct Argument
        {
            Argument(int p, const QByteArray& a) : pos(p), arg(a) {}
            Argument(int p, const QByteArray& a, const QByteArray& v) : pos(p), arg(a), value(v) {}

            int pos;
            QByteArray arg;
            QByteArray value;
        };

        QList<int> input;
        int output;
        int x;
        int c;
        QList<Argument> args;
        QString error;
        Language language;

        QByteArray inputreplace;
        QByteArray outputreplace;
        QByteArray raw;
        Path dir;
    };

    QSharedDataPointer<Data> m_ptr;

    friend QDataStream& operator<<(QDataStream& stream, const GccArguments& args);
    friend QDataStream& operator>>(QDataStream& stream, GccArguments& args);
};

static inline QDebug operator<<(QDebug dbg, const GccArguments &args)
{
    dbg.nospace() << "GccArguments(" << args.raw() << ")";
    return dbg.maybeSpace();
}

QDataStream& operator<<(QDataStream& stream, const GccArguments& args);
QDataStream& operator>>(QDataStream& stream, GccArguments& args);

#endif
