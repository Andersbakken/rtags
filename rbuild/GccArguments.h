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
    bool parse(const QByteArray& cmd, const Path &resolvedPath);
    QByteArray raw() const;
    Path dir() const;
    bool isNull() const;
    bool isEmpty() const;
    QString errorString() const;

    QList<QByteArray> arguments() const;
    QList<QByteArray> arguments(const QByteArray& prefix) const;
    int argumentCount() const;
    QList<Path> includePaths() const;
    enum ClangArgFlag {
        IncludePaths = 0x01,
        Defines = 0x02,
        OtherArgs = 0x04,
        AllArgs = IncludePaths|Defines|OtherArgs
    };
    int getClangArgs(const char **args, int max, unsigned flags) const;

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
    bool operator==(const GccArguments &other) const;
 private:
    void parseCD(const QByteArray& cmd, const Path& path);

    class Data : public QSharedData
    {
    public:
        Data();

        GccArguments::Language guessLanguage() const;
        QByteArray languageString() const;

        struct Argument
        {
            Argument(int p = 0, const QByteArray& a = QByteArray(), const QByteArray& v = QByteArray())
                : pos(p), arg(a), value(v)
            {}

            int pos;
            QByteArray arg;
            QByteArray value;
        };

        QList<int> input;
        int output;
        int x;
        int c;
        QString error;
        Language language;

        QByteArray inputreplace;
        QByteArray outputreplace;
        QByteArray raw;
        Path dir;
        QList<Argument> args;
    };

    QSharedDataPointer<Data> m_ptr;

    friend QDataStream& operator<<(QDataStream& stream, const GccArguments& args);
    friend QDataStream& operator>>(QDataStream& stream, GccArguments& args);
    friend QDataStream& operator<<(QDataStream& stream, const GccArguments::Data::Argument& args);
    friend QDataStream& operator>>(QDataStream& stream, GccArguments::Data::Argument& args);
};

Q_DECLARE_METATYPE(GccArguments)

static inline QDebug operator<<(QDebug dbg, const GccArguments &args)
{
    dbg.nospace() << "GccArguments(" << args.raw() << ")";
    return dbg.maybeSpace();
}

QDataStream& operator<<(QDataStream& stream, const GccArguments& args);
QDataStream& operator>>(QDataStream& stream, GccArguments& args);

QDataStream& operator<<(QDataStream& stream, const GccArguments::Data::Argument &arg);
QDataStream& operator>>(QDataStream& stream, GccArguments::Data::Argument &arg);

#endif
