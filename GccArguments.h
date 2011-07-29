#ifndef ARGS_H
#define ARGS_H

#include <QHash>
#include <QByteArray>
#include <QDataStream>
#include <QSharedData>
#include <QSharedDataPointer>
#include <QDebug>

class GccArguments
{
public:
    enum Language { LangUndefined, LangC, LangCPlusPlus, LangObjC, LangObjCPlusPlus };

    GccArguments();

    bool parse(const QByteArray& args, const QByteArray &dirPath);
    QByteArray raw() const;
    QByteArray dirPath() const;
    QString errorString() const;

    QList<QByteArray> arguments() const;
    QList<QByteArray> arguments(const QByteArray& prefix) const;
    QList<QByteArray> includePaths() const;

    void setPreprocess(bool pre);
    void setLanguage(Language language);
    bool setReplaceInput(const QByteArray& input = QByteArray());
    void setReplaceOutput(const QByteArray& output = QByteArray());

    QByteArray compiler() const;
    QList<QByteArray> input() const;
    QByteArray firstInput() const;
    QByteArray output() const;

    bool hasInput() const;
    bool hasOutput() const;

    bool isCompile() const;

private:
    class Data : public QSharedData
    {
    public:
        Data();

        void guessLanguage();
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
        QByteArray dirPath;
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
