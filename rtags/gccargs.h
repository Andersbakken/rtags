#ifndef ARGS_H
#define ARGS_H

#include <QHash>
#include <QByteArray>
#include <QDataStream>
#include <QSharedData>
#include <QSharedDataPointer>

class GccArguments
{
public:
    enum Language { LangUndefined, LangC, LangCPlusPlus, LangObjC, LangObjCPlusPlus };

    GccArguments();

    bool parse(const QList<QByteArray>& args);
    QString errorString() const;

    QList<QByteArray> arguments() const;
    QList<QByteArray> arguments(const QByteArray& prefix) const;

    void setPreprocess(bool pre);
    void setLanguage(Language language);
    bool setReplaceInput(const QByteArray& input = QByteArray());
    void setReplaceOutput(const QByteArray& output = QByteArray());

    QByteArray compiler() const;
    QByteArray input() const;
    QByteArray output() const;

    bool hasInput() const;
    bool hasOutput() const;

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

        int input;
        int output;
        int x;
        QList<Argument> args;
        QString error;
        Language language;

        QByteArray inputreplace;
        QByteArray outputreplace;
    };

    QSharedDataPointer<Data> m_ptr;

    friend QDataStream& operator<<(QDataStream& stream, const GccArguments& args);
    friend QDataStream& operator>>(QDataStream& stream, GccArguments& args);
};

QDataStream& operator<<(QDataStream& stream, const GccArguments& args);
QDataStream& operator>>(QDataStream& stream, GccArguments& args);

#endif
