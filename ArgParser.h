#ifndef ArgParser_h
#define ArgParser_h

#include <QtCore>

class ArgParser
{
public:
    ArgParser(int argc, char** argv);

    bool isValid() const;

    QHash<QByteArray, QVariant> dashArguments() const;
    QList<QByteArray> freeArguments() const;

private:
    bool parse(int argc, char** argv);
    void addValue(const QByteArray& key, const QByteArray& value);
private:
    bool m_valid;
    QHash<QByteArray, QVariant> m_dash;
    QList<QByteArray> m_free;
};

#endif
