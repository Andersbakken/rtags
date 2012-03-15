#ifndef ADDMESSAGE_H
#define ADDMESSAGE_H

#include <QObject>
#include <QList>
#include <QByteArray>
#include "Message.h"
#include "RTags.h"

class AddMessage : public Message
{
    Q_OBJECT
public:
    enum { MessageId = 1 };

    Q_INVOKABLE AddMessage(QObject* parent = 0);
    AddMessage(RTags::UnitType type, const QByteArray& input, const QByteArray& output,
               const QList<QByteArray>& arguments, const QList<QByteArray>& pchs,
               QObject* parent = 0);

    int messageId() const { return MessageId; }

    RTags::UnitType type() const { return m_type; }
    QByteArray inputFile() const { return m_input; }
    QByteArray outputFile() const { return m_output; }
    QList<QByteArray> arguments() const { return m_args; }
    QList<QByteArray> pchs() const { return m_pchs; }

    QByteArray toByteArray() const;
    Q_INVOKABLE void fromByteArray(const QByteArray& data);

private:
    RTags::UnitType m_type;
    QByteArray m_input, m_output;
    QList<QByteArray> m_args, m_pchs;
};

#endif // ADDMESSAGE_H
