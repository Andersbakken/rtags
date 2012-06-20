#ifndef MESSAGE_H
#define MESSAGE_H

#include <QObject>

class Message : public QObject
{
    Q_OBJECT
public:
    Message(QObject *parent = 0)
        : QObject(parent)
    {}

    virtual int messageId() const = 0;
};

#endif // MESSAGE_H
