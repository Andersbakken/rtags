#ifndef MESSAGES_H
#define MESSAGES_H

#include "messages/AddMessage.h"
#include "messages/QueryMessage.h"
#include "messages/ErrorMessage.h"

class Messages
{
public:
    static void init();

private:
    Messages();
};

#endif // MESSAGES_H
