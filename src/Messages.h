#ifndef MESSAGES_H
#define MESSAGES_H

#include "QueryMessage.h"
#include "MakefileMessage.h"
#include "ErrorMessage.h"
#include "ResponseMessage.h"
#include "CreateOutputMessage.h"

class Messages
{
public:
    static void init();
    static Message *create(const char *data, int size);
};

#endif // MESSAGES_H
