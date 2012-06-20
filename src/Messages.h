#ifndef MESSAGES_H
#define MESSAGES_H

#include "QueryMessage.h"
#include "MakefileMessage.h"
#include "ErrorMessage.h"
#include "ResponseMessage.h"
#include "OutputMessage.h"

class Messages
{
public:
    static void init();

private:
    Messages();
};

#endif // MESSAGES_H
