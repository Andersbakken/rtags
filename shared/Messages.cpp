#include "Messages.h"
#include "Connection.h"

template<typename T>
static void reg()
{
    if (!Connection::registerMessage<T>())
        qFatal("Unable to register %s", T::staticMetaObject.className());
}

Messages::Messages()
{
}

void Messages::init()
{
    reg<AddMessage>();
    reg<QueryMessage>();
    reg<ErrorMessage>();
}
