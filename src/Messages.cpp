#include "Messages.h"
#include "ReadWriteLock.h"
#include "ReadLocker.h"
#include "WriteLocker.h"
#include <assert.h>

class MessageCreatorBase
{
public:
    virtual ~MessageCreatorBase() {}
    virtual Message *create(const char *data, int size) = 0;
};

static Map<int, MessageCreatorBase *> sFactory;
static ReadWriteLock sLock;

template <typename T>
class MessageCreator : public MessageCreatorBase
{
public:
    virtual Message *create(const char *data, int size)
    {
        T *t = new T;
        t->fromData(data, size);
        return t;
    }
};


template<typename T>
static void registerMessage()
{
    const int id = T::MessageId;
    WriteLocker lock(&sLock);
    MessageCreatorBase *&base = sFactory[id];
    if (!base)
        base = new MessageCreator<T>();
}

void Messages::init()
{
    registerMessage<QueryMessage>();
    registerMessage<ErrorMessage>();
    registerMessage<ResponseMessage>();
    registerMessage<CreateOutputMessage>();
    registerMessage<ProjectMessage>();
}

Message * Messages::create(const char *data, int size)
{
    if (size < static_cast<int>(sizeof(int))) {
        error("Can't create message from data (%d)", size);
        return 0;
    }
    Deserializer ds(data, sizeof(int));
    int id;
    ds >> id;
    size -= sizeof(int);
    data += sizeof(int);
    ReadLocker lock(&sLock);
    MessageCreatorBase *base = sFactory.value(id);
    if (!base) {
        error("Can't create message from data id: %d, data: %d bytes", id, size);
        return 0;
    }
    Message *message = base->create(data, size);
    if (!message) {
        error("Can't create message from data id: %d, data: %d bytes", id, size);
        return 0;
    }
    return message;
}
void Messages::cleanup()
{
    WriteLocker lock(&sLock);
    for (Map<int, MessageCreatorBase *>::const_iterator it = sFactory.begin(); it != sFactory.end(); ++it)
        delete it->second;
    sFactory.clear();
}
