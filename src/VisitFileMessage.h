#ifndef VisitFileMessage_h
#define VisitFileMessage_h

#include <rct/Message.h>
#include <rct/String.h>
#include "ClientMessage.h"

class VisitFileMessage : public ClientMessage
{
public:
    enum { MessageId = VisitFileId };

    VisitFileMessage(uint32_t fileId, bool visit)
        : ClientMessage(MessageId), mFileId(fileId), mVisit(visit)
    {
    }

    uint32_t fileId() const { return mFileId; }
    bool visit() const { return mVisit; }

    void encode(Serializer &serializer) const { serializer << mFileId << mVisit; }
    void decode(Deserializer &deserializer) { deserializer >> mFileId >> mVisit; }
private:
    uint32_t mFileId;
    bool mVisit;
};

#endif
