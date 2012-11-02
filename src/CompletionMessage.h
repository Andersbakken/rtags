#ifndef CompletionMessage_h
#define CompletionMessage_h

#include "ClientMessage.h"
#include "Path.h"
#include "ByteArray.h"

class CompletionMessage : public ClientMessage
{
public:
    enum { MessageId = CompletionId };

    CompletionMessage(const Path &path = Path(), int line = -1, int column = -1);
    virtual int messageId() const { return MessageId; }

    Path path() const { return mPath; }
    int line() const { return mLine; }
    int column() const { return mColumn; }

    void setContents(const ByteArray &contents) { mContents = contents; }
    ByteArray contents() const { return mContents; }

    ByteArray encode() const;
    void fromData(const char *data, int size);

    void setProjects(const List<ByteArray> &projects) { mProjects = projects; }
    List<ByteArray> projects() const { return mProjects; }
private:
    Path mPath;
    int mLine, mColumn;
    ByteArray mContents;
    List<ByteArray> mProjects;
};

#endif
