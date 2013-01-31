#ifndef CompletionMessage_h
#define CompletionMessage_h

#include "ClientMessage.h"
#include <rct/Path.h>
#include <rct/String.h>

class CompletionMessage : public ClientMessage
{
public:
    enum { MessageId = CompletionId };

    enum Flag {
        None = 0x0,
        Stream = 0x1
    };

    CompletionMessage(unsigned flags = 0, const Path &path = Path(), int line = -1, int column = -1, int pos = -1);
    virtual int messageId() const { return MessageId; }

    unsigned flags() const { return mFlags; }
    Path path() const { return mPath; }
    int line() const { return mLine; }
    int column() const { return mColumn; }
    int pos() const { return mPos; }

    void setContents(const String &contents) { mContents = contents; }
    String contents() const { return mContents; }

    String encode() const;
    void fromData(const char *data, int size);

    void setProjects(const List<String> &projects) { mProjects = projects; }
    List<String> projects() const { return mProjects; }
private:
    unsigned mFlags;
    Path mPath;
    int mLine, mColumn, mPos;
    String mContents;
    List<String> mProjects;
};

#endif
