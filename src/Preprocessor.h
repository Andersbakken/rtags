#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "Path.h"
#include "List.h"
#include "ByteArray.h"
#include "EventReceiver.h"

class Connection;
class Process;

class Preprocessor : public EventReceiver
{
public:
    Preprocessor(const Path &filename, const List<ByteArray> &arguments, Connection *connection);
    ~Preprocessor();

    void preprocess();

private:
    void onProcessReadyRead();
    void onProcessFinished();

private:
    const Path mFilename;
    List<ByteArray> mArguments;
    Connection *mConnection;

    Process *mProc;
    bool mWrittenArguments;
};

#endif
