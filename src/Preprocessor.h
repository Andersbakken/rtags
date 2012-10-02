#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include "Path.h"
#include "List.h"
#include "ByteArray.h"
#include "EventReceiver.h"
#include "RTags.h"

class Connection;
class Process;
class Preprocessor : public EventReceiver
{
public:
    Preprocessor(const CompileArgs &args, Connection *connection);
    ~Preprocessor();

    void preprocess();

private:
    void onProcessFinished();

private:
    CompileArgs mArgs;
    Connection *mConnection;

    Process *mProc;
};

#endif
