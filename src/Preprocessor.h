#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

#include <rct/Path.h>
#include <rct/List.h>
#include <rct/String.h>
#include "RTags.h"

class Connection;
class Process;
class Preprocessor
{
public:
    Preprocessor(const SourceInformation &args, Connection *connection);
    ~Preprocessor();

    void preprocess();

private:
    void onProcessFinished();

private:
    const SourceInformation mArgs;
    Connection *mConnection;

    Process *mProc;
};

#endif
