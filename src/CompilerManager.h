#ifndef CompilerManager_h
#define CompilerManager_h

#include "Map.h"
#include "List.h"
#include "Path.h"
#include "String.h"

namespace CompilerManager
{
List<String> flags(const Path &compiler);
}

#endif
