#ifndef CompilerManager_h
#define CompilerManager_h

#include "Map.h"
#include "List.h"
#include "Path.h"
#include "ByteArray.h"

namespace CompilerManager
{
List<ByteArray> flags(const Path &compiler, const Path &cpp = Path());
}

#endif
