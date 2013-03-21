#ifndef CompilerManager_h
#define CompilerManager_h

#include <rct/Map.h>
#include <rct/List.h>
#include <rct/Path.h>
#include <rct/String.h>

namespace CompilerManager
{
List<Path> compilers();
List<String> flags(const Path &compiler);
}

#endif
