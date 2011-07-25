#include "Utils.h"

#ifdef DEBUG_FUNCTION_CALLS
int Timer::s_indent = 0;
QMutex Timer::s_mutex;
#endif
bool Options::s_verbose = false;
