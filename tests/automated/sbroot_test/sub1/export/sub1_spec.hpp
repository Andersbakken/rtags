#ifndef SUB1_SPEC_HPP
#define SUB1_SPEC_HPP

#include "module_interface.hpp"

#ifdef BUILDING_SUB1
# define SUB1_EXPORT_FCN   DLL_EXPORT_SYM
# define SUB1_EXPORT_CLASS DLL_EXPORT_SYM
#else
# define SUB1_EXPORT_FCN   DLL_IMPORT_SYM
# define SUB1_EXPORT_CLASS DLL_IMPORT_SYM
#endif

#endif // SUB1_SPEC_HPP
