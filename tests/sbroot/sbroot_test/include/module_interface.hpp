#ifndef MODULE_INTERFACE_HPP
#define MODULE_INTERFACE_HPP

#if __GNUC__ >= 4
# define DLL_EXPORT_SYM __attribute__ ((visibility("default")))
# define DLL_IMPORT_SYM __attribute__ ((visibility("default")))
#else
# error compiler not supported
#endif

#endif // MODULE_INTERFACE_HPP
