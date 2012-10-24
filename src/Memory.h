#ifndef Memory_h
#define Memory_h

#ifdef __GXX_EXPERIMENTAL_CXX0X__
#include <memory>
using std::shared_ptr;
using std::static_pointer_cast;
using std::weak_ptr;
using std::enable_shared_from_this;
#else
#include <tr1/memory>
using std::tr1::shared_ptr;
using std::tr1::static_pointer_cast;
using std::tr1::weak_ptr;
using std::tr1::enable_shared_from_this;
#endif
#endif
