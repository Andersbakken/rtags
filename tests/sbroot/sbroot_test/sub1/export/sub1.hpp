#ifndef SUB_HPP
#define SUB_HPP

#include "sub1_spec.hpp"

namespace SUB1MOD {

    /**
     * @class Csub1
     * @brief Class containing sub1()
     */
    class SUB1_EXPORT_CLASS Csub1 {
      public:
        static int sub1(int arg);
    };

    SUB1_EXPORT_FCN int fcn1(int p1);

} // SUB1MOD

#endif
