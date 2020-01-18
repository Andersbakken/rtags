#include <cstddef>
#include <memory>

#include "export/sub1.hpp"
#include "defs.hpp"

static const std::size_t HSIZE = 0x80000 * sizeof(std::size_t);
static const std::size_t THREE = 3;

namespace SUB1MOD {

    int Csub1::sub1(int arg)
    {
        try {
            std::unique_ptr<char> heapData(new char[HSIZE]);
        } catch(...) {
            return 0;
        }

        return arg*TWO*THREE;
    }

    SUB1_EXPORT_FCN int fcn1(int p1)
    {
        return p1*TWO;
    }


}
