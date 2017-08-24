#include <cstddef>
#include <memory>

#include "export/sub1.hpp"
#include "defs.hpp"

static const std::size_t HSIZE = 0x80000 * sizeof(std::size_t);
static const std::size_t THREE = 3;

namespace SUB1MOD {

    int Csub1::sub1(int arg) // rc --references sub1( => ../main/main.cpp:    int v=SUB1MOD::Csub1::@sub1(1);
    {
        try {
            std::unique_ptr<char> heapData(new char[HSIZE]);
        } catch(...) {
            return 0;
        }

        return arg*TWO*THREE; // rc --follow-location arg => sub1.cpp:    int Csub1::sub1(int @arg)
    }

    SUB1_EXPORT_FCN int fcn1(int p1)
    {
        return p1*TWO;
    }


}


// [eof] sub1.cpp
