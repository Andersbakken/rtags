#include <string>
#include <stdio.h>
#include <string.h>
#include <vector>
#include <stdint.h>
#include "dtl.hpp"

uint32_t translatePoint(uint32_t offset, const dtl::Diff<char, std::string> &diff)
{
    const uint32_t old = offset;
    const dtl::Ses<char> ses = diff.getSes();
    const std::vector<std::pair<char, dtl::elemInfo> > seq = ses.getSequence();
    int i = 0;
    for (std::vector<std::pair<char, dtl::elemInfo> >::const_iterator it = seq.begin(); it != seq.end(); ++it) {
        if (it->second.type == 1) {
            if (it->second.afterIdx >= old || !offset) {
                // printf("0 Breaking at(%c) because %d %lld\n", it->first, offset, it->second.afterIdx);
                break;
            }
            --offset;
            // printf("Reducing offset to %d for character %lld %lld %c\n", offset,
            //        it->second.afterIdx, it->second.beforeIdx, it->first);
        } else if (it->second.type == -1) {
            if (it->second.beforeIdx >= offset) {
                // printf("1 Breaking at(%c) because %d %lld\n", it->first, offset, it->second.beforeIdx);
                break;
            }
            ++offset;
            // printf("Increasing offset to %d for character %lld %lld %c\n", offset,
            //        it->second.afterIdx, it->second.beforeIdx, it->first);
        } else if (it->second.afterIdx > old) {
            // printf("Breaking at %c because of %lld/%d\n", it->first, it->second.afterIdx, offset);
            break;
        }

        // printf("%d: %c %d %d %d\n", i++, it->first, it->second.type, it->second

        // printf("%c %lld %lld %d\n", it->first,
        //        it->second.beforeIdx,
        //        it->second.afterIdx,
        //        it->second.type);
    }
    // printf("%d => %d\n", old, offset);
    return offset;
}

int main(int argc, char **argv)
{
    std::string a, b;
    bool string = false;
    uint32_t offset = 10;
    for (int i=1; i<argc; ++i) {
        if (!strcmp(argv[i], "--string") || !strcmp(argv[i], "-s")) {
            string = true;
        } else if (a.empty()) {
            a = argv[i];
        } else if (b.empty()) {
            b = argv[i];
        } else {
            offset = atoi(argv[i]);
        }
    }
    if (a.empty() || b.empty()) {
        fprintf(stderr, "Not enough data\n");
        return 1;
    }

    if (!string) {
        std::string *files[] = { &a, &b, 0 };
        for (int i=0; files[i]; ++i) {
            FILE *f = fopen(files[i]->c_str(), "r");
            if (!f) {
                fprintf(stderr, "Can't open %s for reading\n", files[i]->c_str());
                return 1;
            }
            files[i]->clear();
            char buf[1024];
            while (true) {
                const int r = fread(buf, sizeof(char), sizeof(buf), f);
                if (r <= 0)
                    break;
                files[i]->append(buf, r);
            }
            fclose(f);
        }
    }

    dtl::Diff<char, std::string> diff(a, b);
    diff.compose();
    diff.composeUnifiedHunks();
    // diff.printSES(std::cout);
    dtl::Diff<char, std::string>::printUnifiedFormat(diff.getUniHunks());
    const uint32_t translated = translatePoint(offset, diff);
    printf("%d => %d : '%c' => '%c'\n", offset, translated, b.at(offset), a.at(translated));

    return 0;
}

