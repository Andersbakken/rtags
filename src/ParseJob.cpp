#include "ParseJob.h"

ParseJob::ParseJob(const QueryMessage &query)
    : Job(query, WriteUnfiltered), mPath(query.query())
{
}

struct Word
{
    Word(const char *start = 0, int length = 0)
        : ch(start), len(length)
    {}
    const char *ch;
    const int len;
    ByteArray toByteArray() const { return ByteArray(ch, len); }
};

static inline Word nextWord(char *&c)
{
    switch (*c) {
    case '(':
    case ')':
        return Word(c++, 1);
    case '/':
        switch (*(c + 1)) {
        case '/':
        case '*':
        default:
            break;
        }
    }
    while (*c && !isalnum(*c))
        ++c;
    if (!*c)
        return Word();
    const char *start = c;
    assert(isalnum(*c));
    ++c;
    while (isalnum(*c))
        ++c;
    if (!strncmp(c, "::", 2) && isalnum(*(c + 2))) {
        c += 3;
        while (isalnum(*c))
            ++c;
    }
    return Word(start, c - start);
}
void ParseJob::run()
{
    FILE *f = fopen(mPath.constData(), "r");
    if (!f) {
        error() << "Can't open" << mPath;
        return;
    }
    const int fileSize = mPath.fileSize();
    char *buf = reinterpret_cast<char*>(calloc(fileSize + 1, 1));
    fread(buf, 1, fileSize, f);
    fclose(f);
    char *ch = buf;
    char *end;
    while (*ch) {
        switch (*ch) {
        case '/':
            switch (*ch + 1) {
            case '/':
                if ((end = strchr(ch, '\n'))) {
                    memset(ch, ' ', end - ch);
                    ch = end + 1;
                } else {
                    ch = 0;
                }
                break;
            case '*':
                if ((end = strstr(ch, "*/"))) {
                    memset(ch, ' ', end - ch);
                    ch = end + 1;
                } else {
                    ch = 0;
                }
                break;
            default:
                break;
            }
        case '\'':
            for (int i=0; i<3; ++i) {
            }

            // while (++ch
        case '"':
            break;
        }
    }
    while (true) {
        const Word word = nextWord(ch);
        if (!word.ch)
            break;
        printf("%s\n", word.toByteArray().nullTerminated());
        write(word.toByteArray());
    }

    free(buf);
}
