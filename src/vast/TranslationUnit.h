#ifndef TRANSLATIONUNIT_H
#define TRANSLATIONUNIT_H

#include <memory>
#include <clang-c/Index.h>

class Node;
class TranslationUnit
{
public:
    ~TranslationUnit();
    enum Flag {
        None = 0,
        ShowDefines = 1 << 1,
        ShowIncludes = 1 << 2,
        ShowTypedefs = 1 << 3
    };

    unsigned int flags() const;
    void setFlags(unsigned int flags);

    static std::unique_ptr<TranslationUnit> create(char **argv, int argc, unsigned int flags);
    Node *root() { return mRoot; }
private:
    TranslationUnit() = default;
    Node *mRoot { nullptr };
    CXTranslationUnit mUnit { nullptr };
    CXIndex mIndex { nullptr };
    unsigned int mFlags { 0 };
};


#endif /* TRANSLATIONUNIT_H */
