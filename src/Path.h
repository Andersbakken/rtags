#ifndef Path_h
#define Path_h

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>
#include "Set.h"
#include "String.h"

class Path : public String
{
public:
    Path(const Path &other)
        : String(other)
    {}
    Path(const String &other)
        : String(other)
    {}
    Path(const char *path)
        : String(path)
    {}
    Path(const char *path, int size)
        : String(path, size)
    {}
    Path() {}
    Path &operator=(const Path &other)
    {
        String::operator=(other);
        return *this;
    }
    Path &operator=(const String &other)
    {
        String::operator=(other);
        return *this;
    }

    Path &operator=(const char *path)
    {
        String::operator=(path);
        return *this;
    }

    bool operator==(const Path &other) const
    {
        return Path::resolved(*this).String::operator==(Path::resolved(other));
    }

    bool operator!=(const Path &other) const
    {
        return !Path::operator==(other);
    }

    enum Type {
        Invalid = 0x00,
        File = 0x01,
        Directory = 0x02,
        CharacterDevice = 0x04,
        BlockDevice = 0x08,
        NamedPipe = 0x10,
        SymLink = 0x20,
        Socket = 0x40,
        All = File|Directory|CharacterDevice|BlockDevice|NamedPipe|SymLink|Socket
    };

    inline bool exists() const { return type() != Invalid; }
    inline bool isDir() const { return type() == Directory; }
    inline bool isFile() const { return type() == File; }
    inline bool isSocket() const { return type() == Socket; }
    inline bool isAbsolute() const { return (!isEmpty() && at(0) == '/'); }
    inline bool isSymLink() const { return type() == SymLink; }
    Path followLink(bool *ok = 0) const;
    const char *fileName(int *len = 0) const;
    const char *extension() const;
    static bool exists(const Path &path) { return path.exists(); }
    static bool mkdir(const Path &path);
    static bool rm(const Path &file);
    static Path home();
    bool mksubdir(const String &subdir) const;
    bool isSource() const;
    static bool isSource(const char *extension);
    bool isSystem() const { return Path::isSystem(constData()); }
    static bool isSystem(const char *path);
    bool isHeader() const;
    static bool isHeader(const char *extension);
    Path parentDir() const;
    Type type() const;
    mode_t mode() const;
    bool resolve(const Path &cwd = Path());
    int canonicalize();
    time_t lastModified() const; // returns time_t ... no shit
    int64_t fileSize() const;
    static Path resolved(const String &path, const Path &cwd = Path(), bool *ok = 0);
    static Path canonicalized(const String &path);
    static Path pwd();
    int readAll(char *&, int max = -1) const;
    Path toTilde() const;

    enum VisitResult {
        Abort,
        Continue,
        Recurse
    };
    typedef VisitResult (*VisitCallback)(const Path &path, void *userData);
    void visit(VisitCallback callback, void *userData) const;

    List<Path> files(unsigned filter = All, int max = -1) const;
};

#endif
