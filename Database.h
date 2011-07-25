#ifndef Database_h
#define Database_h

#include <QFileInfo>
#include <QString>
#include <QRegExp>

class Database
{
public:
    Database();
    bool init(const QString &file);

    enum LookupFlags {
        None = 0x000,
        // IncludeStartsWith = 0x001,
        // IncludeEndsWith = 0x002,
        IncludeContains = 0x004
    };

    enum LookupType {
        Definition,
        Declaration,
        Reference
    };
    
    struct Location {
        QString file;
        int line, column;
    };

    struct Result {
        QString symbolName, path;
        LookupType type;
        QList<Location> locations;
    };

    struct Filter {
        QRegExp regExp;
        QString text;
        enum Flag {
            None = 0x000,
            FilterOut = 0x001,
            ExactMatch = 0x002,
            RegExp = 0x004
        };
    };

    Result lookup(const QString &symbolName, LookupType type, unsigned flags,
                  const QList<Filter> &filters = QList<Filter>());
    void remove(const QFileInfo &file);
private:
    QString m_fileName;
};

#endif
