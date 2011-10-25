#include "RTags.h"

namespace RTags {
QDataStream &operator<<(QDataStream &ds, time_t t)
{
    return (ds << quint64(t));
}

QDataStream &operator>>(QDataStream &ds, time_t &t)
{
    qint64 tmp;
    ds >> tmp;
    t = tmp;
    return ds;
}

bool parseLocation(const std::string &string,
                   std::string &file, unsigned &line, unsigned &col)
{
    file = string;
    size_t colon = file.find_last_of(':');
    if (colon == std::string::npos)
        return false;
    col = atoi(string.c_str() + colon + 1);
    if (!col)
        return false;
    file.resize(colon);
    colon = file.find_last_of(':');
    if (colon == std::string::npos)
        return false;
    line = atoi(string.c_str() + colon + 1);
    if (!line)
        return false;
    file.resize(colon);
    return true;
}

Path findRtagsDb()
{
    char buffer[500];
    if (getcwd(buffer, 500)) {
        char *slash;
        while ((slash = strrchr(buffer, '/'))) {
            // ### this is awful
            struct ::stat s;
            std::string path(buffer);
            path += "/.rtags.db";
            // printf("Testing [%s]\n", path.c_str());
            if (stat(path.c_str(), &s) >= 0)
                return QByteArray(path.c_str(), path.size());
            *slash = '\0';
        }
    }
    return Path();
}
}
