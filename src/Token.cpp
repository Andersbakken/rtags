#include "Token.h"

String Token::toString() const
{
    String ret;
    {
        Log log(&ret);
        log << "Location:" << location
            << "Offset:" << offset
            << "Length:" << length
            << "Kind:" << kind
            << "\nSpelling:";
        if (spelling.contains('\n')) {
            for (const String &line : spelling.split('\n')) {
                log << "\n " << line;
            }
        } else {
            log << "\n " << spelling;
        }
    }
    return ret;

}
