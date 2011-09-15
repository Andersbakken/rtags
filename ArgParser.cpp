#include "ArgParser.h"
#include "Path.h"

ArgParser::ArgParser(int argc, char **argv)
{
    m_valid = parse(argc, argv);
}

bool ArgParser::isValid() const
{
    return m_valid;
}

QHash<QByteArray, QVariant> ArgParser::dashArguments() const
{
    return m_dash;
}

QList<QByteArray> ArgParser::freeArguments() const
{
    return m_free;
}

void ArgParser::addValue(const QByteArray &key, const QByteArray &value)
{
    if (key == "command") { // don't want to resolve makefile to /some/path/Makefile on mac
        m_dash[key] = value;
        return;
    }
    bool ok;
    int intvalue = value.toInt(&ok);
    if (ok) {
        m_dash[key] = intvalue;
        return;
    }
    double doublevalue = value.toDouble(&ok);
    if (ok) {
        m_dash[key] = doublevalue;
        return;
    }

    Path copy = value;
    if (!copy.isResolved() && copy.resolve()) {
        m_dash[key] = copy;
    } else {
        m_dash[key] = value;
    }
}

bool ArgParser::parse(int argc, char **argv)
{
    Q_ASSERT(argc > 0);
    ++argv; // skip the application name

    m_dash.clear();
    m_free.clear();

    QByteArray current;
    const char** end = const_cast<const char**>(argv + argc);
    for (; argv != end; ++argv) {
        current = *argv;
        if (current.startsWith('-')) {
            const int eqpos = current.indexOf('=');
            if (eqpos == -1) { // no '=' present, just add the argument
                while (!current.isEmpty() && current.at(0) == '-')
                    current = current.mid(1);
                if (current.isEmpty())
                    return false;
                m_dash[current] = QVariant();
            } else { // use everything past '='
                QByteArray value = current.mid(eqpos + 1);
                current = current.left(eqpos);
                while (!current.isEmpty() && current.at(0) == '-')
                    current = current.mid(1);
                if (value.isEmpty() || current.isEmpty())
                    return false;
                addValue(current, value);
            }
        } else { // doesn't start with a '-', add as a free argument
            if (!current.isEmpty()) {
                Path copy = current;
                if (!copy.isResolved() && copy.resolve()) {
                    m_free.append(copy);
                } else {
                    m_free.append(current);
                }
            }
        }
    }
    return true;
}
