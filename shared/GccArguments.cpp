#include "GccArguments.h"
#ifdef WITH_GPERF
#include "gccopts_gperf.cpp"
#endif
#include <QDebug>
// #include "Utils.h"

GccArguments::Data::Data()
    : output(-1), x(-1), c(-1), language(LangUndefined)
{
}

GccArguments::Language GccArguments::Data::guessLanguage() const
{
    if (language != LangUndefined)
        return language;

    Language guesslang = LangUndefined;
    if (x != -1) {
        const QByteArray xarg = args.at(x).value;
        if (xarg == "c")
            guesslang = LangC;
        else if (xarg == "c++")
            guesslang = LangCPlusPlus;
        else if (xarg == "objective-c")
            guesslang = LangObjC;
        else if (xarg == "objective-c++")
            guesslang = LangObjCPlusPlus;
        else if (xarg == "c++-header")
            guesslang = LangCPlusPlusHeader;
        else if (xarg == "c-header")
            guesslang = LangHeader;
        return guesslang;
    }

    if (input.size() != 1)
        return guesslang;

    const QByteArray inputfile = args.at(input.first()).arg;
    const int lastdot = inputfile.lastIndexOf('.');
    if (lastdot == -1)
        return guesslang;

    const QByteArray ext = inputfile.mid(lastdot);
    if (ext == ".c")
        guesslang = LangC;
    else if (ext == ".cpp" || ext == ".cc" || ext == ".cxx")
        guesslang = LangCPlusPlus;
    else if (ext == ".m")
        guesslang = LangObjC;
    else if (ext == ".mm")
        guesslang = LangObjCPlusPlus;
    else if (ext == ".hpp" || ext == ".hxx")
        guesslang = LangCPlusPlusHeader;
    else if (ext == ".h")
        guesslang = LangHeader;

    return guesslang;
}

GccArguments::GccArguments()
    : m_ptr(new Data)
{
}

Path GccArguments::parseCD(const QByteArray &cmd, const Path& path) const
{
    const QList<QByteArray> args = cmd.split(' ');
    Q_ASSERT(args.size() > 1);
    const QByteArray& argspath = args.at(1);
    Q_ASSERT(!argspath.isEmpty());
    Path cmdpath(argspath);
    if (argspath.at(1) != '/')
        cmdpath.resolve(path);
    return cmdpath;
}

#ifdef WITH_GPERF
bool GccArguments::parse(const QByteArray& cmd, const Path &p)
{
    Q_ASSERT(p.isResolved() && p.isDir());
    Data* data = m_ptr.data();
    Path path = p;

    QByteArray raw, subcmd;

    // ### should support quoted paths with spaces in them

    // check for multiple commands on the same line
    int semipos = cmd.indexOf(';');
    int amppos = cmd.indexOf("&&");
    int cmdpos = (semipos != -1 && semipos < amppos) ? semipos : amppos, prevpos = 0;
    int sublen = (cmdpos == semipos) ? 1 : 2;
    if (cmdpos != -1) {
        while (prevpos != -1) {
            subcmd = cmd.mid(prevpos, (cmdpos == -1) ? -1 : cmdpos - prevpos).trimmed();
            const int firstSpace = subcmd.indexOf(' ');
            QByteArray first;
            if (firstSpace != -1) {
                first = subcmd.left(firstSpace);
            } else {
                first = subcmd;
            }

            if (first.contains("gcc") || first.contains("g++")
                || first.contains("c++") || first.contains("cc")) {
                if (!raw.isEmpty())
                    qDebug() << cmd << path << raw;
                Q_ASSERT(raw.isEmpty());
                raw = subcmd;
            } else {
                if (subcmd.startsWith("cd "))
                    path = parseCD(subcmd, path);
            }
            prevpos = (cmdpos != -1) ? cmdpos + sublen : -1;
            semipos = cmd.indexOf(';', cmdpos + 1);
            amppos = cmd.indexOf("&&", cmdpos + 1);
            cmdpos = (semipos != -1 && semipos < amppos) ? semipos : amppos;
            sublen = (cmdpos == semipos) ? 1 : 2;
        }
    } else {
        raw = cmd;
    }

    const QList<QByteArray> args = raw.split(' ');
    Q_ASSERT(!args.isEmpty());

    const QByteArray &first = args.first();
    if (!first.contains("gcc") && !first.contains("g++")
        && !first.contains("c++") && !first.contains("cc")) {
        // ### TODO might need to revisit this
        return true; // not a compile line at all, just return without a warning
    }
    data->raw = raw;

    const int argc = args.size();

    data->args.clear();
    data->input.clear();
    data->output = -1;
    data->x = data->c = -1;

    gccopts_gperf gccopts;

    data->args.append(Data::Argument(0, args.first()));
    QByteArray a;
    for (int i = 1, argpos; i < argc; ++i) {
        argpos = data->args.size();
        a = args.at(i);
        if (a.startsWith('-')) { // option
            if (gccopts.in_word_set(a.data(), a.size()) && i + 1 < argc) {
                if (a == "-o") {
                    if (data->output == -1) {
                        data->output = argpos;
                    } else {
                        data->error = QString("Multiple output arguments found ('%1' and '%2')")
                            .arg(QString::fromLocal8Bit(data->args.at(data->output).value.constData()))
                            .arg(args.at(i + 1).constData());
                        return false;
                    }
                } else if (a == "-x") {
                    if (data->x == -1)
                        data->x = argpos;
                    else {
                        data->error = QString("Multiple language arguments found ('%1' and '%2')")
                            .arg(QString::fromLocal8Bit(data->args.at(data->x).value.constData()))
                            .arg(args.at(i + 1).constData());
                        return false;
                    }
                }
                data->args.append(Data::Argument(argpos, a, args.at(++i)));
            } else {
                if (a == "-c")
                    data->c = argpos;
                if (a.startsWith("-I")) {
                    a = Path::resolved(a.mid(2), path);
                    a.prepend("-I");
                }
                data->args.append(Data::Argument(argpos, a));
            }
        } else if (!a.isEmpty()) { // input file?
            Path p = Path::resolved(a, path);
            if (p.isFile()) {
                // qDebug() << a << path << Path::resolved(a, path);
                data->input.append(argpos);
                data->args.append(Data::Argument(argpos, Path::resolved(a, path)));
            } else {
                qDebug() << "couldn't resolve" << a << path;
            }
        }
    }

    return true;
}
#endif

QString GccArguments::errorString() const
{
    return m_ptr->error;
}

QByteArray GccArguments::raw() const
{
    return m_ptr->raw;
}

QList<QByteArray> GccArguments::arguments() const
{
    return arguments(QByteArray());
}

int GccArguments::argumentCount() const
{
    return qMax(0, m_ptr->args.size() - 1);
}

QList<QByteArray> GccArguments::arguments(const QByteArray &prefix) const
{
    const Data* data = m_ptr.constData();
    QList<QByteArray> args;

    foreach(const Data::Argument& arg, data->args) {
        if (arg.pos == 0) // skip the compiler
            continue;

        if (!prefix.isEmpty() && !arg.arg.startsWith(prefix))
            continue;

        args << arg.arg;

        if (arg.pos == data->x && data->language != LangUndefined)
            args << languageString(data->language);
        else if (!arg.value.isEmpty())
            args << arg.value;
    }

    if (data->x == -1 && data->language != LangUndefined)
        args << "-x" << languageString(data->language);

    return args;
}

QByteArray GccArguments::compiler() const
{
    const Data* data = m_ptr.constData();

    if (data->args.isEmpty())
        return QByteArray();
    return data->args.at(0).arg;
}

QList<Path> GccArguments::input() const
{
    const Data* data = m_ptr.constData();

    QList<Path> ret;
    foreach (int pos, data->input) {
        const Path p = data->args.at(pos).arg;
        Q_ASSERT(p.isResolved());
        ret << p;
    }
    return ret;
}

QByteArray GccArguments::firstInput() const
{
    return input().value(0);
}

QByteArray GccArguments::output() const
{
    const Data* data = m_ptr.constData();

    if (data->output == -1)
        return QByteArray();
    Q_ASSERT(data->args.at(data->output).arg == "-o");
    return data->args.at(data->output).value;
}

GccArguments::Language GccArguments::language() const
{
    const Data* data = m_ptr.constData();
    if (data->language == LangUndefined)
        return data->guessLanguage();
    return data->language;
}

bool GccArguments::hasInput() const
{
    return !m_ptr->input.isEmpty();
}

bool GccArguments::hasOutput() const
{
    return m_ptr->output != -1;
}

bool GccArguments::isCompile() const
{
    // ### This should perhaps account for gcc commands that both compile and link at once
    if ((m_ptr->c != -1 && m_ptr->output != -1 && m_ptr->input.size() == 1)
        || (m_ptr->c != -1 && m_ptr->output == -1 && !m_ptr->input.isEmpty())) {
        switch (language()) {
        case LangCPlusPlusHeader:
        case LangHeader:
            return false;
        default:
            return true;
        }
    }
    return false;
}

QDataStream& operator<<(QDataStream& stream, const GccArguments& args)
{
    const GccArguments::Data* data = args.m_ptr.constData();

    stream << data->input << data->output << data->x << data->c
           << data->error << int8_t(data->language)
           << data->raw << data->args;
    return stream;
}

QDataStream& operator>>(QDataStream& stream, GccArguments& args)
{
    args.m_ptr.detach();
    GccArguments::Data* data = args.m_ptr.data();

    int8_t lang;
    stream >> data->input >> data->output >> data->x >> data->c
           >> data->error >> lang
           >> data->raw >> data->args;
    data->language = static_cast<GccArguments::Language>(lang);
    return stream;
}

QDataStream& operator<<(QDataStream& stream, const GccArguments::Data::Argument &arg)
{
    stream << arg.pos << arg.arg << arg.value;
    return stream;
}
QDataStream& operator>>(QDataStream& stream, GccArguments::Data::Argument &arg)
{
    stream >> arg.pos >> arg.arg >> arg.value;
    return stream;
}

QList<Path> GccArguments::includePaths() const
{
    QList<Path> ret;
    QList<QByteArray> includePaths = arguments("-I");
    const int size = includePaths.size();
    for (int i=0; i<size; ++i) {
        ret.append(Path(includePaths.at(i).mid(2)));
    }
    return ret;
}
bool GccArguments::isNull() const
{
    return m_ptr->raw.isEmpty();
}
bool GccArguments::isEmpty() const
{
    return m_ptr->raw.isEmpty();
}

bool GccArguments::operator==(const GccArguments &other) const
{
    return m_ptr == other.m_ptr || m_ptr->raw == other.m_ptr->raw;
}

int GccArguments::getClangArgs(const char **args, int max, unsigned flags) const
{
    const Data* data = m_ptr.constData();

    int added = 0;
    const int count = data->args.size();
    for (int i=1; i<count && max > 0; ++i) {
        const Data::Argument& arg = data->args.at(i);
        // ### is i the same as data.argpos?
        if (arg.arg.startsWith("-I")) {
            if (!(flags & IncludePaths))
                continue;
        } else if (arg.arg.startsWith("-D")) {
            if (!(flags & Defines))
                continue;
        } else if (!(flags & OtherArgs)) {
            continue;
        }
        args[added++] = arg.arg.constData();
        --max;
    }

    return added;
}
const char* GccArguments::languageString(Language language)
{
    switch(language) {
    case LangUndefined:
        break;
    case LangC:
        return "c";
    case LangCPlusPlus:
        return "c++";
    case LangObjC:
        return "objective-c";
    case LangObjCPlusPlus:
        return "objective-c++";
    case LangCPlusPlusHeader:
        return "c++-header";
    case LangHeader:
        return "c-header"; // ### ???
    }
    return "";
}
QByteArray GccArguments::key() const
{
    const Data* data = m_ptr.constData();
    if (data->key.isEmpty()) {
        foreach(const QByteArray& entry, arguments("-D")) {
            data->key += entry;
        }
        foreach(const QByteArray& entry, arguments("-I")) {
            data->key += entry;
        }
    }

    return data->key;
}
const char * GccArguments::languageString() const
{
    return GccArguments::languageString(language());
}
void GccArguments::setLanguage(Language language)
{
    m_ptr->language = language;
}
