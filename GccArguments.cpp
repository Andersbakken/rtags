#include "GccArguments.h"
#include "gccopts_gperf.cpp"
#include <QDebug>
#include "Utils.h"

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
        const QByteArray xarg = args.at(x).arg;
        if (xarg == "c")
            guesslang = LangC;
        else if (xarg == "c++")
            guesslang = LangCPlusPlus;
        else if (xarg == "objective-c")
            guesslang = LangObjC;
        else if (xarg == "objective-c++")
            guesslang = LangObjCPlusPlus;
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

    return guesslang;
}

QByteArray GccArguments::Data::languageString() const
{
    switch(language) {
    case LangUndefined:
        return QByteArray();
    case LangC:
        return "c";
    case LangCPlusPlus:
        return "c++";
    case LangObjC:
        return "objective-c";
    case LangObjCPlusPlus:
        return "objective-c++";
    }
    return QByteArray();
}

GccArguments::GccArguments()
    : m_ptr(new Data)
{
}

bool GccArguments::parse(const QByteArray& raw, const Path &path)
{
    Q_ASSERT(path.isResolved() && path.isDir());
    Data* data = m_ptr.data();
    const QList<QByteArray> args = raw.split(' ');
    Q_ASSERT(!args.isEmpty());
    const QByteArray &first = args.first();
    if (!first.contains("gcc") && !first.contains("g++")
        && !first.contains("c++") && !first.contains("cc")) {
        // ### TODO might need to revisit this
        return true; // not a compile line at all, just return without a warning
    }
    data->dir = path;
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
                data->args.append(Data::Argument(argpos, a));
            }
        } else if (!a.isEmpty()) { // input file?
            data->input.append(argpos);
            data->args.append(Data::Argument(argpos, a));
        }
    }

    return true;
}

QString GccArguments::errorString() const
{
    return m_ptr->error;
}

QByteArray GccArguments::raw() const
{
    return m_ptr->raw;
}

Path GccArguments::dir() const
{
    return m_ptr->dir;
}

QList<QByteArray> GccArguments::arguments() const
{
    return arguments(QByteArray());
}

QList<QByteArray> GccArguments::arguments(const QByteArray &prefix) const
{
    const Data* data = m_ptr.constData();
    QList<QByteArray> args;

    int inputpos = (data->input.size() == 1) ? data->input.first() : -1;

    foreach(const Data::Argument& arg, data->args) {
        if (arg.pos == 0) // skip the compiler
            continue;

        if (!prefix.isEmpty() && !arg.arg.startsWith(prefix))
            continue;

        if (arg.pos == inputpos && !data->inputreplace.isEmpty())
            args << data->inputreplace;
        else
            args << arg.arg;

        if (arg.pos == data->output && !data->outputreplace.isEmpty())
            args << data->outputreplace;
        else if (arg.pos == data->x && data->language != LangUndefined)
            args << data->languageString();
        else if (!arg.value.isEmpty())
            args << arg.value;
    }

    if (data->output == -1 && !data->outputreplace.isEmpty())
        args << "-o" << data->outputreplace;
    if (data->x == -1 && data->language != LangUndefined)
        args << "-x" << data->languageString();
    if (data->input.isEmpty() && !data->inputreplace.isEmpty())
        args << data->inputreplace;

    return args;
}

bool GccArguments::setReplaceInput(const QByteArray &input)
{
    if (input == "-" && m_ptr->language == LangUndefined) {
        m_ptr->language = m_ptr->guessLanguage();
        if (m_ptr->language == LangUndefined)
            return false;
    }
    m_ptr->inputreplace = input;
    return true;
}

void GccArguments::setReplaceOutput(const QByteArray &output)
{
    m_ptr->outputreplace = output;
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

    if (!data->inputreplace.isEmpty() && data->input.size() <= 1)
        return QList<Path>() << data->inputreplace;

    QList<Path> ret;
    foreach(int pos, data->input) {
        Path p = data->args.at(pos).arg;
        if (p.resolve(data->dir)) { // ### inconsistent with isCompile which doesn't check this
            ret << p;
        }
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

    if (!data->outputreplace.isEmpty())
        return data->outputreplace;
    else if (data->output == -1)
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
        return true;
    }
    return false;
}

QDataStream& operator<<(QDataStream& stream, const GccArguments& args)
{
    const GccArguments::Data* data = args.m_ptr.constData();

    stream << data->input << data->output << data->x << data->c
           << data->error << qint8(data->language)
           << data->inputreplace << data->outputreplace
           << data->raw << data->dir
           << data->args;
    return stream;
}

QDataStream& operator>>(QDataStream& stream, GccArguments& args)
{
    args.m_ptr.detach();
    GccArguments::Data* data = args.m_ptr.data();

    qint8 lang;
    stream >> data->input >> data->output >> data->x >> data->c
           >> data->error >> lang
           >> data->inputreplace >> data->outputreplace
           >> data->raw >> data->dir >> data->args;
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



QList<QByteArray> GccArguments::includePaths() const
{
    QList<QByteArray> includePaths = arguments("-I");
    const int size = includePaths.size();
    for (int i=0; i<size; ++i) {
        QByteArray &arg = includePaths[i];
#ifndef Q_OS_UNIX
        Q_ASSERT(0 && "This stuff isn't ported to non-unix platforms");
#endif
        if (arg.size() >= 3 && arg.at(2) == '/') { // absolute path already
            continue;
        }
        arg.replace(0, 2, m_ptr->dir + '/');
        Path p(arg);
        if (!p.isResolved())
            p.resolve();
        arg.insert(0, "-I");
    }
    return includePaths;
}
