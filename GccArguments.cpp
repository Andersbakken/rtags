#include "GccArguments.h"
#include "gccopts_gperf.cpp"

GccArguments::Data::Data()
    : input(-1), output(-1), x(-1), language(LangUndefined)
{
}

void GccArguments::Data::guessLanguage()
{
    Q_ASSERT(language == LangUndefined);

    if (x != -1) {
        const QByteArray xarg = args.at(x).arg;
        if (xarg == "c")
            language = LangC;
        else if (xarg == "c++")
            language = LangCPlusPlus;
        else if (xarg == "objective-c")
            language = LangObjC;
        else if (xarg == "objective-c++")
            language = LangObjCPlusPlus;
        return;
    }

    if (input == -1)
        return;

    const QByteArray inputfile = args.at(input).arg;
    const int lastdot = inputfile.lastIndexOf('.');
    if (lastdot == -1)
        return;

    const QByteArray ext = inputfile.mid(lastdot);
    if (ext == ".c")
        language = LangC;
    else if (ext == ".cpp" || ext == ".cc" || ext == ".cxx")
        language = LangCPlusPlus;
    else if (ext == ".m")
        language = LangObjC;
    else if (ext == ".mm")
        language = LangObjCPlusPlus;
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

bool GccArguments::parse(const QList<QByteArray>& args)
{
    const int argc = args.size();
    Data* data = m_ptr.data();

    data->args.clear();
    data->input = data->output = -1;

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
            } else
                data->args.append(Data::Argument(argpos, a));
        } else { // input file?
            if (data->input == -1)
                data->input = argpos;
            else {
                data->error = QString("Multiple input arguments found ('%1' and '%2')")
                          .arg(QString::fromLocal8Bit(data->args.at(data->input).arg.constData()))
                          .arg(QString::fromLocal8Bit(a.constData()));
                return false;
            }
            data->args.append(Data::Argument(argpos, a));
        }
    }

    return true;
}

QString GccArguments::errorString() const
{
    return m_ptr->error;
}

QList<QByteArray> GccArguments::arguments() const
{
    return arguments(QByteArray());
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

        if (arg.pos == data->input && !data->inputreplace.isEmpty())
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
    if (data->input == -1 && !data->inputreplace.isEmpty())
        args << data->inputreplace;

    return args;
}

bool GccArguments::setReplaceInput(const QByteArray &input)
{
    if (input == "-" && m_ptr->language == LangUndefined) {
        m_ptr->guessLanguage();
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

QByteArray GccArguments::input() const
{
    const Data* data = m_ptr.constData();

    if (!data->inputreplace.isEmpty())
        return data->inputreplace;
    else if (data->input == -1)
        return QByteArray();
    return data->args.at(data->input).arg;
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

bool GccArguments::hasInput() const
{
    return (m_ptr->input != -1);
}

bool GccArguments::hasOutput() const
{
    return (m_ptr->output != -1);
}

QDataStream& operator<<(QDataStream& stream, const GccArguments& args)
{
    const GccArguments::Data* data = args.m_ptr.constData();

    stream << data->input << data->output << data->error
           << data->inputreplace << data->outputreplace
           << data->args.size();
    foreach(const GccArguments::Data::Argument& arg, data->args) {
        stream << arg.pos << arg.arg << arg.value;
    }

    return stream;
}

QDataStream& operator>>(QDataStream& stream, GccArguments& args)
{
    GccArguments::Data* data = args.m_ptr.data();

    int argsize;
    stream >> data->input >> data->output >> data->error
           >> data->inputreplace >> data->outputreplace
           >> argsize;
    int pos;
    QByteArray arg, value;
    for (int i = 0; i < argsize; ++i) {
        stream >> pos >> arg >> value;
        data->args.append(GccArguments::Data::Argument(pos, arg, value));
    }

    return stream;
}
