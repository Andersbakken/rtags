/* This file is part of RTags (http://rtags.net).

   RTags is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   RTags is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with RTags.  If not, see <http://www.gnu.org/licenses/>. */

#ifndef CommandLineParser_h
#define CommandLineParser_h

#include <functional>
#include <initializer_list>
#include <rct/Flags.h>
#include <rct/Hash.h>
#include <rct/List.h>
#include <rct/Path.h>
#include <rct/String.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

namespace CommandLineParser {
enum ValueType {
    Required,
    Optional,
    NoValue
};
template <typename T>
struct Option {
    const T option;
    const String longOpt;
    const char shortOpt;
    ValueType valueType;
    const String description;
};
enum Status {
    Parse_Exec,
    Parse_Ok,
    Parse_Error
};

struct ParseStatus {
    String error;
    Status status;
};
enum Flag {
    NoFlag = 0x0,
    IgnoreUnknown = 0x1
};

enum ConfigOptionType {
    ConfigNone = 0,
    Config,
    NoRc
};

RCT_FLAGS(Flag);
template <typename T>
ParseStatus parse(int argc, char **argv,
                  std::initializer_list<Option<T> > optsList,
                  Flags<Flag> flags,
                  const std::function<ParseStatus(T, String &&value, size_t &idx, const List<String> &args)> &handler,
                  const String &app = String(),
                  std::initializer_list<Option<ConfigOptionType> > configOpts = std::initializer_list<Option<ConfigOptionType> >(),
                  String *cmdLine = 0)
{
    Hash<String, const Option<T> *> longOpts;
    Hash<char, const Option<T> *> shortOpts;
    for (const auto &opt : optsList) {
        if (!opt.longOpt.isEmpty())
            longOpts[opt.longOpt] = &opt;
        if (opt.shortOpt)
            shortOpts[opt.shortOpt] = &opt;
    }
    if (getenv("RTAGS_DUMP_UNUSED")) {
        String unused;
        for (int i=0; i<26; ++i) {
            if (!shortOpts.contains('a' + i))
                unused.append('a' + i);
            if (!shortOpts.contains('A' + i))
                unused.append('A' + i);
        }
        printf("Unused: %s\n", unused.constData());
        for (const auto &opt : optsList) {
            if (!opt.longOpt.isEmpty()) {
                if (!opt.shortOpt) {
                    printf("No shortoption for %s\n", opt.longOpt.constData());
                } else if (opt.longOpt[0] != opt.shortOpt) {
                    printf("Not ideal option for %s|%c\n", opt.longOpt.constData(), opt.shortOpt);
                }
            }
        }
        return { String(), Parse_Ok };
    }

    List<String> args;
    if (configOpts.size() && !app.isEmpty()) {
        bool norc = false;
        Path rcfile = Path::home() + "." + app + "rc";
        if (!rcfile.exists()) {
            const char * configPath = getenv("XDG_CONFIG_HOME");
            rcfile = configPath ? configPath : Path::home() + ".config";
            rcfile += "/rtags/";
            rcfile.mkdir(Path::Recursive);
            rcfile += app + "rc";
        }
        parse<ConfigOptionType>(argc, argv, configOpts,
                                IgnoreUnknown, [&norc, &rcfile](ConfigOptionType type, String &&value, size_t &, const List<String> &) -> ParseStatus {
                                    switch (type) {
                                    case ConfigNone: {
                                        assert(0);
                                        break; }
                                    case Config: {
                                        rcfile = std::move(value);
                                        break; }
                                    case NoRc: {
                                        norc = true;
                                        break; }
                                    }
                                    return { String(), Parse_Exec };
                                });
        if (!norc) {
            args.push_back(argv[0]);
            String rc = Path("/etc/rcrc").readAll();
            if (!rc.isEmpty()) {
                for (const String &s : rc.split('\n')) {
                    if (!s.isEmpty() && !s.startsWith('#'))
                        args += s.split(' ');
                }
            }
            if (!rcfile.isEmpty()) {
                rc = rcfile.readAll();
                if (!rc.isEmpty()) {
                    for (const String& s : rc.split('\n')) {
                        if (!s.isEmpty() && !s.startsWith('#'))
                            args += s.split(' ');
                    }
                }
            }
            for (int i=1; i<argc; ++i)
                args.append(argv[i]);
        }
    }
    if (args.isEmpty()) {
        args.resize(argc);
        for (int i=0; i<argc; ++i) {
            args[i] = argv[i];
        }
    }


    if (cmdLine) {
        bool first = true;
        for (const String &arg : args) {
            if (first) {
                first = false;
            } else {
                cmdLine->append(' ');
            }
            const bool space = arg.contains(' ');
            if (space)
                cmdLine->append('"');
            cmdLine->append(arg);
            if (space)
                cmdLine->append('"');
        }
    }

    ParseStatus status = { String(), Parse_Exec };
    for (size_t i=1; i<args.size(); ++i) {
        const String &arg = args.at(i);
        List<const Option<T> *> opts;
        String value;
        auto addArg = [&arg, &opts, &status, flags](const Option<T> *opt) -> bool {
            if (opt) {
                opts.append(opt);
                return true;
            }
            if (flags & IgnoreUnknown)
                return true;

            status = { String::format<1024>("Couldn't parse argument %s", arg.constData()), Parse_Error };
            return false;
        };
        if (arg == "--") {
            addArg(longOpts.value("--"));
        } else if (arg.startsWith("--")) {
            const size_t eq = arg.indexOf('=');
            String a;
            if (eq == String::npos) {
                a = arg.mid(2);
            } else {
                a = arg.mid(2, eq - 2);
                value = arg.mid(eq + 1);
            }
            addArg(longOpts.value(a));
        } else if (arg.startsWith("-")) {
            for (size_t j=1; j<arg.size(); ++j) {
                if (j > 1 && !opts.isEmpty() && opts.back()->valueType != NoValue) {
                    if (arg.at(j) == '=')
                        ++j;
                    value = arg.mid(j);
                    break;
                }

                addArg(shortOpts.value(arg.at(j)));
            }
        } else {
            addArg(0);
        }

        for (const Option<T> *opt : opts) {
            switch (opt->valueType) {
            case Required:
                if (value.isEmpty() && i + 1 < args.size())
                    value = args.at(++i);
                status = handler(opt->option, std::move(value), i, args);
                break;
            case Optional:
                if (value.isEmpty() && i + 1 < args.size() && (!args.at(i + 1).startsWith('-') || args.at(i + 1).size() == 1))
                    value = args.at(++i);
                status = handler(opt->option, std::move(value), i, args);
                break;
            case NoValue:
                status = handler(opt->option, String(), i, args);
                break;
            }
            if (status.status != Parse_Exec)
                break;
        }
        if (status.status != Parse_Exec)
            break;
    }
    return status;
}

template <typename T>
static void help(FILE *f, const char *app, std::initializer_list<Option<T> > optsList)
{
    List<String> out;
    size_t longest = 0;
    for (const auto &opt : optsList) {
        if (opt.longOpt.isEmpty() && !opt.shortOpt) {
            out.append(String());
        } else {
            out.append(String::format<64>("  %s%s%s%s",
                                          (opt.longOpt.isEmpty() ? String() : ("--" + opt.longOpt)).constData(),
                                          !opt.longOpt.isEmpty() && opt.shortOpt ? "|" : "",
                                          opt.shortOpt ? String::format<2>("-%c", opt.shortOpt).constData() : "",
                                          opt.valueType == Required ? " [arg] "
                                          : opt.valueType == Optional ? " [optional] " : ""));
            longest = std::max<size_t>(out.back().size(), longest);
        }
    }
    fprintf(f, "%s options...\n", app);
    const Option<T> *opts = optsList.begin();
    const size_t c = out.size();
    for (size_t i=0; i<c; ++i) {
        if (out.at(i).isEmpty()) {
            fprintf(f, "%s\n", opts[i].description.constData());
        } else {
            fprintf(f, "%s%s %s\n",
                    out.at(i).constData(),
                    String(longest - out.at(i).size(), ' ').constData(),
                    opts[i].description.constData());
        }
    }
}

template <typename T>
static void man(std::initializer_list<Option<T> > optsList)
{
    const Option<T> *opts = optsList.begin();
    String out = ("<!DOCTYPE manpage SYSTEM \"http://masqmail.cx/xmltoman/xmltoman.dtd\">\n"
                  "<?xml-stylesheet type=\"text/xsl\" href=\"http://masqmail.cx/xmltoman/xmltoman.xsl\"?>\n"
                  "\n"
                  "<manpage name=\"rc\" section=\"1\" desc=\"command line client for RTags\">\n"
                  "\n"
                  "<synopsis>\n"
                  "  <cmd>rc <arg>file.1.xml</arg> > file.1</cmd>\n"
                  "</synopsis>\n"
                  "\n"
                  "<description>\n"
                  "\n"
                  "<p>rc is a command line client used to control RTags.</p>\n"
                  "\n"
                  "</description>\n");
    for (size_t i=0; i<optsList.size(); ++i) {
        if (!opts[i].description.isEmpty()) {
            if (opts[i].longOpt.isEmpty() && !opts[i].shortOpt) {
                if (i)
                    out.append("</section>\n");
                out.append(String::format<128>("<section name=\"%s\">\n", opts[i].description.constData()));
            } else {
                out.append(String::format<64>("  <option>%s%s%s%s<optdesc>%s</optdesc></option>\n",
                                              (opts[i].longOpt.isEmpty() ? String() : ("--" + opts[i].longOpt)).constData(),
                                              !opts[i].longOpt.isEmpty() && opts[i].shortOpt ? "|" : "",
                                              opts[i].shortOpt ? String::format<2>("-%c", opts[i].shortOpt).constData() : "",
                                              opts[i].valueType == Required ? " [arg] " : opts[i].valueType == Optional ? " [optional] " : "",
                                              opts[i].description.constData()));
            }
        }
    }
    out.append("</section>\n"
               "<section name=\"Authors\">\n"
               "  <p>RTags was written by Jan Erik Hanssen &lt;jhanssen@gmail.com&gt; and Anders Bakken &lt;abakken@gmail.com&gt;</p>\n"
               "</section>\n"
               "<section name=\"See also\">\n"
               "  <p><manref name=\"rdm\" section=\"1\"/></p>\n"
               "</section>\n"
               "<section name=\"Comments\">\n"
               "  <p>This man page was written using <manref name=\"xmltoman\" section=\"1\" href=\"http://masqmail.cx/xmltoman/\"/>.</p>\n"
               "</section>\n"
               "</manpage>\n");
    printf("%s", out.constData());
}
}

#endif
