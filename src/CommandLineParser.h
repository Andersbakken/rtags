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

namespace CommandLineParser {
template <typename T>
struct Option {
    const T option;
    const char *longOpt;
    const char shortOpt;
    const int argument;
    const String description;
};
enum ParseStatus {
    Parse_Exec,
    Parse_Ok,
    Parse_Error
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
ParseStatus parse(int &argc, char **argv, std::initializer_list<Option<T> > optsList, Flags<Flag> flags, const std::function<ParseStatus(T)> &handler)
{
    optind = 1;
#ifdef OS_Darwin
    optreset = 1;
#endif
    opterr = (flags & IgnoreUnknown) ? 0 : 1;
    Hash<int, const Option<T> *> shortOptions, longOptions;
    List<option> options;
    String shortOptionsString;

    const Option<T> *opts = optsList.begin();
    for (size_t i=0; i<optsList.size(); ++i) {
        if (opts[i].option) {
            const option opt = { opts[i].longOpt, opts[i].argument, 0, opts[i].shortOpt };
            if (opts[i].shortOpt) {
                shortOptionsString.append(opts[i].shortOpt);
                switch (opts[i].argument) {
                case no_argument:
                    break;
                case required_argument:
                    shortOptionsString.append(':');
                    break;
                case optional_argument:
                    shortOptionsString.append("::");
                    break;
                }
                assert(!shortOptions.contains(opts[i].shortOpt));
                shortOptions[opts[i].shortOpt] = &opts[i];
            }
            if (opts[i].longOpt)
                longOptions[options.size()] = &opts[i];
            options.push_back(opt);
        }
    }

    if (getenv("RTAGS_DUMP_UNUSED")) {
        String unused;
        for (int i=0; i<26; ++i) {
            if (!shortOptionsString.contains('a' + i))
                unused.append('a' + i);
            if (!shortOptionsString.contains('A' + i))
                unused.append('A' + i);
        }
        printf("Unused: %s\n", unused.constData());
        for (size_t i=0; i<optsList.size(); ++i) {
            if (opts[i].longOpt) {
                if (!opts[i].shortOpt) {
                    printf("No shortoption for %s\n", opts[i].longOpt);
                } else if (opts[i].longOpt[0] != opts[i].shortOpt) {
                    printf("Not ideal option for %s|%c\n", opts[i].longOpt, opts[i].shortOpt);
                }
            }
        }
        return Parse_Ok;
    }

    {
        const option opt = { 0, 0, 0, 0 };
        options.push_back(opt);
    }

    ParseStatus ret = Parse_Exec;

    while (ret == Parse_Exec) {
        int idx = -1;
        const int c = getopt_long(argc, argv, shortOptionsString.constData(), options.data(), &idx);
        switch (c) {
        case -1:
            return ret;
        case '?':
        case ':':
            if (!(flags & IgnoreUnknown)) {
                return Parse_Error;
            }
            continue;
        default:
            break;
        }

        const Option<T> *opt = (idx == -1 ? shortOptions.value(c) : longOptions.value(idx));
        const Option<T> *opt = (c ? shortOptions.value(c) : longOptions.value(idx));
        assert(opt);
        assert(opt->option);
        ret = handler(opt->option);
    }
    if (ret == Parse_Exec && optind < argc) {
        fprintf(stderr, "unexpected option -- '%s'\n", argv[optind]);
        return Parse_Error;
    }

    return ret;
}

template <typename T>
static void help(FILE *f, const char *app, std::initializer_list<Option<T> > optsList)
{
    List<String> out;
    size_t longest = 0;
    for (const auto &opt : optsList) {
        if (!opt.longOpt && !opt.shortOpt) {
            out.append(String());
        } else {
            out.append(String::format<64>("  %s%s%s%s",
                                          opt.longOpt ? String::format<4>("--%s", opt.longOpt).constData() : "",
                                          opt.longOpt && opt.shortOpt ? "|" : "",
                                          opt.shortOpt ? String::format<2>("-%c", opt.shortOpt).constData() : "",
                                          opt.argument == required_argument ? " [arg] "
                                          : opt.argument == optional_argument ? " [optional] " : ""));
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
            if (!opts[i].longOpt && !opts[i].shortOpt) {
                if (i)
                    out.append("</section>\n");
                out.append(String::format<128>("<section name=\"%s\">\n", opts[i].description.constData()));
            } else {
                out.append(String::format<64>("  <option>%s%s%s%s<optdesc>%s</optdesc></option>\n",
                                              opts[i].longOpt ? String::format<4>("--%s", opts[i].longOpt).constData() : "",
                                              opts[i].longOpt && opts[i].shortOpt ? "|" : "",
                                              opts[i].shortOpt ? String::format<2>("-%c", opts[i].shortOpt).constData() : "",
                                              opts[i].argument == required_argument ? " [arg] "
                                              : opts[i].argument == optional_argument ? " [optional] " : "",
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
