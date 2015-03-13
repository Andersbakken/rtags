/* This file is part of RTags.

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

#include "Diagnostic.h"
#include "RTags.h"
#include "Server.h"

static inline String xmlEscape(const String& xml)
{
    if (xml.isEmpty())
        return xml;

    std::ostringstream strm;
    const char* ch = xml.constData();
    bool done = false;
    while (true) {
        switch (*ch) {
        case '\0':
            done = true;
            break;
        case '"':
            strm << "\\\"";
            break;
        case '<':
            strm << "&lt;";
            break;
        case '>':
            strm << "&gt;";
            break;
        case '&':
            strm << "&amp;";
            break;
        default:
            strm << *ch;
            break;
        }
        if (done)
            break;
        ++ch;
    }
    return strm.str();
}

static inline const String elispEscape(const String &data)
{
    String ret = data;
    ret.replace("\"", "\\\"");
    ret.replace("\n", "\\n"); // ### this could be done more efficiently
    return ret;
}

String Diagnostic::format(const Diagnostics &diagnostics, Format format)
{
    Server *server = Server::instance();
    assert(server);
    if (server->activeBuffers().isEmpty())
        server = 0;
    String ret;

    const char *severities[] = { "none", "warning", "error", "fixit", "skipped" };
    uint32_t lastFileId = 0;

    static const char *header[] = {
        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n  <checkstyle>",
        "(list 'checkstyle "
    };
    static const char *startFile[] = {
        "\n    <file name=\"%s\">",
        "(cons \"%s\" (list"
    };
    static const char *endFile[] = {
        "\n    </file>",
        "))"
    };
    static const char *trailer[] = {
        "\n  </checkstyle>",
        ")"
    };
    std::function<String(const Location &, const Diagnostic &)> formatDiagnostic;
    if (format == XML) {
        formatDiagnostic = [severities](const Location &loc, const Diagnostic &diagnostic) {
            return String::format("\n      <error line=\"%d\" column=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                                  loc.line(), loc.column(),
                                  (diagnostic.length <= 0 ? ""
                                   : String::format<32>("length=\"%d\" ", diagnostic.length).constData()),
                                  severities[diagnostic.type], xmlEscape(diagnostic.message).constData());
        };
    } else {
        formatDiagnostic = [severities](const Location &loc, const Diagnostic &diagnostic) {
            return String::format<256>(" (list %d %d %s '%s \"%s\")",
                                       loc.line(), loc.column(),
                                       diagnostic.length > 0 ? String::number(diagnostic.length).constData() : "nil",
                                       severities[diagnostic.type],
                                       elispEscape(diagnostic.message).constData());
        };
    }
    bool first = true;
    for (const auto &entry : diagnostics) {
        const Location &loc = entry.first;
        if (server && !server->isActiveBuffer(loc.fileId()))
            continue;
        if (first) {
            ret = header[format];
            first = false;
        }
        const Diagnostic &diagnostic = entry.second;
        if (loc.fileId() != lastFileId) {
            if (lastFileId)
                ret << endFile[format];
            lastFileId = loc.fileId();
            ret << String::format<256>(startFile[format], loc.path().constData());
        }
        if (diagnostic.type != Diagnostic::None) {
            ret << formatDiagnostic(loc, diagnostic);
        }
    }
    if (lastFileId)
        ret << endFile[format];

    ret << trailer[format];
    return ret;
}
