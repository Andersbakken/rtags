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

String Diagnostic::format(const DiagnosticsMap &diagnostics)
{
    Server *server = Server::instance();
    assert(server);
    if (server->activeBuffers().isEmpty())
        server = 0;
    String xmlDiagnostics = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n  <checkstyle>";
    if (!diagnostics.isEmpty()) {
        const char *severities[] = { "none", "warning", "error", "fixit", "skipped" };
        uint32_t lastFileId = 0;

        for (const auto &entry : diagnostics) {
            const Location &loc = entry.first;
            if (server && !server->isActiveBuffer(loc.fileId()))
                continue;
            const Diagnostic &diagnostic = entry.second;
            if (loc.fileId() != lastFileId) {
                if (lastFileId)
                    xmlDiagnostics += "\n    </file>";
                lastFileId = loc.fileId();
                xmlDiagnostics += String::format<128>("\n    <file name=\"%s\">", loc.path().constData());
            }
            if (diagnostic.type != Diagnostic::None) {
                xmlDiagnostics += String::format("\n      <error line=\"%d\" column=\"%d\" %sseverity=\"%s\" message=\"%s\"/>",
                                                 loc.line(), loc.column(),
                                                 (diagnostic.length <= 0 ? ""
                                                  : String::format<32>("length=\"%d\" ", diagnostic.length).constData()),
                                                 severities[diagnostic.type], xmlEscape(diagnostic.message).constData());
            }
        }
        if (lastFileId)
            xmlDiagnostics += "\n    </file>";
    }

    xmlDiagnostics += "\n  </checkstyle>";
    return xmlDiagnostics;
}
