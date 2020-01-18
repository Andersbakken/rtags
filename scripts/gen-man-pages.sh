#!/bin/bash -e
# gen-man-pages.sh --- generate man pages
#
# Copyright (c) Christian Schwarzgruber <c.schwarzgruber.cs@gmail.com>
#
# This file is part of RTags (https://github.com/Andersbakken/rtags).
#
# RTags is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or (at
# your option) any later version.
#
# RTags is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */
#
# Description: rc, rdm, help2man and sed need to be in the PATH environment variable.
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [ $# -lt 1 ]; then
    echo "Usage: $(basename $0) RDM_RC_BIN_DIR"
    exit 1
fi

SED=$(command -v sed)
if [ "$(uname)" == "Darwin" ]; then
    SED=$(command -v gsed)
fi

if [ ! -x "$SED" ]; then
    echo "You need sed installed (and on Mac it needs to be gsed) to run ${BASH_SOURCE[0]}"
    exit 1
fi

MAN_BASE="$BASE_DIR/man/man7"

# Force simply locale `C`
LC_TIME=C

# Generate the rc manual page.
echo 'Generating manual page for "rc"'
help2man --no-info -s 7                         \
         -i <(echo "
[SYNOPSIS]
rc [OPTION]...

[DESCRIPTION]
rc is the RTags client application.

[SEE ALSO]
rdm(7)
") "$1/rc" > "$MAN_BASE/rc.7"

# Generate the rdm manual page.
echo 'Generating manual page for "rdm"'
help2man --no-info -s 7                         \
         -i <(echo "
[SYNOPSIS]
rdm [OPTION]...

[DESCRIPTION]
RTags is a client/server application that indexes C/C++ code and keeps
a persistent file\-based database of references, declarations,
definitions, symbolnames etc. There's also limited support for
ObjC/ObjC++. It allows you to find symbols by name (including nested
class and namespace scope). Most importantly we give you proper
follow\-symbol and find\-references support. We also have neat little
things like rename\-symbol, integration with clang's \"fixits\"
(http://clang.llvm.org/diagnostics.html). We also integrate with
flymake using clang's vastly superior errors and warnings. Since
RTags constantly will reindex \"dirty\" files you get live updates of
compiler errors and warnings. Since we already know how to compile
your sources we have a way to quickly bring up the preprocessed output
of the current source file in a buffer.

While existing taggers like gnu global, cscope, etags, ctags etc do a
decent job for C they often fall a little bit short for C++. With its
incredible lexical complexity, parsing C++ is an incredibly hard task
and we make no bones about the fact that the only reason we are able
to improve on the current tools is because of clang
(http://clang.llvm.org/). RTags is named RTags in recognition of
Roberto Raggi on whose C++ parser we intended to base this project but
he assured us clang was the way to go. The name stuck though.

[SEE ALSO]
rc(7)
") "$1/rdm" > "$MAN_BASE/rdm.7"

# Fix-ups
"$SED" -ri                                         \
    -e '/^(rdm|rc) options...$/d'               \
    -e 's/^Options:$/.SH OPTIONS/'              \
    -e 's/^(Path to rp) \(default.*\).$/\1./'   \
    -e '/job\\-count/{
                n
                s/(.*indexing) \(default [0-9]+\).$/\1./
       }'                                       \
    "$MAN_BASE/rc.7" "$MAN_BASE/rdm.7"
exit 0
