# This file is part of RTags (https://github.com/Andersbakken/rtags).

# RTags is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# RTags is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with RTags.  If not, see <https://www.gnu.org/licenses/>.

__init_completion()
{
    COMPREPLY=()
    _get_comp_words_by_ref cur prev words cword
}

_rtags ()
{
    # Those local variables are needed by _init_completion.
    local cur prev words cword
    if declare -F _init_completions >/dev/null 2>&1; then
        _init_completion
    else
        __init_completion
    fi
    local app=${words[0]}

    if [[ $app == \~* ]]; then
        # Tell shellcheck we know that single quotes prevent expansion.
        # shellcheck disable=SC2016
        eval app="${app%%/*}"/'${app#*/}'
    fi

    # In case app starts with a dollar sign, like $HOME/...
    if [[ $app == \$* ]]; then
        # shellcheck disable=SC2016
        eval app="${app%%/*}"/'${app#*/}'
    fi

    # Always use absolute path to executable so we can verify it later.
    if [[ $app != /* ]]; then
        app=$(which $app)
    fi

    # If value of app doesn't point to an executable after possible expansion
    # return immediately.
    test -x $app || return

    local -r value_opts=$($app --help | grep '^ \+-' | grep "\[[A-Za-z]*\]" | \
                              sed -e 's,\([^ ]\) .*,\1,' -e 's,|, ,')

    if [ -n "$prev" ] && [ $cword -gt 1 ] \
           && printf -- "${value_opts}\n" | grep --quiet -- "$prev"; then
        COMPREPLY=()
        return
    fi

    local -r non_value_opts=$($app --help | grep '^ \+-' | \
                                  grep -v "\[[A-Za-z]*\]" | \
                                  sed -e 's,\([^ ]\) .*,\1,' -e 's,|, ,')

    COMPREPLY=($(compgen -W "$value_opts $non_value_opts" -- $cur))

    if [ -n "$cur" ] && [ ${#COMPREPLY[@]} -eq 0 ] \
           && printf -- "$cur\n" | grep --quiet -- "^-[^-]"; then
        COMPREPLY=($(compgen -W "$value_opts $non_value_opts" -- -$cur))
    fi
}
complete -F _rtags -o default rc rdm

if [ Cygwin = "$(uname -o 2>/dev/null)" ]; then
    complete -F _rtags -o default rc.exe rdm.exe
fi
