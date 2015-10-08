_rtags ()
{
    local prev value_opts non_value_opts app cur

    app=${COMP_WORDS[0]}
    value_opts=$($app --help | grep '^ \+-' | grep "\[[A-Za-z]*\]" | sed -e 's,\([^ ]\) .*,\1,' -e 's,|, ,')
    prev=${COMP_WORDS[COMP_CWORD-1]}

    if [ -n "$prev" ] && [ ${COMP_CWORD} -gt 1 ] && printf -- "${value_opts}\n" | grep --quiet -- "$prev"; then
        COMPREPLY=()
        return;
    fi

    cur=${COMP_WORDS[COMP_CWORD]}
    non_value_opts=$($app --help | grep '^ \+-' | grep -v "\[[A-Za-z]*\]" | sed -e 's,\([^ ]\) .*,\1,' -e 's,|, ,')

    COMPREPLY=($(compgen -W "$value_opts $non_value_opts" -- $cur))

    if [ -n "$cur" ] && [ ${#COMPREPLY[@]} -eq 0 ] && printf -- "$cur\n" | grep --quiet -- "^-[^-]"; then
        COMPREPLY=($(compgen -W "$value_opts $non_value_opts" -- -$cur))
    fi
}
complete -F _rtags -o default rc rdm

if [ Cygwin = "$(uname -o 2>/dev/null)" ]; then
    complete -F _rtags -o default rc.exe rdm.exe
fi
