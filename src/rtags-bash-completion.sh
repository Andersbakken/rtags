complete-rtags ()
{
    app=${COMP_WORDS[0]}
    test -x "$app" || app=`which $app`
    modified="`ls -la \"$app\" | awk '{print $5,$6,$7,$8}'`"
    appname="`basename $app`"

    cache="/tmp/completions-$appname-`id -u`"
    if [ ! -e "$cache" ] || [ "$modified" != "`head -n 1 $cache`" ]; then
        echo $modified > "$cache"
        "$app" --help | grep '^ \+-' | grep "\[[A-Za-z]*\]" | sed -e 's,\([^ ]\) .*,\1,' -e 's,|, ,' | xargs >> "$cache"
        "$app" --help | grep '^ \+-' | grep -v "\[[A-Za-z]*\]" | sed -e 's,\([^ ]\) .*,\1,' -e 's,|, ,' | xargs >> "$cache"
    fi
    local valueopts=`head -n 2 "$cache" | tail -n 1`
    local prev=${COMP_WORDS[COMP_CWORD-1]}
    if [ -n "$prev" ] && printf -- "${valueopts}\n" | grep --quiet -- "$prev"; then
        COMPREPLY=()
        return;
    fi

    local cur=${COMP_WORDS[COMP_CWORD]}
    local nonvalueopts=`tail -n 1 $cache`
    COMPREPLY=(`compgen -W "$valueopts $nonvalueopts" -- $cur`)
    if [ -n "$cur" ] && [ ${#COMPREPLY[@]} -eq 0 ] && printf -- "$cur\n" | grep --quiet -- "^-[^-]"; then
        COMPREPLY=(`compgen -W "$valueopts $nonvalueopts" -- -$cur`)
    fi
}

complete -F complete-rtags -o default rc rdm ./rc ./rdm
