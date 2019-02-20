#!/usr/bin/env awk -f

# Usage: doc/developers-guide/Hooks-and-handlers.md

BEGIN {
    RS=")"
    ORS=""
    FS="[ (,]"
}

$0 ~ /ejabberd_hooks:run/ {
    found = -1
    for (i = 1; i < NF; i++) {
        if ($i ~ /ejabberd_hooks:run/) {
            found = i
        }
    }
    if (found != -1 && $(found+1) != "" && $(found+1) ~ /^[a-z]/)
        print $(found+1)"\n"
}

