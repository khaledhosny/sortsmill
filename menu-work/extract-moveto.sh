#!/bin/sh

export LC_ALL
LC_ALL=C

for i in *.menu_fields; do
    sed -e '/\.moveto[ \t]*=/p;d' $i \
        | sed -e 's/^.*=[ \t]\([A-Z_a-z][A-Z_a-z0-9]*\).*/\1/' \
        | sort -u \
        | sed -e '/^NULL$/d' \
        > `basename ${i} .menu_fields`.moveto
done
