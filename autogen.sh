#!/bin/sh
# Run this to generate all the initial makefiles, etc.

test -n "$srcdir" || srcdir=`dirname "$0"`
test -n "$srcdir" || srcdir=.

olddir=`pwd`
cd $srcdir

echo -n "checking for gnulib-tool... "
which gnulib-tool || {
	echo "*** No gnulib-tool found, please install it ***"
	exit 1
}

echo -n "checking for intltoolize... "
which intltoolize || {
	echo "*** No intltoolize found, please install it ***"
	exit 1
}

echo -n "checking for autoreconf... "
which autoreconf || {
	echo "*** No autoreconf found, please install it ***"
	exit 1
}

echo -n "checking for pkg-config... "
which pkg-config || {
	echo "*** No pkg-config found, please install it ***"
	exit 1
}

echo "running gnulib-tool --update"
gnulib-tool --update || exit $?

echo "running intltoolize --copy --force --automake"
intltoolize --copy --force --automake || exit $?

echo "running autoreconf --install --verbose"
autoreconf --install --verbose || exit $?

cd $olddir
