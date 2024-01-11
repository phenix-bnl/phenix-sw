#!/bin/sh
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

(cd $srcdir; aclocal -I ${OFFLINE_MAIN}/share -W all;\
libtoolize --force; automake -a -W all; autoconf -W all)

$srcdir/configure "$@"
