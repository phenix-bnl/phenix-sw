#!/bin/sh
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

#(cd $srcdir; aclocal -I ${OFFLINE_MAIN}/share;\
(cd $srcdir; aclocal -I /usr/share;\
libtoolize --force; automake --add-missing; autoconf)

$srcdir/configure --enable-maintainer-mode "$@"
