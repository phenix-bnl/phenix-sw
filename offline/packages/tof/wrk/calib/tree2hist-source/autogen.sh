#!/bin/sh
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

(cd $srcdir; aclocal -I ${OFFLINE_MAIN}/share;\
libtoolize --copy --force; automake --copy --add-missing; autoconf)

$srcdir/configure --enable-maintainer-mode "$@"
