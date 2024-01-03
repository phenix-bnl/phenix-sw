#!/bin/sh
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

# Inside RCF we have some setup with patched standard
# m4 files in non-standard locations, so add them to
# the include path there.
[ -d $OFFLINE_MAIN/share ] && EXTRA_M4="-I $OFFLINE_MAIN/share"

(cd $srcdir; aclocal $EXTRA_M4;\
libtoolize --force; automake -a --add-missing; autoconf)

$srcdir/configure  "$@"

