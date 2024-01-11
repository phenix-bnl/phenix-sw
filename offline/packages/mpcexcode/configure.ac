AC_INIT(mpcex, [1.00])
AC_CONFIG_SRCDIR([configure.ac])

AM_INIT_AUTOMAKE

AC_PROG_CXX(CC g++)
AC_ENABLE_STATIC(no)
AC_PROG_LIBTOOL

dnl   no point in suprressing warnings people should
dnl   at least see them, so here we go for g++: -Wall
if test $ac_cv_prog_gxx = yes; then
    CXXFLAGS="$CXXFLAGS -Wall -Werror"
fi

AC_OUTPUT(Makefile)
