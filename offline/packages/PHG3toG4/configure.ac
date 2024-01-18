AC_INIT(configure.ac)

AM_INIT_AUTOMAKE(PHG3toG4All, 0.1)
AC_PROG_CXX(CC g++)
AC_ENABLE_STATIC(no)
AC_PROG_CC(g++)
AC_PROG_LIBTOOL

ROOTLIBS=`root-config --libs`
ROOTINC=`root-config --incdir`
AC_SUBST(ROOTLIBS)
AC_SUBST(ROOTINC)

dnl PYTHIALIB='-lEG -lpythia8'
dnl AC_SUBST(PYTHIALIB)

dnl   no point in suppressing warnings people should
dnl   at least see them, so here we go for g++: -Wall
if test $ac_cv_prog_gxx = yes; then
   CXXFLAGS="$CXXFLAGS -Wall -Werror  -std=c++11"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT


