AC_INIT(mutoo_alignment, [1.00])
AC_CONFIG_SRCDIR([configure.ac])

AM_INIT_AUTOMAKE

AC_PROG_CXX(CC g++)
AC_PROG_F77(gfortran f77 g77)
LT_INIT([disable-static])

dnl loop optimization screws us for gfortran > 4.8
if test `g++ -dumpversion | gawk '{print $1>=4.8?"1":"0"}'` = 1; then
 FFLAGS="$FFLAGS -fno-aggressive-loop-optimizations"
fi

### Checks for libraries.
AC_F77_LIBRARY_LDFLAGS

if test $ac_cv_prog_gxx = yes; then
  CXXFLAGS="$CXXFLAGS  -Wall -Werror"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
