AC_INIT(mfm, [1.00])
AC_CONFIG_SRCDIR([configure.ac])

AM_INIT_AUTOMAKE
  
AC_PROG_CXX(CC g++)
AC_PROG_F77(gfortran f77 g77)
LT_INIT([disable-static])
dnl loop optimization screws us for gfortran > 4.8
if test `g++ -dumpversion | gawk '{print $1>=4.8?"1":"0"}'` = 1; then
 FFLAGS="$FFLAGS -fno-aggressive-loop-optimizations"
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
