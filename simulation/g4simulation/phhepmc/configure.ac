AC_INIT(configure.ac)

AM_INIT_AUTOMAKE(phhepmc, 1.00)
AC_PROG_CXX(CC g++)
AC_ENABLE_STATIC(no)
LT_INIT

if test $ac_cv_prog_gxx = yes; then
  CXXFLAGS="$CXXFLAGS -Wall"
  if  test `g++ -dumpversion | gawk '{print $1>=4.8?"1":"0"}'` = 1; then
    CXXFLAGS="$CXXFLAGS -std=c++11"
  fi
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
