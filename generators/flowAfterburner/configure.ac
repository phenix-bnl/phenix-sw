AC_INIT(flowAfterburner.C)

AM_INIT_AUTOMAKE(flowAfterburner, 1.00)

AC_PROG_CXX
AC_ENABLE_STATIC(no)
AC_PROG_LIBTOOL
if test $ac_cv_prog_gxx = yes; then
   if  test `g++ -dumpversion | gawk '{print $1>=4.8?"1":"0"}'` = 1; then
     CXXFLAGS="$CXXFLAGS -std=c++11"
   fi
fi

AC_OUTPUT(Makefile)
