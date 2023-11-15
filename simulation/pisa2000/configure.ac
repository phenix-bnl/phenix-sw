AC_INIT(pisa2000,[2.0])
AC_CONFIG_SRCDIR([configure.ac])

AM_INIT_AUTOMAKE

AC_PROG_CXX(CC g++)
AC_PROG_CC(cc gcc)
AC_PROG_F77(gfortran f77 g77)
LT_INIT([disable-static])


case $F77 in
gfortran*)
 FFLAGS="$FFLAGS -fno-automatic -ffixed-line-length-none -fsecond-underscore -Wall"
if  test `g++ -dumpversion | gawk '{print $1>=4.8?"1":"0"}'` = 1; then
 FFLAGS="$FFLAGS -fno-aggressive-loop-optimizations"
fi

;;
dnl handles f77 and g77
*77)
 if test $ac_cv_prog_g77 = yes; then
  FFLAGS="$FFLAGS -O -fno-automatic -finit-local-zero -fugly-complex -DCERNLIB_LINUX -DCERNLIB_UNIX -DCERNLIB_LNX -DCERNLIB_QMGLIBC -DCERNLIB_BLDLIB"
 fi
;;
*)
 AC_MSG_ERROR([unknown fortran compiler: $F77])
;;
esac

### Checks for libraries.
AC_F77_LIBRARY_LDFLAGS

### AC_FIND_ROOT
ROOTLIBS=`root-config --libs`
AC_SUBST(ROOTLIBS)

KUIPC=${CERN}/${CERN_LEVEL}/bin/kuipc
AC_SUBST(KUIPC)

#########
# CERNLIB
if test -e ${OPT_PHENIX}/bin/phenix_cernlib.pl; then
  CERNLIBS="`${OPT_PHENIX}/bin/phenix_cernlib.pl`"
else
  AC_MSG_ERROR([${OPT_PHENIX}/bin/phenix_cernlib.pl not found])
fi
AC_SUBST(CERNLIBS) 

AC_CONFIG_FILES([
Makefile
config/Makefile
config/m4/Makefile
inc/Makefile
src/Makefile
src/aer/Makefile
src/bbc/Makefile
src/cdf/Makefile
src/crk/Makefile
src/emc/Makefile
src/fcl/Makefile
src/itr/Makefile
src/mpc/Makefile
src/mpcex/Makefile
src/mum/Makefile
src/mun/Makefile
src/svx/Makefile
src/rxn/Makefile
src/hbd/Makefile
src/pad/Makefile
src/phnxcore/Makefile
src/root/Makefile
src/tfw/Makefile
src/tof/Makefile
src/trd/Makefile
src/zdc/Makefile
src/muonTrg_pad/Makefile
src/rlt/Makefile
src/ncc/Makefile
src/pdb_util/Makefile
])
AC_OUTPUT

