# _AC_CONST_TGFILEINFO
# ----------
# Check for _AC_CONST_TGFILEINFO
AC_DEFUN([AC_CONST_TGFILEINFO],
[AC_LANG_PUSH(C++)
AC_CACHE_CHECK([for const TGFileInfo],
               [ac_cv_const_tgfileinfo],
       [CPPFLAGS_save="$CPPFLAGS"                   CPPFLAGS="-I$ROOTSYS/include $CPPFLAGS"
                AC_COMPILE_IFELSE([AC_LANG_PROGRAM([#include "TGFileDialog.h"],
                                                   [char *s="s";
                                                    TGFileInfo f;
                                                    f.fFileTypes = &s;])],
                               [ac_cv_const_tgfileinfo=no],
                                   [ac_cv_const_tgfileinfo=yes])])
AC_LANG_POP(C++)
if test "$ac_cv_const_tgfileinfo" = yes; then
  AC_DEFINE(CONST_TGFILEINFO, 1,
           [Define to 1 if TGFileInfo.fFileTypes is const char **])
fi
])# _AC_CONST_TGFILEINFO

