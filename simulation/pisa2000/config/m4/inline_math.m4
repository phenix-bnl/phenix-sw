AC_DEFUN(AC_INLINE_MATH,
[AC_CACHE_CHECK(whether inline math functions work,
ac_cv_inline_math,
[AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_RUN([#include <math.h>
int main() { double x, e2, r, t; 
x = -1.0; e2 = 0.001; t = -0.881373587; r = asinh(x); 
if ((r-t)*(r-t) < e2) return(0); else return(1);
}
], ac_cv_inline_math=yes, ac_cv_inline_math=no, ac_cv_inline_math=no)
if test $ac_cv_inline_math=no; then
  AC_DEFINE(__NO_MATH_INLINES)
fi
AC_LANG_RESTORE
])])
