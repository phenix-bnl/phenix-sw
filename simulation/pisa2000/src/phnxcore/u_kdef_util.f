C
      SUBROUTINE u_kdef_util
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' U_KDEF_UTIL: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
