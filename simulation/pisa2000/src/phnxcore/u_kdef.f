c
c     entry points for GSI user routines
c
      SUBROUTINE  u_kdef
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' U_KDEF: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
