c
c     entry points for GSI data summary tape routines
c
      SUBROUTINE e_kdef
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' E_KDEF: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
