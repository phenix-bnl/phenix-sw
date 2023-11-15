 
      SUBROUTINE e_kdef_dst
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' E_KDEF_DST: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
