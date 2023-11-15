C
      SUBROUTINE e_dst_chck
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' E_DST_CHCK: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
