C
      SUBROUTINE SINR_GUSTEP    !dummy Si inner STonse
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' SINR_GUSTEP: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
