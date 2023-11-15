C
      SUBROUTINE cernlib_log
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' CERNLIB_LOG: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
