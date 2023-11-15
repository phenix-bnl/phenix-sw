c
c     entry points for GUEVGEN calls
c
      SUBROUTINE mc_fred
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' MC_FRED: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
