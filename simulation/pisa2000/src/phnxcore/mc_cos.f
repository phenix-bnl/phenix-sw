C
      SUBROUTINE mc_cos         ! cosmic rays
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' MC_COS: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
