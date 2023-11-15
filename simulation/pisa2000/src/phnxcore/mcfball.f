C
      SUBROUTINE mcfball        ! private GSI cluster routine
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' MCFBALL: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
