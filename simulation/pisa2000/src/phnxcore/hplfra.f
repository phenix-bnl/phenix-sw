C
      SUBROUTINE hplfra
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' HPLFRA: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
