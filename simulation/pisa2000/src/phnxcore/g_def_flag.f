C
      SUBROUTINE g_def_flag
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' G_DEF_FLAG: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
