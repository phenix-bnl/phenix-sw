C
      SUBROUTINE mc_fire        ! PBM & JS fireball
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        WRITE(6,*)' MC_FIRE: <I> Dummy Routine Entered '
        FIRST = .FALSE.
      END IF
      RETURN
      END
