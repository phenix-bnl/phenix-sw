      SUBROUTINE FCL_GUSTEP

      Implicit none

#include "gctrak.inc"
#include "gckine.inc"
#include "gcsets.inc"

      INTEGER K
      INTEGER ITEST
      REAL HIT_ALL(8)  ! the 8 refers to how many tracking parameters are stored for each hit

c      write(6,2019)inwvol
c 2019 format(/,  ' fcl_gustep inwvol ', i4,/)

      IF (INWVOL .EQ. 0 .AND. ISTOP .EQ. 0) THEN
C  accumulate energy loss
         HIT_ALL(4) = HIT_ALL(4) + DESTEP
      ELSE IF( INWVOL .EQ. 1 ) THEN
C  record position. Only for entry into the volume
         DO K = 1,3
            HIT_ALL( K) = VECT( K)
         END DO
C  initialize energy loss & record entry momentum
         HIT_ALL(4) = 0.0
         HIT_ALL(6) = VECT(7)*VECT(4)
         HIT_ALL(7) = VECT(7)*VECT(5)
         HIT_ALL(8) = VECT(7)*VECT(6)
      ELSE IF (INWVOL .EQ. 2 .OR. ISTOP .NE. 0) THEN
         HIT_ALL(4) = HIT_ALL(4) + DESTEP
c         write(*,*) ' FCL energy is: ', HIT_ALL(4)
         HIT_ALL(5) = FLOAT(IPART)
c         write(6,998)hit_all(4)
c 998     format(/, '  FCL_GUSTEP store DELE ', e14.5,/)
         CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HIT_ALL, ITEST )
c         write(*,*) ISET, IDET, ITRA, NUMBV, HIT_ALL, ITEST 
         IF(ITEST.EQ.0)THEN
           WRITE(6,999)ITEST
999        FORMAT(/,' FCL_GUSTEP <E>: failure in call to GSAHIT ',
     +            /,' return value was ITEST = ', i10,/)
           STOP  ' PISA is stopping in FCL_GUSTEP'
         ENDIF  !  check for successful store
      END IF
      RETURN
      END
