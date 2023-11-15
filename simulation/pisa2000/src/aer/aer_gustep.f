      subroutine aer_gustep
      implicit none

#include "gctrak.inc"
#include "gckine.inc"
#include "gcsets.inc"
#include "subevt.inc"

      integer track_id
      integer track_id1
      
      integer hit_counter

      INTEGER K
      INTEGER ITEST
      integer nhAER
      parameter (nhAER = 17)
      REAL HIT_ALL(nhAER)  ! the 17 refers to how many tracking parameters 
                           ! are stored for each hit
      SAVE HIT_ALL

      integer ifirst /0/
      save ifirst

c Start Execute

c---------------------------------------------------------------
c Particle is inside the Aerogel volume
c---------------------------------------------------------------
      IF (INWVOL .EQ. 0 .AND. ISTOP .EQ. 0) THEN
C  record position. 
         DO K = 1,3
            HIT_ALL(K) = VECT(K)
         END DO
C  accumulate energy loss
         HIT_ALL(4) = HIT_ALL(4) + DESTEP
C  current step length
         HIT_ALL(5) = FLOAT(IPART)
         HIT_ALL(6) = VECT(7)*VECT(4)
         HIT_ALL(7) = VECT(7)*VECT(5)
         HIT_ALL(8) = VECT(7)*VECT(6)
         HIT_ALL(9) = SLENG ! current path length (cm)
         HIT_ALL(10) = TOFG*1.0E9  ! time of flight, converted to ns
         HIT_ALL(11) = STEP ! current step length (cm)
         HIT_ALL(12) = GETOT ! current total energy
         HIT_ALL(13) = CHARGE ! current charge of particle
         HIT_ALL(14) = VECT(7) ! current total momentun
         HIT_ALL(15) = VERT(1)
         HIT_ALL(16) = VERT(2)
         HIT_ALL(17) = VERT(3)
         CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HIT_ALL, ITEST )
c---------------------------------------------------------------         
c Particle enter the Aerogel volume
c---------------------------------------------------------------
      ELSE IF( INWVOL .EQ. 1 ) THEN
C  record position. 
         DO K = 1,3
            HIT_ALL(K) = VECT(K)
         END DO
C  current step length
         HIT_ALL(4) = 0.0
         HIT_ALL(5) = FLOAT(IPART)
         HIT_ALL(6) = VECT(7)*VECT(4)
         HIT_ALL(7) = VECT(7)*VECT(5)
         HIT_ALL(8) = VECT(7)*VECT(6)
         HIT_ALL(9)  = SLENG ! current path length (cm)
         HIT_ALL(10) = TOFG*1.0E9  ! time of flight, converted to ns
         HIT_ALL(11) = STEP ! current step length (cm)
         HIT_ALL(12) = GETOT ! current total energy
         HIT_ALL(13) = CHARGE ! current charge of particle
         HIT_ALL(14) = VECT(7) ! current total momentum
         HIT_ALL(15) = VERT(1)
         HIT_ALL(16) = VERT(2)
         HIT_ALL(17) = VERT(3)
         
c        write(6,*)'CHARGE=',HIT_ALL(13)
C     initialise energy loss & record entry momentum
         CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HIT_ALL, ITEST )
c----------------------------------------------------------------
c Particle exit the Aerogel volume 
c----------------------------------------------------------------
      ELSE IF (INWVOL .EQ. 2 .OR. ISTOP .NE. 0) THEN
C  record position. 
         DO K = 1,3
            HIT_ALL(K) = VECT(K)
         END DO

c  particle is exiting the volume, or has stopped

         HIT_ALL(4) = HIT_ALL(4) + DESTEP
         HIT_ALL(5) = FLOAT(IPART)
         HIT_ALL(6) = VECT(7)*VECT(4)
         HIT_ALL(7) = VECT(7)*VECT(5)
         HIT_ALL(8) = VECT(7)*VECT(6)
         HIT_ALL(9) = SLENG ! current path length (cm)
         HIT_ALL(10) = TOFG*1.0E9  ! time of flight, converted to ns
         HIT_ALL(11) = STEP ! current step length (cm)
         HIT_ALL(12) = GETOT ! current total energy
         HIT_ALL(13) = CHARGE ! current charge of particle
         HIT_ALL(14) = VECT(7) ! current total momentum
         HIT_ALL(15) = VERT(1)
         HIT_ALL(16) = VERT(2)
         HIT_ALL(17) = VERT(3)

         if(ifirst.lt.0)then
            ifirst = ifirst + 1
            write(6,914)hit_all
 914        format(//,'  aer_gustep  hit_all ',/,3x,5e14.5,/,
     +             3x,5e14.5,//)
         endif  ! check for print out of hits information

         CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, HIT_ALL, ITEST )
         IF(ITEST.EQ.0)THEN
           WRITE(6,999)ITEST
999        FORMAT(/,' AER_GUSTEP <E>: failure in call to GSAHIT ',
     +            /,' return value was ITEST = ', i10,/)
           STOP  ' PISA is stopping in AER_GUSTEP'
         ENDIF  !  check for successful store
c      write(6,998)(hit_all(k),k=1,10)
998    format(/,' AER hit stored ',10(f8.4,2x))
      END IF

      track_id = itra

      return
      end
