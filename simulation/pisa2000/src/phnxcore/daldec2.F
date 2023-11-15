*CMZ :  2.04/00 08/04/93  15.54.42  by  Unknown
*-- Author :
*-- Author :
      SUBROUTINE DALDEC2
C  decay subroutine. Now using for Dalitz decay of the eta. SRTonse
C   12-AUG-1992
      IMPLICIT NONE
*KEEP,GCKINE.
#include "gckine.inc"
*KEEP,GCKING.
#include "geant321/gcking.inc"
*KEEP,GCTRAK.
#include "gctrak.inc"
*KEND.
      INTEGER I
      REAL PIZMOM(4),PHOMOM(4),ELEMOM(4),POSMOM(4),
     &   ELEMASS
      DATA ELEMASS/0.000511/
C  call Dalitz decay routine. Input Pi zero 4-mom. Output 4-mom of 3 daughters.
C  if Eta is stopped, vect(7) is zero, leading to numerical problems in the
C  called pdalitz_eta code. Apply a minimum of 1eV, or use VECT(7) if it is larger
      DO I = 1,3
        PIZMOM(I) = VECT(I+3)*MAX(1.e-9,VECT(7))
      END DO
      PIZMOM(4) = SQRT(PIZMOM(1)**2 + PIZMOM(2)**2 + PIZMOM(3)**2 +
     &   AMASS**2)
      CALL PDALITZ_ETA(PIZMOM,PHOMOM,ELEMOM,POSMOM)
      NGKINE = 3
C  photon
      DO I = 1,4
        GKIN(I,1) = PHOMOM(I)
      END DO
C  positron
      DO I = 1,4
        GKIN(I,2) = POSMOM(I)
      END DO
C  electron
      DO I = 1,4
        GKIN(I,3) = ELEMOM(I)
      END DO
      GKIN(5,1) = 1.0         !photon
      GKIN(5,2) = 2.0
      GKIN(5,3) = 3.0         !e+ & e-
C  set time of flight of the particles to zero.
      TOFD(1) = 0.0
      TOFD(2) = 0.0
      TOFD(3) = 0.0

C     GEANT 3.21 additions

      DO I = 1,3
         GPOS(I,1) = VECT(I)
         GPOS(I,2) = VECT(I)
         GPOS(I,3) = VECT(I)
      ENDDO

      RETURN
      END
