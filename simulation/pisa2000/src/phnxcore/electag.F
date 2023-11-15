**
      FUNCTION ELECTAG(I)
C  to return the type of electron/positron we have just produced
C  If the secondary is an e+/e- we have a special measure to tag it so that
C  later we can know whther it is a Dalitz(1)/Pair prod(2)/Deltaray(3)/
C  compton or "other"(4).  SRTonse 19-AUG-1992
      IMPLICIT NONE
*KEEP,GCKING.
#include "geant321/gcking.inc"
*KEEP,GCKINE.
#include "gckine.inc"
*KEEP,GCTRAK.
#include "gctrak.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEND.
      INTEGER I,IT,J, IUCOMP
      REAL ELECTAG
      ELECTAG = 0.0
C  pair production
      IF(IPART .EQ. 1 .AND. NGKINE .EQ. 2)THEN
        IT = 6
C  search NMEC elements of array LMEC for item = IT
        IF(IUCOMP(IT,LMEC,NMEC) .NE. 0)THEN
          ELECTAG = 2.0
        ELSE
          WRITE(LOUT,*)'ELECTAG:PAIR ',(LMEC(J),J=1,NMEC)
        END IF
C    Dalitz decay of a pi0 has occurred
      ELSE IF(IPART .EQ. 7 .AND. NGKINE .EQ. 3)THEN
C  ensure that current process is a decay
        IT = 5
        IF(IUCOMP(IT,LMEC,NMEC) .NE. 0)THEN
          ELECTAG = 1.0
        ELSE
          WRITE(LOUT,*)'ELECTAG:DALITZ ',(LMEC(J),J=1,NMEC)
        END IF
      ELSE IF(GKIN(5,I) .EQ. 3)THEN
        IT = 10
        IF(IUCOMP(IT,LMEC,NMEC) .NE. 0)THEN
          ELECTAG = 3.0       !Delta ray
        ELSE
          ELECTAG = 4.0       !other
        END IF
      ELSE IF(GKIN(5,I) .EQ. 2)THEN
        ELECTAG = 4.0            !other
      END IF
      RETURN
      END
