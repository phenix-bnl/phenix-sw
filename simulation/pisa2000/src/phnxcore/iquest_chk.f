*CMZ :  2.04/00 05/10/92  11.19.41  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE IQUEST_CHK(READNXT)
C  check that FZIN read went OK. Return READNXT false if not OK
      IMPLICIT NONE
*KEEP,QUEST.
#include "quest.inc"
*KEND.
      LOGICAL READNXT
      CHARACTER*40 NORMSTAT(6)
      DATA NORMSTAT/'    ',
     &   'EOR record',
     &   'Zebra EOF',
     &   'System EOF, continuation possible',
     &   'System EOData, continuation not possible',
     &   'First attempt to read past EOData'/
      IF(IQUEST(1) .EQ. 0)THEN
        IF(IQUEST(11) .EQ. 1)WRITE(6,30)' Start of new event'
30      FORMAT(1H0,A)
      ELSE IF(IQUEST(1) .LT. 0)THEN
        WRITE(6,*)' Error condition. IQUEST(1) ',IQUEST(1)
        READNXT = .FALSE.
      ELSE IF(IQUEST(1) .GT. 0)THEN
        IF(IQUEST(1) .EQ. 1)THEN
          WRITE(6,70)IQUEST(11)
70        FORMAT(1H ,'Start of run. Run# ',I10)
        ELSE IF(IQUEST(1) .GE. 2 .AND. IQUEST(1) .LE. 4)THEN
          WRITE(6,80)NORMSTAT(IQUEST(1))
80        FORMAT(1H ,A)
        ELSE IF(IQUEST(1) .EQ. 5 .OR. IQUEST(1) .EQ. 6)THEN
          WRITE(6,80)NORMSTAT(IQUEST(1))
          READNXT = .FALSE.
        ELSE
          WRITE(6,*)' IQUEST(1) = ',IQUEST(1)
          READNXT = .FALSE.
        END IF
      END IF
      RETURN
      END
