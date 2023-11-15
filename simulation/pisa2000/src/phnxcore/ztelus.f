*CMZ :  2.04/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :    Charles F. Maguire   01/10/92
        Subroutine ZTELUS
C  SRTonse 21-SEP-1992. Write out some explanation of what is happenening
C  whwnever Zebra has some kind of error
      IMPLICIT NONE
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEND.
        Common /ZTELLC/ ID, MODE
        Integer           ID, MODE
      INTEGER ICOUNT, MAXICOUNT
      SAVE ICOUNT, MAXICOUNT
      DATA ICOUNT/0/, MAXICOUNT/11/
      ICOUNT = ICOUNT + 1
      IF(ID .EQ. 99)THEN
        IF(ICOUNT .LT. MAXICOUNT)THEN
          WRITE(6,50)
     &       ' ZTELUS: Insufficient memory available. Occurence # ',
     &       ICOUNT,' Abort current event, and move on '
        ELSE
          WRITE(6,50)
     &       ' ZTELUS: Insufficient memory available. Occurence # ',
     &       ICOUNT,' Abort run. Call ZEND'
          MODE = 3
        END IF
      ELSE
          Write (6, 100) ID, MODE
      END IF
50    FORMAT(A,I5,/,A)
100     Format (1H ,'ZTELUS:Error trap. ID = ', I4, 1X, 'Mode = ',
     &   I2)
        Return
        End
