*CMZ :  2.04/00 28/07/94  20.19.55  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   19/07/94
         SUBROUTINE TRDSTP
*     ===============================

***************************************************
*                                                 *
*    USER ROUTINE CALLED FROM GUSTEP              *
*    TO ORGANIZE THE HITS DATA FOR TEC            *
*                                                 *
***************************************************

         IMPLICIT NONE
 
 
#include "gckine.inc"
#include "gctmed.inc"
 

c    Local variables

        INTEGER LFIRST, JFLSEL

        SAVE LFIRST, JFLSEL
 
      integer jftrsl, jfctsl   !  these really are used in PISORP
 
      COMMON /ITRCTP/  jftrsl, jfctsl
 
        DATA LFIRST /0/, JFLSEL/ 0/

         IF ( LFIRST .EQ. 0 ) THEN
           LFIRST = 1
          JFLSEL = JFTRSL
         ENDIF

          IF ( ISVOL.GT. 0 .AND. CHARGE.NE.0. ) THEN
             call tecstp
          ENDIF

 9999  continue
      return
          END
