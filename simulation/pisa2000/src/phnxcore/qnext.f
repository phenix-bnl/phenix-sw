*CMZ :  2.04/00 05/10/92  11.19.37  by  Charles F. Maguire
*-- Author :
*-- Author :    Charles F. Maguire   01/10/92
      SUBROUTINE QNEXT
C  SRTonse 21-SEP-1992. Make Pisa in batch mode recover from a Zebra problem
C  and go on to the next event. Got code from B.Cole.
      IMPLICIT NONE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBEVT.
#include "subevt.inc"
*KEND.
      IF(.NOT.FIRST)THEN
C  write out an empty bank called ENDE so that PISORP programme wil know
C  that this event had something wrong with it.
C  Also set "end of true event" flag true so that Monte carlo reading interface
C  programme will fast-forward to reading the next event
        CALL U_PUT_DS(IXDIV_FE,0,'PISA','ENDE','    ','Z')
        END_EVTFLG = .TRUE.
C  clear main Pisa detector zebra event division
      CALL MZWIPE(IXDIV_FE)
C  clear main Geant zebra event division
      CALL GTRIGC
      ELSE
        FIRST = .FALSE.
      END IF
C Loop over events again
      CALL GRUN
C       Get here when GRUN returns (i.e. GEANT execution is done)
C       Due to the fact that control passes to QNEXT through QNEXTE,
C       a return is not the best way to return to main code and
C       terminate.  Thus, the ZEBRA pre-scribed termination routine
C       ZEND will be used to call UGLAST and terminate execution.
        CALL ZEND
      RETURN
      END
