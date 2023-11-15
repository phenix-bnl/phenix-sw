*CMZ :  2.04/00 05/10/92  11.19.37  by  Charles F. Maguire
*-- Author :
*-- Author :    Charles F. Maguire   01/10/92
        Subroutine ZEND
C  FROM Brian Cole
C       First crack at ZEND routine.  This routine calls UGLAST
C         and terminates.  It is called by QNEXT at end of event
C         processing loop.
C
C
C BAC --> Set ZEBRA execution phase to TERMINATION
        Call ZPHASE (-1)
C-->    Do end-of-run processing:
        CALL GULAST
      CALL MZEND
        STOP
        End
