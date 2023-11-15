      SUBROUTINE GTRIGI
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       Initialises event partition                              *
C.    *    ==>Called by : GRUN ,<GXINT> GINC4                          *
C.    *       Author    R.Brun  *********                              *
C.    *                                                                *
C.    ******************************************************************
C.
#include "gcbank.inc"
#include "gcnum.inc"
#include "gcflag.inc"
#include "gcunit.inc"
#include "gctrak.inc"

#include "subevt.inc"

c     Modified to set random start time

      real t0phnx /0.0/
C.
C.    ------------------------------------------------------------------
C.
      IEOTRI=0
      NTRACK=0
      NVERTX=0
      IDEBUG=0
      TOFG  = t0start   ! randomized start time in seconds is set in t0init function

C               Print event number and random number generator

      IF(RAYTRA.NE.1.)CALL GRNDMQ(NRNDM(1),NRNDM(2),0,'G')

C               Create event header bank

      CALL MZBOOK(IXDIV,JHEAD,JHEAD,1,'HEAD', 1, 1,NHEAD,2,0)
      IDEVT=IDEVT+1
      IQ(JHEAD+1)=IDRUN
      IQ(JHEAD+2)=IDEVT
      IQ(JHEAD+3)=NRNDM(1)
      IQ(JHEAD+4)=NRNDM(2)

      IF(ITEST.GT.0)THEN
         IF(MOD(IEVENT,ITEST).EQ.0)THEN
           IF(RAYTRA.NE.1.)THEN
            WRITE (CHMAIL,1000) IEVENT,IDEVT,(NRNDM(I),I = 1,2)
            CALL GMAIL(0,0)
           ENDIF
         ENDIF
      ENDIF

      IF(IEVENT.GE.IDEMIN.AND.IEVENT.LE.IDEMAX)IDEBUG=1

 1000 FORMAT(' **** GTRIGI: IEVENT=',I7,' IDEVT=',I7,
     +' Random Seeds = ',I10,2X,I10)
  99  RETURN
      END
