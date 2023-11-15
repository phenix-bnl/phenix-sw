*
* $Id: gsvert.F,v 1.1 2020/01/29 20:04:04 lajoie Exp $
*
* $Log: gsvert.F,v $
* Revision 1.1  2020/01/29 20:04:04  lajoie
* added ability to handle multiple linked KINE and VERT banks
*
* Revision 1.2  1998/02/10 16:04:21  japost
*   Comments are correctly redistributed.
*
* Revision 1.1.1.1  1995/10/24 10:21:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GSVERT(V,NTBEAM,NTTARG,UBUF,NWBUF,NVTX)
C.
C.    ******************************************************************
C.    *                                                                *
C     *       Creates a new vertex bank                                *
C     *       Vertex is generated from tracks NTBEAM NTTARG            *
C     *       NVTX is the new vertex number                            *
C.    *                                                                *
C.    *                                                                *
C.    * In detail:                                                     *
C.    *                                                                *
C.    *  Stores vertex parameters.                                     *
C.    *                                                                *
C.    *  Input:                                                        *
C.    *    V         array of (x,y,z) position of the vertex           *
C.    *    NTBEAM    beam track number origin of the vertex            *
C.    *            =0 if none exists                                   *
C.    *    NTTARG    target track number origin of the vertex          *
C.    *    UBUF      user array of NUBUF floating point numbers        *
C.    *    NUBUF                                                       *
C.    *                                                                *
C.    *  Output:                                                       *
C.    *    NVTX      new vertex number (=0 in case of error).          *
C.    *                                                                *
C.    *    ==>Called by : <USER>, GUKINE,GUSTEP                        *
C.    *       Authors    R.Brun, F.Carena, M.Hansroul  *********       *
C.    *                                                                *
C.    *    JGL 1/8/2020 - add multiple KINE/VERT banks to permit       *
C.    *    larger than 64K track/vertex entries.                       *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gcpush.inc"
#include "geant321/gctrak.inc"
      DIMENSION V(3),UBUF(1)
      INTEGER LGKINE, LGVERT, J, JK, NT(2), JVERT, NVTXI, NPUSH, JV
C.
C.    ------------------------------------------------------------------
C.
      NVTX   = NVERTX + 1
      IF (JVERTX.EQ.0)THEN
         CALL MZBOOK(IXDIV,JVERTX,JVERTX,1,'VERT',NCVERT,NCVERT,2,2,0)
         IQ(JVERTX+2)=0
      ENDIF
      J   = LgVERT(JVERT,NVTX)
      NVTXI = NVTX-IQ(JVERT+2)

*
*     Book another bank if >=64k entries in current bank
      IF (NVTXI.GT.63999) then
*         PRINT *, 'Booking additional VERT bank in gsvert.F = ', JVERT
         CALL MZBOOK(IXDIV,JVERT,JVERT,0,'VERT',NCVERT,NCVERT,2,2,0)
         IQ(JVERT+2)=NVERTX
         J   = LgVERT(JVERT,NVTX)
         NVTXI = NVTX-IQ(JVERT+2)
      endif

      IF(NVTXI.GT.IQ(JVERT-2)) THEN 
         NPUSH=min(NPVERT*int(Alog(NVTX+2.)),63999-IQ(JVERT-2))
         CALL MZPUSH(IXDIV,JVERT,NPUSH,0,'I')
*     Update saved pointer to current bank (LGKINE takes care of rest)
*     This avoids a saved pointer to a dead bank
         JVERTX = JVERT
      ENDIF
      IF (NVTXI.GT.IQ(JVERT-2)) THEN
         PRINT *,' GSVERT ERROR: WRONG NVertex ',NVTX
         GOTO 90
      ENDIF

      NVERTX = NVTX
      IQ(JVERT+1)=NVERTX
      CALL MZBOOK(IXDIV,JV,JVERT,-NVTXI,'VERT',1,1,9,3,0)

      IF(NWBUF.GT.0)THEN
         CALL MZBOOK(IXDIV,JUV,JV,-1,'VERU',0,0,NWBUF,3,0)
         IQ(JUV-5)=NVTX
         DO 3 I=1,NWBUF
   3     Q(JUV+I)=UBUF(I)
      ENDIF
      DO 4 I=1,3
   4  Q(JV + I) = V(I)
      Q(JV + 4) = TOFG
      Q(JV + 5) = NTBEAM
      Q(JV + 6) = NTTARG

      NT(1) = NTBEAM
      NT(2) = NTTARG
      DO I=1,2
         IF (NT(I).GT.0) THEN
            JK   = LGKINE(J,NT(I))
            IF(JK.LE.0) GO TO 90
            NVG  = Q(JK + 7)
            NFREE=IQ(JK-1)-7-NVG
            IF(NFREE.LE.0) CALL MZPUSH(IXDIV,JK,0,max(2,NVG/5),'I')
            Q(JK + NVG + 8) = NVTX
            Q(JK + 7) = NVG + 1
         END IF
      END DO

      GO TO 99
C
C             Error
C
  90  NVTX   = 0
  99  RETURN
      END
