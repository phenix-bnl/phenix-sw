*
* $Id: gfkine.F,v 1.1 2020/01/29 20:06:17 lajoie Exp $
*
* $Log: gfkine.F,v $
* Revision 1.1  2020/01/29 20:06:17  lajoie
* added ability to handle multiple linked KINE and VERT banks
*
* Revision 1.2  1998/02/10 16:04:18  japost
*   Comments are correctly redistributed.
*
* Revision 1.1.1.1  1995/10/24 10:21:18  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
*-- Author :
      SUBROUTINE GFKINE(ITRA,VERT,PVERT,IPART,NVERT,UBUF,NWBUF)
*
************************************************************************
*                                                                      *
*           Retrieves Vertex and Track parameters                      *
*           -------------------------------------                      *
*                                                                      *
*  Retrieves parameters for a track with a long lifetime.              *
*                                                                      *
*  Input:                                                              *
*    ITRA      track number for which parameters are requested         *
*                                                                      *
*  Output:                                                             *
*    VERT      vector origin of the track                              *
*    PVERT     4 momentum components at the track origin               *
*    IPART     particle type (=0 if track ITRA does not exist)         *
*    NVERT     vertex number origin of the track                       *
*    UBUF      user words stored in GSKINE.                            *
*                                                                      *
*    ==>Called by : <USER>, GTREVE                                     *
*       Author    R.Brun  *********                                    *
*                                                                      *
*     JGL 1/8/2020 - add multiple KINE banks to permit larger          *
*    than 64K track entries.                                           *
*                                                                      *
************************************************************************
*
#include "geant321/gcbank.inc"
#include "geant321/gcnum.inc"
#include "geant321/gctrak.inc"
      DIMENSION VERT(3),PVERT(4),UBUF(*)
      INTEGER LGVERT, LGKINE
C.
C.    ------------------------------------------------------------------
C.
      IPART  = 0
      IF (JKINE.LE.0)GO TO 99
      IF (ITRA.LE.0)GO TO 99
      IF (ITRA.GT.NTRACK)GO TO 99
C
c select kine bank, get pointer
      JK = LgKINE(JKINE,ITRA)
      IF (JK.LE.0) THEN
c         PRINT *,'gfkine.f: JK<=0'
         GO TO 99
      END IF
      NV = Q(JK + 6)
      IF (JVERTX.LE.0) THEN
c         PRINT *,'gfkine.f: JVERTX<=0'
         GO TO 99
      END IF
      JV = LgVERT(JVERTX,NV)
      IF (JV.LE.0) THEN
c         PRINT *,'gfkine.f: JV<=0'
         GO TO 99
      END IF
      DO 10 I=1,3
  10  VERT(I) = Q(JV + I)
*     TOFG=Q(JV+4)
C
      DO 20 I=1,4
  20  PVERT(I) = Q(JK + I)
      IPART = Q(JK + 5)
      NVERT = Q(JK + 6)
      NLINK = IQ(JK-2)
      NWBUF  = 0
      IF (NLINK.LE.0)GO TO 99
      JKU = LQ(JK- 1)
      IF (JKU.LE.0)GO TO 99
      NWBUF = IQ(JKU-1)
      DO 30 I=1,NWBUF
  30  UBUF(I)=Q(JKU+I)
C
  99  RETURN
      END
