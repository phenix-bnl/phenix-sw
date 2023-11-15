*CMZ :  2.04/00 31/03/93  14.08.26  by  S.R.Tonse
*-- Author :
*-- Author :
      SUBROUTINE GSSTAK (IFLAG)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *    SUBR. GSSTAK (IFLAG)                                        *
C.    *                                                                *
C.    *   Stores in auxiliary stack JSTAK the particle currently       *
C.    *    described in common /GCKINE/.                               *
C.    *                                                                *
C.    *   On request, creates also an entry in structure JKINE :       *
C.    *    IFLAG =                                                     *
C.    *     0 : No entry in JKINE structure required (user)            *
C.    *     1 : New entry in JVERTX / JKINE structures required (user) *
C.    *    <0 : New entry in JKINE structure at vertex -IFLAG (user)   *
C.    *     2 : Entry in JKINE structure exists already (from GTREVE)  *
C.    *                                                                *
C.    *   Called by : GSKING, GTREVE                                   *
C.    *   Author    : S.Banerjee, F.Bruyant                            *
C.    *                                                                *
C.    ******************************************************************
C.
C  modified 19-JUL-1991 (see SECUBUF) SRTonse to include user words UBUF
C  into KINE data bank.
*KEEP,GCBANK.
#include "gcbank.inc"
*KEEP,GCKINE.
#include "gckine.inc"
*KEEP,GCMZFO.
#include "gcmzfo.inc"
*KEEP,GCNUM.
#include "gcnum.inc"
*KEEP,GCSTAK.
#include "gcstak.inc"
*KEEP,GCTRAK.
#include "gctrak.inc"
*KEEP,SECUBUF.
#include "secubuf.inc"
*KEND.

c      NUBUF is in a common block, so one cannot use a DATA statement
c      NUBUF should be set whenever one makes a specific all to GSSTAK

C      DATA NUBUF/0/        !see secubuf.inc (can't have this with IBM)


      IF (IPART.LE.0.OR.IPART.GT.NPART) THEN
         PRINT *, ' GSSTAK - Unknown particle code, skip track ', IPART
         GO TO 999
      ENDIF

* *** Give control to user for track selection

      CALL GUSKIP (ISKIP)
      IF (ISKIP.NE.0) GO TO 999

* *** Check if an entry in JKINE structure is required

      IF (IFLAG.EQ.1) THEN
         CALL GSVERT (VERT, ITRA, 0, 0, 0, NVTX)
         CALL GSKINE (PVERT, IPART, NVTX, UBUF,NUBUF, ITR)
      ELSE IF (IFLAG.LT.0) THEN
         NVTX = -IFLAG
         CALL GSKINE (PVERT, IPART, NVTX, UBUF,NUBUF,ITR)
      ELSE
         IF (IFLAG.EQ.0) THEN
*          Store -ITRA in stack for a track without entry in JKINE
            ITR = -ITRA
         ELSE
            ITR = ITRA
         ENDIF
      ENDIF

* *** Store information in stack

      IF (JSTAK.EQ.0) THEN
         NDBOOK = NTSTKP*NWSTAK +3
         NDPUSH = NTSTKS*NWSTAK
         CALL MZBOOK (IXCONS,JSTAK,JSTAK,1,'STAK', 0,0,NDBOOK, IOSTAK,3)
         IQ(JSTAK+2) = NTSTKP
      ELSE IF (IQ(JSTAK+1).EQ.IQ(JSTAK+2)) THEN
         CALL MZPUSH (IXCONS, JSTAK, 0, NDPUSH, 'I')
         IQ(JSTAK+2) = IQ(JSTAK+2) +NTSTKS
      ENDIF

      JST = JSTAK +IQ(JSTAK+1)*NWSTAK +3
      IQ(JSTAK+1) = IQ(JSTAK+1) +1
      IF (IQ(JSTAK+3).EQ.0) IQ(JSTAK+3) = IQ(JSTAK+1)
      IF (IQ(JSTAK+1).GT.NSTMAX)  NSTMAX = IQ(JSTAK+1)

      IQ(JST+1)   = ITR
      IQ(JST+2)   = IPART
      IQ(JST+3)   = 0
      DO 90 I = 1,3
         Q(JST+3+I) = VERT(I)
         Q(JST+6+I) = PVERT(I)
   90 CONTINUE
      Q(JST+10) = TOFG
      Q(JST+11) = SAFETY
      Q(JST+12) = UPWGHT

      NALIVE = NALIVE +1
*                                                             END GSSTAK
  999 END
