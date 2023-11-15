**
      SUBROUTINE GXPICT
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      HIGZ/PICTURES control commands                            *
C.    *                                                                *
C.    *       Authors:   R.Brun      **********                        *
C.    *                                                                *
C.    ******************************************************************
C.
*KEEP,GCXLUN.
#include "gcxlun.inc"
*KEND.
      CHARACTER*20 CHOPT
      CHARACTER*80 CHTITL, CHTEMP
      CHARACTER*64 CHKEY
      CHARACTER*32 CHPATL
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)

      IF(CHPATL.EQ.'FILE')THEN
         CALL KUGETI(LUN)
         CALL GXLUNF(LUN,1,IFREE)
         IF(IFREE.NE.0)GO TO 99
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETI(LRECL)
         CALL KUGETC(CHOPT,NCH)
         CALL RZOPEN(LUN,CHKEY,CHTITL,'W'//CHOPT,LRECL,ISTAT)
         CALL IZFILE(LUN,CHKEY,CHOPT)
         LUNIT(LUN)=4
         GO TO 99
      ENDIF

C           LIST

      IF(CHPATL.EQ.'LIST')THEN
         CALL IZPICT(' ','L')
         GO TO 99
      ENDIF

C           DELETE

      IF(CHPATL.EQ.'DELETE')THEN
         CALL KUGETC(CHTITL ,NCH)
         CALL IZPICT(CHTITL,'S')
         GO TO 99
      ENDIF

C           SCRATCH

      IF(CHPATL.EQ.'SCRATCH')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETI(ICYCLE)
         CALL IZSCR(CHTITL,ICYCLE)
         GO TO 99
      ENDIF

C           PLOT

      IF(CHPATL.EQ.'PLOT')THEN
         CALL KUGETC(CHTITL,NCH)
         IF(NCH.GT.0)THEN
            LP=IZRPIP(CHTITL)
            IF(LP.EQ.0)THEN
               CALL IZIN(CHTITL,9999)
            ENDIF
         ENDIF
         CALL IZPICT(CHTITL,'D')
         GO TO 99
      ENDIF

C           RENAME

      IF(CHPATL.EQ.'RENAME')THEN
         CALL KUGETC(CHTITL ,NCH)
         CALL KUGETC(CHTEMP,NCH)
         CALL IZCOPY(CHTITL,CHTEMP,'R')
         GO TO 99
      ENDIF

C           IZOUT

      IF(CHPATL.EQ.'IZOUT')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL IZOUT(CHTITL,ICYCLE)
         GO TO 99
      ENDIF

C           IZIN

      IF(CHPATL.EQ.'IZIN')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETI(ICYCLE)
         CALL IZIN(CHTITL,ICYCLE)
         GO TO 99
      ENDIF

C           IGSET

      IF(CHPATL.EQ.'IGSET')THEN
         CALL KUGETC(CHTITL,NCH)
         XT=0.
         IF(CHTITL.NE.'SHOW')CALL KUGETR(XT)
         CALL IGSET(CHTITL,XT)
         GO TO 99
      ENDIF

  99  END
