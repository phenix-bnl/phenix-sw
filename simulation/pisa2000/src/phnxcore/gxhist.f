**
      SUBROUTINE GXHIST
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      HISTOGRAM control commands                                *
C.    *                                                                *
C.    *       Authors:   R.Brun      **********                        *
C.    *                                                                *
C.    ******************************************************************
C.
*KEEP,GCXLUN.
#include "gcxlun.inc"
*KEND.
      COMMON/CWK/IWK
      CHARACTER*20 CHOPT
      CHARACTER*64 CHPATH
      CHARACTER*64 CHKEY
      CHARACTER*32 CHPATL
      DIMENSION PAR(10)
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)

      IF(CHPATL.EQ.'FILE')THEN
         CALL KUGETI(LUN)
         CALL GXLUNF(LUN,1,IFREE)
         IF(IFREE.NE.0)GO TO 99
         CALL KUGETC(CHPATH,NCH)
         CALL KUGETI(LRECL)
         CALL KUGETC(CHOPT,NCH)
         CALL RZOPEN(LUN,CHKEY,CHPATH,'W'//CHOPT,LRECL,ISTAT)
         IF(ISTAT.NE.0)THEN
            PRINT *,' Cannot open HBOOK file'
            GO TO 99
         ENDIF
         CALL HRFILE(LUN,CHKEY,CHOPT)
         LUNIT(LUN)=3
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'LIST')THEN
         CALL KUALFA
         CALL KUGETC(CHOPT,NCH)
         CALL HLDIR(' ',CHOPT)
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'DELETE')THEN
         CALL KUGETI(ID)
         CALL HDELET(ID)
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'PLOT')THEN
         CHOPT=' '
         CALL KUGETI(ID)
         CALL KUGETC(CHOPT,NCH)
         IF(IWK.NE.0)THEN
            CALL HPLOT(ID,CHOPT,' ',0)
         ENDIF
         IF(IWK.EQ.0.OR.IWK.EQ.-2)THEN
            CALL HPRINT(ID)
         ENDIF
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'LEGO')THEN
         CALL KUGETI(ID)
         CALL KUGETR(THETA)
         CALL KUGETR(PHI)
         PAR(1)=THETA
         PAR(2)=PHI
         CALL HPLTAB(ID,2,PAR,'LEGO')
         CALL HPLFIL
         CALL HPLDAT
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'HRIN')THEN
         CALL KUGETI(ID)
         CALL HRIN(ID,9999,0)
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'HROUT')THEN
         CALL KUGETI(ID)
         CALL KUGETC(CHOPT,NCH)
         CALL HROUT(ID,ICYCLE,CHOPT)
         GO TO 99
      ENDIF

      IF(CHPATL.EQ.'PUT')THEN
         CALL KUGETI(ID)
         CALL KUGETC(CHPATH,NCH)
         CALL KUGETC(CHOPT,NCH)
         CALL HRPUT(ID,CHPATH,CHOPT)
         GO TO 99
      ENDIF

C             ZONE

      IF(CHPATL.EQ.'ZONE')THEN
         CALL KUGETI(NX)
         CALL KUGETI(NY)
         CALL KUGETI(IFIRST)
         CALL KUGETC(CHOPT,NCH)
         CALL HPLZON(NX,NY,IFIRST,CHOPT)
         GO TO 99
      ENDIF

C             SET

      IF(CHPATL.EQ.'SET')THEN
         CALL KUGETC(CHOPT,NCH)
         CALL KUGETR(VAL)
         CALL HPLSET(CHOPT,VAL)
         GO TO 99
      ENDIF

C             OPTION

      IF(CHPATL.EQ.'OPTION')THEN
         CALL KUGETC(CHOPT,NCH)
         CALL HPLOPT(CHOPT,1)
         GO TO 99
      ENDIF

C             NULL

      IF(CHPATL.EQ.'NULL')THEN
         IF(NPAR.EQ.0)THEN
            CALL HPLNUL
            GO TO 99
         ELSE
            CALL KUGETR(XMIN)
            CALL KUGETR(XMAX)
            CALL KUGETR(YMIN)
            CALL KUGETR(YMAX)
            CALL KUGETC(CHOPT,NCH)
            CALL HPLFRA(XMIN,XMAX,YMIN,YMAX,CHOPT)
         ENDIF
      ENDIF

  99  END
