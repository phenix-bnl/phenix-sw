 
**
      SUBROUTINE GXFORT
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      COMIS/FORTRAN control commands                            *
C.    *                                                                *
C.    *       Authors:   R.Brun      **********                        *
C.    *                                                                *
C.    ******************************************************************
C.
*KEEP,GCBANK.
#include "gcbank.inc"
*KEEP,GCOMIS.
#include "gcomis.inc"
*KEEP,GCXLUN.
#include "gcxlun.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEND.
*KEEP, HCDIRE.
      PARAMETER (NLPATM=100)
      COMMON /HCDIRN/NLCDIR,NLNDIR,NLPAT,ICDIR,NCHTOP,ICHTOP(20)
      CHARACTER*16   CHNDIR,    CHCDIR,    CHPAT    ,CHTOP
      COMMON /HCDIRC/CHCDIR(NLPATM),CHNDIR(NLPATM),CHPAT(NLPATM)
     +              ,CHTOP(NLPATM)
      CHARACTER*80 HFNAME
      COMMON /HCFILE/HFNAME(20)

*KEEP,HIMETA
      COMMON /HIMETA/ IDMETA,XMFACT,YMFACT,TEKACT,METACT,FILOPN
      LOGICAL TEKACT,METACT,FILOPN

      COMMON/QUEST/IQUEST(100)
      CHARACTER*80 CHFILE,CHTITL
      CHARACTER*32 CHPATL,CHTEMP
      COMMON/CSUNIT/LUNINP,LUNPM,LUNFIL,LUNLOG,LUNMAP,LUNLIB,
     +                     ISTPM,ISTFIL,ISTLOG,ISTMAP,ISTLIB
      CHARACTER*32 CHPATH(10),CHFORM
      CHARACTER*1 TYPE
      DIMENSION ITOK(2,10),IPAR(10),RPAR(10),LAD(10)
      LOGICAL OPENED
      INTEGER CSADDR
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)

      IF(CHPATL.EQ.'FILE')THEN
         CALL KUGETI(LUN)
         CALL GXLUNF(LUN,1,IFREE)
         IF(IFREE.EQ.0)THEN
            CALL KUGETC(CHFILE,NCH)
            IF(NCH.GT.0)THEN
               CALL KUOPEN(LUN,CHFILE,'UNKNOWN',ISTAT)
            ENDIF
            LUNIT(LUN)=9
         ENDIF

      ELSE IF(CHPATL.EQ.'CLOSE')THEN
         CALL KUGETI(LUN)
         CALL GXLUNF(LUN,2,IFREE)
         IF(IFREE.EQ.0)THEN
            INQUIRE(LUN,OPENED=OPENED)
            IF(OPENED) THEN
               WRITE(CHMAIL,10000) LUN
10000 FORMAT(' Unit ',I3,' will be closed via a FORTRAN close only')
               CALL GMAIL(0,0)
               CALL GXCLOS(LUN)
            ENDIF
         ELSEIF(IFREE.NE.6.AND.IFREE.NE.7) THEN
            IF(LUNIT(LUN).EQ.1)THEN
               CALL FZENDI(LUN,'T')

            ELSEIF(LUNIT(LUN).EQ.2)THEN
               CALL FZENDO(LUN,'T')

            ELSEIF(LUNIT(LUN).GT.2.AND.LUNIT(LUN).LT.6)THEN
               DO 10 I=2,NCHTOP
                  IF(ICHTOP(I).EQ.LUN)THEN
                     CHTEMP=CHTOP(I)
                     CALL HREND(CHTEMP)
                     GO TO 20
                  ENDIF
   10          CONTINUE
               CHTEMP=' '
               IF(LUN.LT.10) THEN
                  WRITE(CHTEMP,10100)LUN
               ELSEIF(LUN.GE.10) THEN
                  WRITE(CHTEMP,10200)LUN
               ENDIF
10100       FORMAT('LUN',I1,'    ')
10200       FORMAT('LUN',I2,'   ')
               CALL RZEND(CHTEMP)

            ELSEIF(LUNIT(LUN).EQ.8)THEN
               CALL IGMETA(0,0)
               CALL ICLWK(IDMETA)
               FILOPN=.FALSE.

            ENDIF
   20       LUNIT(LUN)=0
            CALL GXCLOS(LUN)
         ENDIF

      ENDIF

      END
