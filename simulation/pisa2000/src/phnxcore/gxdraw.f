 
**
      SUBROUTINE GXDRAW
C.
C.    ******************************************************************
C.    *                                                                *
C.    *      Drawing commands                                          *
C.    *                                                                *
C.    *       Authors:   R.Brun      **********                        *
C.    *                  P.Zanarini  **********                        *
C.    *                  S.Giani     **********                        *
C.    *                                                                *
C.    ******************************************************************
C.
*KEEP,GCBANK.
#include "gcbank.inc"
*KEEP,PAWC.
#include "pawc.inc"
*KEEP,GCUNIT.
#include "gcunit.inc"
*KEEP,GCDRAW.
#include "gcdraw.inc"
*KEEP,GCGOBJ.
#include "gcgobj.inc"
*KEEP,GCMUTR.
#include "gcmutr.inc"
*KEEP,GCSPEE.
#include "gcspee.inc"
*KEEP,GCCURS.
#include "gccurs.inc"
*KEEP,GCHIL2.
#include "gchil2.inc"
*KEEP,GCURSB.
#include "gcursb.inc"
*KEND.

      COMMON/QUEST/IQUEST(100)

      DIMENSION NNAME(15),NNUMB(15),RVAL(2)
*SG
      DIMENSION VX(4),VXX(4),VVX(4),XV(4),BX(4)
      DIMENSION VY(4),VYY(4),VVY(4),YV(4),BY(4)
      CHARACTER*4 NAME,CHNUMB,IDS,IVS,ICS,NNVV,NVNV
      CHARACTER*4 CHNRS,CHAX,YESNO,CENT
      CHARACTER*4 NOPT,SAMP,KSAM,KLSA
      CHARACTER*6 MODE
*SG
      CHARACTER*80 CHTEXT
      CHARACTER*32 CHPATL,VNAME,FILL
      CHARACTER*64 NAMNUM
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)

      IF (CHPATL.EQ.'BOX ') THEN
         IHOLE=0

*    It is now possible to clip different volumes by different SHAPES !
*    Moreover, one can clip twice each volume by different SHAPES !

         NCVOLS=NCVOLS+1
         IF(NCVOLS.EQ.MULTRA)THEN
            WRITE(CHMAIL, 10000)
10000      FORMAT(' *** GXDRAW ***:',
     +            ' No more space to store MCVOL information.')
            CALL GMAIL(0,0)
            WRITE(CHMAIL, 10100)
10100      FORMAT(' *** GXDRAW ***: Please reset MCVOL')
            CALL GMAIL(0,0)
            GOTO 999
         ENDIF
         CALL KUGETC(NNVV,NCH)
***SG
         CALL KUGETR(XMIN)
         CALL KUGETR(XMAX)
         CALL KUGETR(YMIN)
         CALL KUGETR(YMAX)
         CALL KUGETR(ZMIN)
         CALL KUGETR(ZMAX)
         IF(XMIN.GE.XMAX.OR.YMIN.GE.YMAX.OR.ZMIN.GE.ZMAX)THEN
            PRINT *,' Wrong Box limits. Check values '
            GOTO 999
         ENDIF
****SG
         GNNVV(NCVOLS)=NNVV
         GNASH(NCVOLS)='BOX'
         GXMIN(NCVOLS)=XMIN
         GXMAX(NCVOLS)=XMAX
         GYMIN(NCVOLS)=YMIN
         GYMAX(NCVOLS)=YMAX
         GZMIN(NCVOLS)=ZMIN
         GZMAX(NCVOLS)=ZMAX
         IF(GXMIN(NCVOLS).GT.-99999.)IHOLE=1
* Resetting Mcvol mode
         IF(GNNVV(NCVOLS).EQ.'.')THEN
            IHOLE=0
            DO 10 JJ=1,NCVOLS
               GNNVV(JJ)=' '
               GXMIN(JJ)=-100000
               GXMAX(JJ)=-99999
               GYMIN(JJ)=-100000
               GYMAX(JJ)=-99999
               GZMIN(JJ)=-100000
               GZMAX(JJ)=-99999
   10       CONTINUE
            NCVOLS=0
         ENDIF
      ELSEIF (CHPATL.EQ.'TUBE')THEN
         IHOLE=0

*    It is now possible to clip different volumes by different SHAPES !
*    Moreover, one can clip twice each volume by different SHAPES !

         NCVOLS=NCVOLS+1
         IF(NCVOLS.EQ.MULTRA)THEN
            WRITE(CHMAIL, 10000)
            CALL GMAIL(0,0)
            WRITE(CHMAIL, 10100)
            CALL GMAIL(0,0)
            GOTO 999
         ENDIF
         CALL KUGETC(NNVV,NCH)
***SG
         CALL KUGETR(RMAX)
         CALL KUGETR(ZDEM)
         CALL KUGETR(XMED)
         CALL KUGETR(YMED)
         CALL KUGETR(ZMED)
****SG
         GNNVV(NCVOLS)=NNVV
         GNASH(NCVOLS)='TUBE'
         GXMIN(NCVOLS)=RMAX
         GXMAX(NCVOLS)=ZDEM
         GYMIN(NCVOLS)=XMED
         GYMAX(NCVOLS)=YMED
         GZMIN(NCVOLS)=ZMED
         GZMAX(NCVOLS)=0.
         IF(GXMIN(NCVOLS).GT.-99999.)IHOLE=1
*Resetting Mcvol mode
         IF(GNNVV(NCVOLS).EQ.'.')THEN
            IHOLE=0
            DO 20 JJ=1,NCVOLS
               GNNVV(JJ)=' '
               GXMIN(JJ)=0.1
               GXMAX(JJ)=0.1
               GYMIN(JJ)=-100000
               GYMAX(JJ)=-100000
               GZMIN(JJ)=-100000
               GZMAX(JJ)=0.
   20       CONTINUE
            NCVOLS=0
         ENDIF
 
      ELSEIF (CHPATL.EQ.'CONE')THEN
         IHOLE=0

*    It is now possible to clip different volumes by different SHAPES !
*    Moreover, one can clip twice each volume by different SHAPES !

         NCVOLS=NCVOLS+1
         IF(NCVOLS.EQ.MULTRA)THEN
            WRITE(CHMAIL, 10000)
            CALL GMAIL(0,0)
            WRITE(CHMAIL, 10100)
            CALL GMAIL(0,0)
            GOTO 999
         ENDIF
         CALL KUGETC(NNVV,NCH)
***SG
         CALL KUGETR(RMAX1)
         CALL KUGETR(RMAX2)
         CALL KUGETR(ZDEM)
         CALL KUGETR(XMED)
         CALL KUGETR(YMED)
         CALL KUGETR(ZMED)
****SG
         GNNVV(NCVOLS)=NNVV
         GNASH(NCVOLS)='CONE'
         GXMIN(NCVOLS)=RMAX1
         GXMAX(NCVOLS)=RMAX2
         GYMIN(NCVOLS)=ZDEM
         GYMAX(NCVOLS)=XMED
         GZMIN(NCVOLS)=YMED
         GZMAX(NCVOLS)=ZMED
         IF(GXMIN(NCVOLS).GT.-99999.)IHOLE=1
*Resetting Mcvol mode
         IF(GNNVV(NCVOLS).EQ.'.')THEN
            IHOLE=0
            DO 30 JJ=1,NCVOLS
               GNNVV(JJ)=' '
               GXMIN(JJ)=0.1
               GXMAX(JJ)=0.1
               GYMIN(JJ)=0.1
               GYMAX(JJ)=-100000
               GZMIN(JJ)=-100000
               GZMAX(JJ)=-100000
   30       CONTINUE
            NCVOLS=0
         ENDIF
 
      ELSEIF (CHPATL.EQ.'SPHE')THEN
         IHOLE=0

*    It is now possible to clip different volumes by different SHAPES !
*    Moreover, one can clip twice each volume by different SHAPES !

         NCVOLS=NCVOLS+1
         IF(NCVOLS.EQ.MULTRA)THEN
            WRITE(CHMAIL, 10000)
            CALL GMAIL(0,0)
            WRITE(CHMAIL, 10100)
            CALL GMAIL(0,0)
            GOTO 999
         ENDIF
         CALL KUGETC(NNVV,NCH)
***SG
         CALL KUGETR(RMAX)
         CALL KUGETR(XMED)
         CALL KUGETR(YMED)
         CALL KUGETR(ZMED)
****SG
         GNNVV(NCVOLS)=NNVV
         GNASH(NCVOLS)='SPHE'
         GXMIN(NCVOLS)=RMAX
         GXMAX(NCVOLS)=XMED
         GYMIN(NCVOLS)=YMED
         GYMAX(NCVOLS)=ZMED
         IF(GXMIN(NCVOLS).GT.-99999.)IHOLE=1
*Resetting Mcvol mode
         IF(GNNVV(NCVOLS).EQ.'.')THEN
            IHOLE=0
            DO 40 JJ=1,NCVOLS
               GNNVV(JJ)=' '
               GXMIN(JJ)=0.1
               GXMAX(JJ)=-100000
               GYMIN(JJ)=-100000
               GYMAX(JJ)=-100000
   40       CONTINUE
            NCVOLS=0
         ENDIF

      ELSEIF (CHPATL.EQ.'DRAW') THEN
         CALL KUGETC(NAME,NCH)
         CALL KUGETR(GTHETA)
         CALL KUGETR(GPHI)
         CALL KUGETR(GPSI)
         CALL KUGETR(GU0)
         CALL KUGETR(GV0)
         CALL KUGETR(GSCU)
         CALL KUGETR(GSCV)
         CALL GDRAW(NAME,GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV)

      ELSEIF (CHPATL.EQ.'DVOLUME') THEN
         CALL KUGETI(N)
         IF (N.EQ.0) GO TO 60
         IF (N.LT.0.OR.N.GT.15) GO TO 999

         CALL KUGETC(CHTEXT,NCH)
         DO 50 I=1,N
            CALL KUGETL(NAMNUM,NCH)
            CALL UCTOH(NAMNUM,NNAME(I),4,4)
            CALL KUGETL(CHNUMB,NCH)
            CALL KICTON(CHNUMB,NNUMB(I),RVAL)
            IF (IQUEST(1).NE.0) GO TO 999
   50    CONTINUE

         CALL KUGETC(CHNRS,NCH)
         NRS=0
         IF (CHNRS.EQ.'DRS') NRS=1
         NBASE=N*2+1
         CALL KUGETR(GTHETA)
         CALL KUGETR(GPHI)
         CALL KUGETR(GPSI)
         CALL KUGETR(GU0)
         CALL KUGETR(GV0)
         CALL KUGETR(GSCU)
         CALL KUGETR(GSCV)
   60    CALL GDRVOL(N,NNAME,NNUMB,NRS,GTHETA,GPHI,GPSI,GU0,GV0,GSCU,
     +   GSCV)

      ELSEIF (CHPATL.EQ.'DCUT') THEN
         IHOLE=0
         CALL KUGETC(NAME,NCH)
         CALL KUGETC(CHAX,NCH)
         IF (CHAX.EQ.'X'.OR.CHAX.EQ.'1') THEN
            IAX=1
         ELSEIF (CHAX.EQ.'Y'.OR.CHAX.EQ.'2')THEN
            IAX=2
         ELSEIF (CHAX.EQ.'Z'.OR.CHAX.EQ.'3')THEN
            IAX=3
         ENDIF
         CALL KUGETR(CCUT)
         CALL KUGETR(GU0)
         CALL KUGETR(GV0)
         CALL KUGETR(GSCU)
         CALL KUGETR(GSCV)
         CALL GDRAWC(NAME,IAX,CCUT,GU0,GV0,GSCU,GSCV)

      ELSEIF (CHPATL.EQ.'DXCUT') THEN
         CALL KUGETC(NAME,NCH)
         CALL KUGETR(CUTTHE)
         CALL KUGETR(CUTPHI)
         CALL KUGETR(CCUT)
         CALL KUGETR(GTHETA)
         CALL KUGETR(GPHI)
         CALL KUGETR(GU0)
         CALL KUGETR(GV0)
         CALL KUGETR(GSCU)
         CALL KUGETR(GSCV)
         CALL GDRAWX(NAME,CUTTHE,CUTPHI,CCUT,GTHETA,GPHI,GU0,GV0,GSCU,
     +   GSCV)

***SG


*   It is now possible to shift each volume into a more visible place !

      ELSEIF(CHPATL.EQ.'SHIFT') THEN
         IF(NSHIFT.EQ.0)KSHIFT=1
         NSHIFT=NSHIFT+1
         IF(NSHIFT.EQ.MULTRA)THEN
            WRITE(CHMAIL, 10200)
10200      FORMAT(' *** GXDRAW ***:',
     +            ' No more space to store SHIFT information.')
            CALL GMAIL(0,0)
            GOTO 999
         ENDIF
         CALL KUGETC(NVNV,NCH)
         CALL KUGETR(XXXX)
         CALL KUGETR(YYYY)
         CALL KUGETR(ZZZZ)
         GNVNV(NSHIFT)=NVNV
         GXXXX(NSHIFT)=XXXX
         GYYYY(NSHIFT)=YYYY
         GZZZZ(NSHIFT)=ZZZZ
*   Resetting Shift mode
         IF(GNVNV(NSHIFT).EQ.'.')THEN
            KSHIFT=0
            DO 70 KK=1,NSHIFT
               GNVNV(KK)=' '
               GXXXX(KK)=0
               GYYYY(KK)=0
               GZZZZ(KK)=0
   70       CONTINUE
            NSHIFT=0
         ENDIF

*  To make the detector 'explode'

      ELSEIF(CHPATL.EQ.'BOMB')THEN
         CALL KUGETR(BOOM)
         GBOOM=BOOM

***SG

      ELSEIF (CHPATL.EQ.'DTREE') THEN
*         JSIM=0
         KXXX=0
         NNPAR=NPAR
         ICHAR=1000
         IGZFLA=0
         CALL KUGETC(NAME,NCH)
         CALL UHTOC(IQ(JVOLUM+1),4,MOMO,4)
         CALL KUGETI(LEVMAX)
         IF(NNPAR.EQ.3)THEN
           CALL KUGETI(ISELT)
           IISELT=ISELT
         ELSE
           ISELT=111
         ENDIF
   80    CONTINUE
         IWTY=IGIWTY(1)
         JVSIM=2
         IF(IWTY.GT.10.OR.IWTY.LT.1)JVSIM=1
        IF (NAME.EQ.'    ')NAME=MOMO
        IF (NAME.NE.MOMO) THEN
          INTFLA=10
          CALL GDTREE(MOMO,0,110)
          DO 93 J=1,NUMND2
            IQ(JFINAM+J)=IQ(JNAM1+J)
            IQ(JFISCA+J)=IQ(JSCA1+J)
            IQ(JFIMOT+J)=IQ(JMOT1+J)
  93      CONTINUE
          KXXX=1
        ELSE
          INTFLA=10
          CALL GDTREE(NAME,0,110)
          DO 94 J=1,NUMND2
            IQ(JFINAM+J)=IQ(JNAM1+J)
            IQ(JFISCA+J)=IQ(JSCA1+J)
            IQ(JFIMOT+J)=IQ(JMOT1+J)
  94      CONTINUE
          INTFLA=0
          CALL GDTREE(NAME,LEVMAX,ISELT)
        ENDIF

         CALL GDPLST(JVSIM,NAME,LEVMAX,KXXX)

      ELSEIF (CHPATL.EQ.'DSPEC') THEN
         CALL KUGETC(NAME,NCH)
         CALL GDSPEC(NAME)

      ELSEIF (CHPATL.EQ.'DFSPC') THEN
         CALL KUGETC(NAME,NCH)
         ISORT=0
         CALL KUGETC(YESNO,NCH)
         IF (YESNO.EQ.'Y') ISORT=1
         INTER=1
         CALL KUGETC(MODE,NCH)
         IF (MODE.EQ.'B') INTER=0
         CALL GDFSPC(NAME,ISORT,INTER)

      ELSEIF (CHPATL.EQ.'DTEXT') THEN
         CALL KUGETR(X0)
         CALL KUGETR(Y0)
         CALL KUGETS(CHTEXT,NCH)
         CALL KUGETR(SIZE)
         CALL KUGETR(ANGLE)
         CALL KUGETI(LWID)
         CALL KUGETC(CENT,NCH)
         IF (CENT.EQ.'LEFT'.OR.CENT.EQ.'-1') THEN
            IOPT=-1
         ELSEIF (CENT.EQ.'RIGHT'.OR.CENT.EQ.'1') THEN
            IOPT=1
         ELSE
            IOPT=0
         ENDIF
         CALL IGSET('TXFP',-60.)
         IWTY=IGIWTY(1)
         IF(IWTY.GT.10.OR.IWTY.LT.1)CALL IGSET('TXFP',-61.)
         CALL GDRAWT(X0,Y0,CHTEXT,SIZE,ANGLE,LWID,IOPT)
         CALL IGSET('TXFP',2.)

      ELSEIF (CHPATL.EQ.'DVECTOR') THEN
         CALL KUGETV(VNAME,LPARX,LLL)
         CALL KUGETV(VNAME,LPARY,LLL)
         CALL KUGETI(NP)
         CALL GDRAWV(QQ(LPARX),QQ(LPARY),NP)

      ELSEIF (CHPATL.EQ.'DSCALE') THEN
         CALL KUGETR(X0)
         CALL KUGETR(Y0)
         CALL GDSCAL(X0,Y0)

      ELSEIF (CHPATL.EQ.'DAXIS') THEN
         CALL KUGETR(XX0)
         CALL KUGETR(YY0)
         CALL KUGETR(ZZ0)
         CALL KUGETR(DDX)
         CALL GDAXIS(XX0,YY0,ZZ0,DDX)

      ELSEIF (CHPATL.EQ.'DMAN') THEN
         CALL KUGETR(U0)
         CALL KUGETR(V0)
         CALL KUGETC(MODE,NCH)
         IF (MODE.EQ.'GIRL') THEN
            CALL GDGIRL(UO,VO)
         ELSE
            CALL GDMAN(U0,V0)
         ENDIF

      ELSEIF (CHPATL.EQ.'DHEAD') THEN
         CHRSIZ=0.6
         CALL KUGETS(CHTEXT,NCH)
         CALL KUGETR(CHRSIZ)
         ISELH=111110
         CALL KUGETI(ISELH)
         CALL GDHEAD(ISELH,CHTEXT,CHRSIZ)

      ELSEIF (CHPATL.EQ.'MEASURE') THEN
         CALL IGLOC2(1,NT,U0,V0,U1,V1,ISTAT,'L')
         IF (ISTAT.EQ.0) GO TO 999
         UDIST=(U1-U0)/(GSCU*GZUA)
         VDIST=(V1-V0)/(GSCV*GZVA)
         DIST=SQRT(UDIST*UDIST+VDIST*VDIST)
         WRITE (CHMAIL,'('' MEASURE : '',F9.4,'' CM'')') DIST
         CALL GMAIL(0,0)

      ELSEIF (CHPATL.EQ.'MOVE')  THEN
         IWTY=IGIWTY(1)
         IF(IWTY.LE.10.AND.IWTY.GE.1)THEN
            CONTINUE
            LEP=-ABS(LEP)
            CALL KUGETC(NAME,NCH)
            CALL KUGETC(NOPT,NCH)
            CALL KUGETC(SAMP,NCH)
            VX(1)=0.
            VX(2)=4.
            VX(3)=4.
            VX(4)=0.
            VY(1)=0.
            VY(2)=0.
            VY(3)=1.
            VY(4)=1.
            VXX(1)=4.
            VXX(2)=8.
            VXX(3)=8.
            VXX(4)=4.
            VYY(1)=0.
            VYY(2)=0.
            VYY(3)=1.
            VYY(4)=1.
            VVX(1)=8.
            VVX(2)=12.
            VVX(3)=12.
            VVX(4)=8.
            VVY(1)=0.
            VVY(2)=0.
            VVY(3)=1.
            VVY(4)=1.
            XV(1)=12.
            XV(2)=16.
            XV(3)=16.
            XV(4)=12.
            YV(1)=0.
            YV(2)=0.
            YV(3)=1.
            YV(4)=1.
            BX(1)=16.
            BX(2)=20.
            BX(3)=20.
            BX(4)=16.
            BY(1)=0.
            BY(2)=0.
            BY(3)=1.
            BY(4)=1.
*****           CALL IGSET('DRMD',2.)
            CALL ISFAIS(1)
            CALL GDCOL1(2)
            CALL IFA(4,VX,VY)
            CALL GDCOL1(3)
            CALL IFA(4,VXX,VYY)
            CALL GDCOL1(4)
            CALL IFA(4,VVX,VVY)
            CALL GDCOL1(6)
            CALL IFA(4,XV,YV)
            CALL GDCOL1(7)
            CALL IFA(4,BX,BY)
            AITXCO=5.
            CALL IGSET('TXCI',AITXCO)
            CALL IGSET('TXFP',-60.)
            CALL GDRAWT(2.,.2,'THETA',.7,0.,4,0)
            CALL GDRAWT(6.,.2,'PHI',.7,0.,4,0)
            CALL GDRAWT(10.,.2,'TRASL',.7,0.,4,0)
            CALL GDRAWT(14.,.2,'ZOOM',.7,0.,4,0)
            CALL GDRAWT(18.,.2,'OFF',.7,0.,4,0)
            CALL IGSET('TXFP',2.)
            LLEP=ABS(LEP)
            IF(LLEP.GT.1)THEN
               LCLC=1
            ELSE
               LCLC=0
            ENDIF
            CALL ISFACI(LCLC)
            CALL IGBOX(0.,20.,20.,1.)
            CALL GDRAW(NAME,GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV)
            IF(NOPT(1:1).EQ.'T')CALL GDXYZ(0)
            IF(NOPT(2:2).EQ.'H'.OR.NOPT(1:1).EQ.'H')
     +      CALL GDHITS('*','*',0,0,.2)
            MO=2
*           OOY2=10.
*           OOX2=10.
            OGSCU=GSCU
            OGSCV=GSCV
*         ipx=1
               CALL IGQWK(1,'MXDS',RVAL)
               IXXX=RVAL(1)
               IYYY=RVAL(2)
               IYYY1=(IYYY*19.)/20.
            DO 150 J=1,1000000
               IF(ISTAT.EQ.2.AND.NOPT(1:1).EQ.'T')THEN
                 CALL GKXYZ(-.25)
               ENDIF
               IF(ISTAT.EQ.2.AND.(NOPT(1:1).EQ.'H'.OR.
     +         NOPT(2:2).EQ.'H'))THEN
                 CALL GKHITS('*','*',-.1)
               ENDIF
               CALL IRQLC(1,MO,ISTAT,NT,X2,Y2)
*           CALL ISFAIS(1)
*****           CALL IGSET('DRMD',2.)
               IF(MO.NE.-2)THEN
                  IF(X2.GT.0..AND.X2.LT.4..AND.Y2.LT.1.)NBAR=1
                  IF(X2.GT.4..AND.X2.LT.8..AND.Y2.LT.1.)NBAR=2
                  IF(X2.GT.8..AND.X2.LT.12..AND.Y2.LT.1.)NBAR=3
                  IF(X2.GT.12..AND.X2.LT.16..AND.Y2.LT.1.)NBAR=4
                  IF(X2.GT.16..AND.X2.LT.20..AND.Y2.LT.1.)THEN
                     CALL IGSET('DRMD',1.)
                     LEP=-LEP
                     GO TO 999
                  ENDIF
               ENDIF
*           YY22=ABS(Y2-OOY2)
               GOTO (100,110,120,130), NBAR
  100          CONTINUE
               GTHETA=18.*Y2
               IF(SAMP(1:2).EQ.'ON')MO=-2
               IF(ISTAT.EQ.0.OR.ISTAT.EQ.2)MO=2
*             IF(YY22.LT..2)GOTO 177
*             OOY2=Y2
               GOTO 140
  110          CONTINUE
*             GBOOM=Y2/10.
               GPHI=18.*Y2
               IF(SAMP(1:2).EQ.'ON')MO=-2
               IF(ISTAT.EQ.0.OR.ISTAT.EQ.2)MO=2
*             IF(YY22.LT..2)GOTO 177
*             OOY2=Y2
               GOTO 140
  120          CONTINUE
*             XX22=ABS(X2-OOY2)
               GU0=X2
               GV0=Y2
***             GTHETA=18.*Y2
***             GPHI=18.*Y2
               IF(SAMP(1:2).EQ.'ON')MO=-2
               IF(ISTAT.EQ.0.OR.ISTAT.EQ.2)MO=2
*             IF(YY22.LT..2.AND.XX22.LT..2)GOTO 177
*             OOY2=Y2
*             OOX2=X2
               GOTO 140
  130          CONTINUE
               GSCU=OGSCU*Y2*.25
               GSCV=OGSCV*Y2*.25
               IF(SAMP(1:2).EQ.'ON')MO=-2
               IF(ISTAT.EQ.0.OR.ISTAT.EQ.2)MO=2
*             IF(YY22.LT..2)GOTO 177
*             OOY2=Y2
  140          CONTINUE
*****           CALL IGSET('DRMD',1.)
               CALL IGPXMP(IPX,IXXX,IYYY1,'O')
               CALL ISFACI(LCLC)
               IF(LCLC.NE.0)CALL IGBOX(0.,20.,20.,1.)
               CALL GDRAW(NAME,GTHETA,GPHI,GPSI,GU0,GV0,GSCU,GSCV)
               IF(NOPT(1:1).EQ.'T')CALL GDXYZ(0)
               IF(NOPT(2:2).EQ.'H'.OR.NOPT(1:1).EQ.'H')
     +         CALL GDHITS('*','*',0,0,.2)
               CALL IGPXMP(IPX,0,0,'CDR')
**       CALL GDRAW(NAME,SGT1,SGT2,SGT3,SGT4,SGT5,SGT6,SGT7)
*       CALL GDXYZ(0)
*       CALL GDHITS('*','*',0,-1,.4)
  150       CONTINUE
         ENDIF

      ELSEIF (CHPATL.EQ.'LENS') THEN
         IWTY=IGIWTY(1)
         IF(IWTY.LE.10.AND.IWTY.GE.1)THEN
            CALL KUGETI(KNUM)
            CALL KUGETC(KSAM,NCH)
            KLLM=KNUM
            KLSA=KSAM
            CALL GDLENS(KLLM,KLSA)
         ENDIF

      ELSEIF (CHPATL.EQ.'ZOOM') THEN
         CONTINUE
         ZFU=2.
         CALL KUGETR(ZFU)
         ZFV=ZFU
         CALL KUGETR(ZFV)
         IF(ZFU.EQ.0.OR.ZFV.EQ.0)GO TO 160
         IMODE=1
         CALL KUGETI(IMODE)
         UZ0=PLTRNX*.5
         CALL KUGETR(UZ0)
         VZ0=PLTRNY*.5
         CALL KUGETR(VZ0)
         U0 =UZ0
         CALL KUGETR(U0)
         V0 =U0
         CALL KUGETR(V0)

         IF(IMODE.GT.1000)THEN
            IWTY=IGIWTY(1)
            IF(IWTY.LE.10.AND.IWTY.GE.1)THEN
               ISEL1=IMODE-1000
               CALL GDXZOO(ISEL1,ZFU,ZFV,UZ0,VZ0,U0,V0)
               RETURN
            ENDIF
         ENDIF

         IF(IMODE.EQ.0)THEN

            CALL GDCURS(UZ0,VZ0,JCHAR)
            IF (JCHAR.EQ.0) GO TO 999

         ELSE IF(IMODE.EQ.1)THEN

            CALL IGLOC2(1,NT,UZ1,VZ1,UZ2,VZ2,ISTAT,'R')
            IF (ISTAT.EQ.0) GO TO 999
            IF (UZ2-UZ1.EQ.0.) UZ2=UZ1+PLTRNX/200.
            IF (VZ2-VZ1.EQ.0.) VZ2=VZ1+PLTRNY/200.
            ZFU=PLTRNX/ABS(UZ2-UZ1)
            ZFV=PLTRNY/ABS(VZ2-VZ1)
            UZ0=(UZ1+UZ2)/2.
            VZ0=(VZ1+VZ2)/2.

         ELSE IF(IMODE.EQ.2)THEN

            CALL GDCURS(UZ0,VZ0,JCHAR)
            IF (JCHAR.EQ.0) GO TO 999
            CALL GDCURS(U0,V0,JCHAR)
            IF (JCHAR.EQ.0) GO TO 999

         ENDIF

  160    CALL GDZOOM(ZFU,ZFV,UZ0,VZ0,U0,V0)


      ELSEIF (CHPATL.EQ.'DXYZ') THEN
         CALL KUGETI(IT)
         CALL GDXYZ(IT)

      ELSEIF (CHPATL.EQ.'KXYZ') THEN
         CALL KUGETR(EPSXYZ)
         CALL GKXYZ(EPSXYZ)

      ELSEIF (CHPATL.EQ.'DPART') THEN
         CALL KUGETI(IT)
         ISELP = 11
         CALL KUGETI(ISELP)
         CALL KUGETR(SIZE)
         CALL GDPART(IT,ISELP,SIZE)

      ELSEIF (CHPATL.EQ.'DHITS') THEN
         CALL KUGETC(IVS,NCH)
         CALL KUGETC(ICS,NCH)
         CALL KUGETI(IUTR)
         ISYMB=0
         CALL KUGETI(ISYMB)
         CALL KUGETR(SSYMB)
         CALL GDHITS(IVS,ICS,IUTR,ISYMB,SSYMB)

      ELSEIF (CHPATL.EQ.'KHITS') THEN
         CALL KUGETC(IVS,NCH)
         CALL KUGETC(ICS,NCH)
         CALL KUGETR(EPSHIT)
         CALL GKHITS (IVS,ICS,EPSHIT)

      ELSEIF (CHPATL.EQ.'DCHIT') THEN
         IUTR =0
         ISYMB=0
         SIZMAX=1.
         KDHIT =4
         HITMIN=0.
         HITMAX=0.
         CALL KUGETC(IVS,NCH)
         CALL KUGETC(ICS,NCH)
         CALL KUGETI(IUTR)
         CALL KUGETC(MODE,NCH)
         CALL KUGETC(FILL,NCH)
         CALL KUGETI(ISYMB)
         CALL KUGETR(SIZMAX)
         CALL KUGETI(KDHIT)
         CALL KUGETR(HITMIN)
         CALL KUGETR(HITMAX)
         CALL GDCHIT(IVS,ICS,IUTR,ISYMB,SIZMAX,KDHIT, HITMIN,HITMAX)

      ELSEIF (CHPATL.EQ.'DUVIEW') THEN
         CALL KUGETC(IDS,NCH)
         CALL KUGETC(IVS,NCH)
         CALL KUGETC(ICS,NCH)
         CALL KUGETI(IVIEW)
         CALL GUVIEW(IDS,IVS,ICS,IVIEW)
      ENDIF

  999 END
