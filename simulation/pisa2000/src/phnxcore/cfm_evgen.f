*CMZU:  2.04/00 07/03/94  17.35.14  by  Mohini W. Rawool-Sullivan
*CMZ :  2.02/00 01/06/93  15.24.20  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.38  by  Charles F. Maguire
      SUBROUTINE CFM_EVGEN
      IMPLICIT NONE

c     Old code for reading Pat McGaughey's binary output file for dimuons.
c     Will have to upgrade if kept.
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        "Private Event Generator Code from C.F. Maguire         *
C.    * Code originally in GUEVGEN.FOR but removed to separate routine *
C.    *                                                                *
C.    ******************************************************************
C.
C.    ------------------------------------------------------------------

*KEEP,GCKINE.
#include "gckine.inc"
*KEEP,GCONST.
#include "gconst.inc"
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,SUBEVT.
#include "subevt.inc"
*KEEP,GUEVGEN.
#include "guevgen.inc"
*KEEP,CMCEVT.
#include "cmcevt.inc"
*KEND.

C     NOTE: MXTOT in GUEVEGEN cannot be bigger than 20000 !

      REAL P1(3)/0.0,0.0,0.0/
      REAL UB(3)/0.0,0.0,0.0/
      INTEGER   NUBUF /0/
      INTEGER   I, IFILL
      INTEGER ISEED /2/
      INTEGER MXDIMU, NACCP, IPT, IPTOLD
      SAVE IPTOLD
      REAL THPMIN, THPMAX, ETPMIN, CSPMIN, CSPMAX, ETOT, PTOT, CSPAT
      REAL PHIRT1, PHIRT2, PHI_1, PHI_2, PHIROT, PRAD, RNDM
      SAVE THPMIN, THPMAX, ETPMIN, CSPMIN, CSPMAX, ETOT, PTOT, CSPAT
      SAVE PHIRT1, PHIRT2
      logical logopn /.false./
      logical logpat /.false./
      logical logdiel /.false./
      logical logrot /.false./
      logical logrph /.false./
      logical logpi3 /.false./
      logical logmxb /.false./
      save logopn, logpat, logdiel, logrph, logpi3, logmxb
 3000 continue
      entry LW_EVENT
      print *, ' '
      print *,'  CALL TO LWLEAK SUBROUTINE'
      print *, ' '
      return
 6000 continue
      entry JPSI_EVENT
      CHEVT_NAME = 'JPSI_EVENT from data file'
      if(ikine.eq.-6)then
         logdiel = .true.
      else
         logdiel = .false.
      endif

c     Read Pat JPSI data file

      if(.not.logpat)then
         logpat=.true.
         open(unit=17,file='dimuon.par',status='old',
     +         access='sequential',form='formatted',
     +         err=6002)
         read(17,*,err=6009,end=6010)mxdimu,thpmin,thpmax,
     +         etpmin,phirt1,phirt2
         print 6011,mxdimu,thpmin,thpmax,etpmin
 6011 format(//,2x,'dimuon parameters: MXDIMU ',i4,' THMIN,THMAX ',
     +      2f8.2,'  ETMIN ',f8.2,//)
         close(unit=17)
         cspmax = cos(thpmin/57.29578)
         cspmin = cos(thpmax/57.29578)
         if(phirt1.ne.0.0.and.phirt2.ne.0.0)then
            logrot = .true.
            phirt1 = phirt1/57.29578
            phirt2 = phirt2/57.29578
            phirt2 = phirt2 - phirt1
         else
            logrot = .false.
         endif
         ifill=0
         iptold = 1
c        unformatted --> formatted (JPS)
         open(unit=17,file='dimuon.dat',status='old',
     +         access='sequential',form='formatted',
     +         err=6003)
	 ipopsub = 2
         MC_TRACK_NO = 0           !track # used in mum_user
         MC_EVENT_NO = 0           !event # used in mum_user
      endif
      naccp = 0
 6012 continue
      if(iptold.eq.1)then
         ifill=ifill+1
         read(17,*,err=6005,end=6007)(pptot(2,i),pptot(3,i), pptot(4,i)
     +   , i=1,200)
         if(logrot)then
            do i = 1,200,2
               phi_1 = atan2(pptot(3,i),pptot(2,i))
               phi_2 = atan2(pptot(3,i+1),pptot(2,i+1))
               phirot = phirt1 + rndm(0)*phirt2
               phi_2 = phirot + (phi_2 - phi_1)
               prad = sqrt(pptot(3,i)**2 + pptot(2,i)**2)
               pptot(3,i) = prad*sin(phirot)
               pptot(2,i) = prad*cos(phirot)
               prad = sqrt(pptot(3,i+1)**2 + pptot(2,i+1)**2)
               pptot(3,i+1) = prad*sin(phi_2)
               pptot(2,i+1) = prad*cos(phi_2)
            enddo
         endif
      endif
      ipt = iptold
      do i=1,mxdimu
         etot = pptot(2,ipt)*pptot(2,ipt)
     +   + pptot(3,ipt)*pptot(3,ipt) +
     +   pptot(4,ipt)*pptot(4,ipt)
         if(etot.gt.0.0)then
            etot = sqrt(etot)
         endif
         ptot = etot
         if(ptot.gt.0.0)then
            cspat = pptot(4,ipt)/ptot
         endif
 
C         if(etot.gt.etpmin.and.cspat.ge.cspmin.and.cspat.le.cspmax
C     +   .and.okay.eq.1)then
 
            naccp = naccp+1
            pptot(2,naccp) = pptot(2,ipt)
            pptot(3,naccp) = pptot(3,ipt)
            pptot(4,naccp) = pptot(4,ipt)
            if(ipt-2*(ipt/2).eq.0)then
               if(logdiel)then          !come here for even ipt values
                  idtot(naccp) = 3      !e-
               else
                  idtot(naccp) = 6      !mu-
               endif
            else
               if(logdiel)then          !come here for odd ipt values
                  idtot(naccp) = 2      !e+
               else
                  idtot(naccp) = 5      !mu+
               endif
            endif
C         endif
         ipt = ipt + 1
         if(ipt.eq.201)then
            iptold=1
            ipt = 1
            if(naccp.lt.mxdimu)go to 6012
         endif
      enddo
      iptold = ipt
      if(naccp.lt.1)go to 6012
      if(ifill-100*mxdimu*(ifill/(100*mxdimu)).eq.0)then
         print 6013,ifill,naccp,(pptot(i,naccp),i=1,3)
 6013 format(2x,'fill #',i5,' naccp',i4,'  P3VEC',3e12.5)
      endif
      mxtot=naccp
      xyz(1)=0.
      xyz(2)=0.
      xyz(3)=(22.*RNDM (ISEED+2)-11.)
      end_evtflg = .true.        ! simple events
      MC_EVENT_NO= MC_EVENT_NO + 1
      MC_TRACK_NO= 0
      goto 9000
 6003 continue
      print 6004
 6004 format(/,3x,'Cannot open dimuon.dat file'/)
      return
 6005 continue
      print 6006
 6006 format(/,3x,'Error in reading dimuon.dat file'/)
      return
 6007 continue
      print 6008
 6008 format(/,3x,'EOF in reading dimuon.dat file'/)
      close(unit=17)
      return
 6002 continue
      print *, ' Cannot find dimuon.PAR file'
      return
 6009 continue
      print *, ' ERR in reading dimuon.PAR file'
      return
 6010 continue
      print *, ' EOF in reading dimuon.PAR file'
      return

c     return to the GO TO 9000 location in GUEVGEN

 9000 continue
      return
      END
