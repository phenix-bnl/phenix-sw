      subroutine trkxyz(it,idet)

      implicit none

#include "g77trigdef.inc"


c     Special return for MapFest meetings (1996)
c     Original author: Charles F. Maguire
c     Creation date:   October 19, 1996

c     Purpose: Routine is called to sample the volume points occupied
c              by particle tracks originating from the primary vertex and
c              passing through a sensitive detector.
c              Tracks are sampled every SEP_DIST value (in cm)


#include "secubuf.inc"
#include "subevt.inc"
#include "gcbank.inc"
#include "guphnx.inc"


c     calling variable

      integer it   ! Track number in subevent
      integer idet ! number of detector set which called this routine


c     local variables

      real sep_dist
      parameter (sep_dist = 2.0)
      real rmaxv
      parameter (rmaxv = 3.0)
      real rmaxd
      parameter (rmaxd = 400.0)

      integer i
      integer ifirst /0/
      integer iipl
      integer ipart
      integer ipf
      integer ipff
      integer ipoint
      integer j
      integer jx
      integer nxyz
      integer n1
      integer n2
      integer np
      integer npoint
      real sep_chk
      real rdist
      real rdistold
      real dist
      real xyz(3)
      real xyzold(3)

      integer spart, snvert          ! for gfkine call
      real pvrtx(4), svrtx(3)        ! for gfkine call
      real ptot
      real ptheta
      real pphi

      save ifirst

c     NTUPLE 95

      integer np95
      parameter (np95=14)
      character*8 ch95(np95) /'ITRACK', 'IDPART', 'P_VERTEX',
     +                        'XPOS', 'YPOS', 'ZPOS',
     +                        'IDET', 'Z_VERTEX', 'P_THETA', 'P_PHI',
     +                        'X_VERTEX', 'Y_VERTEX',
     +                        'SUBEVENT', 'EVENT'/
      real evt95(np95)


c     begin execution


c     NTUPLE booking

      if(ifirst.eq.0)then
	  ifirst = 1
          call hbookn(95,'Field Map Sampling',np95,'GEANHIST',
     1                6000,ch95)
          write(6,*)'  NTUPLE 95 is booked'
      endif  ! initialization
      if(it.le.0)then
         return   ! bad track number
      endif
      call gfkine(it,svrtx,pvrtx,spart,snvert,ubuf,nubuf)
      if(cvolu_opt(3,5).ne.'MAPF')then
         rdist = svrtx(1)*svrtx(1) + svrtx(2)*svrtx(2)
         if(rdist.gt.0.0)then
            rdist = sqrt(rdist)
         endif
         if(rdist.gt.rmaxv)then
            return
         endif
      endif
      ptot = sqrt(pvrtx(1)*pvrtx(1) + pvrtx(2)*pvrtx(2) +
     +            pvrtx(3)*pvrtx(3))
      if(ptot.gt.0.0)then
         ptheta = acosd(pvrtx(3)/ptot)
         pphi = atan2d(pvrtx(2),pvrtx(1))

c   phi convention is -22.5 to 67.5 for first arm
c                     112.5 to 202.5 for second arm

         if(pphi.lt.-90.0)pphi = 360.0 + pphi
      else
         ptheta = -999.
         pphi = -999.
      endif
      evt95(np95-1) = nsub_evt ! SUBEVENT NUMBER
      evt95(np95) = ntru_evt ! EVENT NUMBER
      evt95(1) = it
      evt95(3) = ptot
      evt95(7) = idet
      evt95(8) = svrtx(3)
      evt95(9) = ptheta
      evt95(10) = pphi
      evt95(11) = svrtx(1)
      evt95(12) = svrtx(2)
      IF(JXYZ.LE.0)GO TO 70
      NXYZ=IQ(JXYZ-2)
      N1  =1
      N2  =NXYZ
      IF(IT.NE.0)N1=IT
      IF(IT.NE.0)N2=IT
      IF(N1.LE.0)GO TO 70
      IF(N2.GT.NXYZ)GO TO 70

      DO 60 I=N1,N2
         JX=LQ(JXYZ-I)
         IF(JX.LE.0)GO TO 50
         IPOINT=JX+3
   10    IF(IPOINT.GT.JX+IQ(JX-1))GO TO 50
         NPOINT=Q(IPOINT)
         IF(NPOINT.LE.0)GO TO 50
         IPART=Q(IPOINT+1)
         if(spart.ne.ipart)then
            write(6,11)spart,ipart,n1,n2,i
 11         format('  SPART,IPART,N1,N2,I' ,5i10)
         endif
         if(ipart.lt.1.or.ipart.gt.45.or.spart.ne.ipart)go to 50
         evt95(2) = ipart
         ipf = 1
   20    IIPL=IPF+49
         IF(IIPL.GT.NPOINT)IIPL=NPOINT
         NP=IIPL-IPF+1
         IF(NP.LE.0)GO TO 40

c         CALL GDFR3D(Q(IPOINT+3*IPF-1),NP,U,V)
c         q(ipoint+3*ipf-1) points to xyz(3) array

         ipff = ipf
         do j = 1,np
            xyz(1) = q(ipoint+3*ipff-1)
            if(ipff.ne.1)then

c     Problems with JXYZ data structure

               if(abs(xyz(1)-xyzold(1)).gt.50.)go to 35
            endif
            xyz(2) = q(ipoint+3*ipff)
            if(ipff.ne.1)then

c     Problems with JXYZ data structure

               if(abs(xyz(2)-xyzold(2)).gt.50.)go to 35
            endif
c            write(6,222)npoint,ipoint,j,np,ipff,xyz(1),xyz(2),
c     +                  sqrt(xyz(1)**2+xyz(2)**2)
 222        format(' NP,IP,J,NP,IPFF',5i8,' X,Y,R ',3(f8.1,1x))
            rdist = xyz(1)*xyz(1) + xyz(2)*xyz(2)
            if(rdist.ge.0.0)then
               rdist = sqrt(rdist)
            else
               go to 35
            endif
            if(rdist.gt.rmaxd)then
               go to 35
            endif
            xyz(3) = q(ipoint+3*ipff+1)
            if(ipff.ne.1)then

c     Problems with JXYZ data structure

               if(abs(xyz(3)-xyzold(3)).gt.50.)go to 35
            endif
            if(xyz(3).lt.-300.or.xyz(3).gt.300.)go to 35
            if(xyz(2).lt.-rmaxd.or.xyz(2).gt.rmaxd)go to 35
            if(xyz(1).lt.-rmaxd.or.xyz(1).gt.rmaxd)go to 35
            if(ipff.eq.1)then
               xyzold(1) = xyz(1)
               xyzold(2) = xyz(2)
               xyzold(3) = xyz(3)
               rdistold = rdist
            endif
            if(abs(rdist-rdistold).gt.50.0)then

c     Problem with JXYZ data structure
c     Sometimes random (x,y,z) or even non-numeric values?

               go to 35
            endif
            dist = (xyz(1)-xyzold(1))*(xyz(1)-xyzold(1)) +
     +             (xyz(2)-xyzold(2))*(xyz(2)-xyzold(2)) +
     +             (xyz(3)-xyzold(3))*(xyz(3)-xyzold(3))
            if(dist.gt.0.0)then
               dist = sqrt(dist)
            endif
            sep_chk = amax1(sep_dist,0.02*rdist)
            if(dist.ge.sep_chk)then
               evt95(4) = xyz(1)
               evt95(5) = xyz(2)
               evt95(6) = xyz(3)
               call hfn(95,evt95)
               xyzold(1) = xyz(1)
               xyzold(2) = xyz(2)
               xyzold(3) = xyz(3)
            endif
            ipff = ipff + 1
            rdistold = rdist
         enddo
 35      continue
         IPF=IIPL
         IF(IPF.LT.NPOINT)GO TO 20
   40    IPOINT=IPOINT+3.*Q(IPOINT)+2.
c         GO TO 10   ! particles on stack ??

   50    CONTINUE

   60 CONTINUE

   70 CONTINUE

      END
