      subroutine polrpl

      implicit none

#include "g77trigdef.inc"

C     AUTHOR: C.F. Maguire
C     DATE:    November 24, 2004
C     GENERATES PARTICLES HAVING MOMENTUM
c     CORRELATIONS WITH REACTION PLAN
C     USED FOR V2 ANISTROPY AND POLARIZATION

 
#include "gugeom.inc"
#include "gcflag.inc"
#include "guphnx.inc"
#include "guevgen.inc"
#include "subevt.inc"

      real DEGRAD
 
      integer  i, j, id, itr, nwb, nubuf, nvertex, ntdummy
      real     thmin, thmax, pmin, pmax, theta, phi, delta
      real     dz, y, ptotal, pt, pxreac, pyreac, phireac
      real     phiTest, deltaTest, denomTest
      real     mass, charge, tlife, ub( 10 ), deltaOrig, dchk
      real     rndm, gamma, beta, pl, etot, p1( 3 ), mt, tran
      real     tran2, gProd, fProdSum, thminr, thmaxr, ratio
      integer  kCount, kLoop

      real prob(10) /0.05, 0.15, 0.25, 0.35, 0.45, 0.55,
     +               0.65, 0.75, 0.85, 0.95/ 

      logical fixProb /.false./
      logical zUniform /.false./
      logical noV2  /.true./
      logical noPolarization /.true./
      
      character*20          cpart
      
      integer icall /0/
      integer npos /0/
      integer nneg /0/

      integer np97
      parameter (np97 = 6)
      character*8 ch97(np97) /'RAPIDITY', 'PTRAN', 'PHI', 'MTRAN',
     +     'MASS', 'EVENT'/
      real evt97(np97)

      save zUniform, noV2, noPolarization  ! redundant if already initialized in specification
      save id, thminr, thmaxr, pmin, pmax, dz, icall

      DEGRAD = 3.14159/180.0

      chevt_name = 'POLRCP: reaction plane correlations'
      numevt = numevt + 1
      if ( icall .eq. 0 ) then  ! initialize program
         call hbookn(97, 'POLRCP Primaries', np97, 'GEANHIST',
     +        25000, ch97)
         icall = 1
 
c     Input parameters

         mxtot   = nint( pmc2( 1 ) ) ! no of particles per event
         id      = nint( pmc2( 2 ) ) ! particle id
         if(id.lt.0)then
            id = -id
            LorentzDisable = 1
         endif
         thmin =  pmc2( 3 ) ! minimum angle
         thmax =  pmc2( 4 ) ! maximum angle
         thminr = thmin*3.14159/180.0
         thmaxr = thmax*3.14159/180.0
         pmin  =  pmc2( 5 ) ! minimum total momentum
         pmax  =  pmc2( 6 ) ! maximum total momentum
         dz    =  pmc2( 7 ) ! vertex z-sigma
         if(pmc2(8).gt.0.5)then
            zUniform = .true.
         endif
 
         if(dz.eq.0.0)then
            multi_xyz = 0
         else
            multi_xyz = 1
         endif

         v2const = pmc2(9)
         pzconst = pmc2(10)

         if(v2const.ne.0.0)then
            noV2 = .false.
         endif
         if(pzconst.ne.0.0)then
            noPolarization = .false.
         endif

         mxtot = min( mxtot, max_mxtot )
         call  gfpart( id, cpart, itr, mass, charge,
     1        tlife, ub, nwb )

         write(6,11)mxtot, id, thmin, thmax, pmin,
     +        pmax, abs(dz)
 11      format(//,'  POLRCP Event generator for ',
     +        'reaction plane correlations',/,
     +        '   particles per event ', i6,/,
     +        '   GEANT ID type ' ,i6,/,
     +        '   THMIN   ',e14.5, ',  THMAX   ',e14.5,/,
     +        '   PMIN  ',e14.5, ',  PMAX  ',e14.5,/,
     +        '   Z-SIGMA ',e14.5)
         if(logacc)then
            write(6,110)
 110        format('  Forced acceptance for particles ',
     +           'in DC/PC1/PC3, or in Muon Arm')
         endif
         if(multi_xyz.eq.0)then
            write(6,12)
 12         format('  All particles at (x,y,z) = (0,0,0)')
         else
            write(6,13)
 13         format('  Particles have randomized Z vertex')
         endif
         if(zUniform)then
            write(6,14)
 14         format('  Z Randomization is Flat')
         else
            write(6,15)
 15         format('  Z Randomization is Gaussian')
         endif
         if(LorentzDisable.eq.1)then
            write(6,151)
 151        format('  Mass broadening is disabled')
         else
            write(6,152)
 152        format('  Mass broadening is enabled')
         endif
         write(6,16)
 16      format(//)
      end if                    ! check on first call


c     generate vertex

      xyz(1) = 0.0
      xyz(2) = 0.0
      xyz(3) = 0.0
      do i = 1, mxtot 
 
         if(dz.gt.0.0)then
            if(zUniform)then
               call grndm(tran,1)
               xyz( 3 ) = 2.0*dz*(tran-0.5)
            else
 5             continue
               call granor(tran,tran2)
               if(abs(tran).gt.2.0)go to 5
               xyz( 3 )  = dz * tran
            endif               ! check on zUniform
            xyz( 1 )        = 0.0
            xyz( 2 )        = 0.0
 
C***  store vertex info
            
            nubuf = 0
            xyzmv(1,i) = xyz(1)
            xyzmv(2,i) = xyz(2)
            xyzmv(3,i) = xyz(3)
         endif                  !  check on non-zero dz value
 
C***  kinematic variables

         call grndm(tran,1)
         ptotal = (pmax - pmin)*tran + pmin


c     Generate azimuthal angle in reaction plane frame 0 to 2*pi radians

c     Distribution function is  a*(1 + v2const*cos(2*phi))
c     Probability normalization produces a = 1/(2*pi)

c     For an integrated probability value g the corresponding phi angle is the solution to
c        phi + (v2const/2)*sin(2*phi) = g/a

c     Iterative solution
c        assume phiTest = (g/a) = gProd
c        correct to phi = gProd - (v2const/2)*sin(2*phiTest)
c        check for convergence

         kCount = 0             ! count how many attempts to get theta within limits
 55      continue
         call grndm(tran,1)
         if(fixProb)then
            tran = prob(i)
         endif
         gProd = 2.0*3.14159*tran
         phiTest = gprod        ! radians
         if(noV2)then
            phi = phiTest
         else
            do kLoop=1,1000     ! 1000 convergence iterations
               phi = gProd - 0.5*v2const*sin(2.0*phiTest)
               if(abs(phi - phiTest).le.1.0e-05*phiTest)then
                  go to 50
               endif
               phiTest = phi
            enddo
            write(6,49)phiTest, phi 
 49         format(1h  , ' phiTest ', f12.7, ', phi ', f12.7,/)
            stop ' Phi did not converge'
 50         continue
         endif                  ! check if iterative search is needed because v2const is not zero


c     Generate theta angle after first calculating the delta angle in (x,z) plane

c         kCount = 0             ! count how many attempts to get theta within limits
c 55      continue

c     Generate (x,z) plane delta angle 0 to pi radians

c     Distribution function is  a*(1 + pzconst*sin(2*delta)), correlates px and pz momentum
c     Probability normalization produces a = 1/pi

c     For an integrated probability value f the corresponding delta angle is the solution to
c        delta - (pzconst/2)*cos(2*delta) = f/a - (pzconst/2)

c     Iterative solution
c        assume deltaTest = (f/a) - (pzconst/2) = fProdSum
c        correct to delta = fProdSum + (pzconst/2)*cos(2*deltaTest)
c        check for convergence

         call grndm(tran, 1)
         if(fixProb)then
            tran = prob(i)
         endif
         fProdSum = 3.14159*tran - 0.5*pzconst
         deltaTest = fProdSum
         if(noPolarization)then
            delta = deltaTest
         else
            do kLoop = 1,1000
               delta = fProdSum + 0.5*pzconst*cos(2.0*deltaTest)
               if(abs(delta - deltaTest).le.1.0e-05*deltaTest)then
                  go to 60
               endif
               deltaTest = delta
            enddo
            write(6,59)deltaTest, delta
 59         format(1h  , ' deltaTest ', f12.7, ', delta ', f12.7,/)
            stop ' delta did not converge'
 60         continue
         endif                  ! check for no input polarization

c        calculate polar angle theta from delta and phi

         deltaOrig = delta
         if(phi.gt.0.5*3.14159.and.phi.lt.1.5*3.14159)then
            delta = 3.14159 - delta  ! polarization correlation on negative X side
         endif ! check for negative x side
         denomTest = 1.0 - cos(delta)*cos(delta)*sin(phi)*sin(phi)
         if(denomTest.ge.0.0)then
            ratio = cos(phi)*cos(delta)/sqrt(denomTest)
            if(abs(ratio).le.1.0)then
               theta = acos(ratio)
               if(phi.gt.0.5*3.14159.and.phi.lt.1.5*3.14159)then
                  theta = 3.14159 - theta ! negative x side polarization correlation
               endif            ! check for negative x side
            endif               ! check ratio
         endif                  ! check the denominator

c        check if this theta angle is within input limits

         kCount = kCount + 1
         if(kCount.gt.1000)then
            write(6,61)phi/DEGRAD, delta/DEGRAD, theta/DEGRAD
 61         format(1h , ' last phi ', f12.7, ', last delta ', f12.7,
     +                  ', last theta ', f12.7, ' STOPPING')
            stop ' Unable to get theta in limits'
         endif
         if(theta.lt.thminr .or. theta.gt.thmaxr)then
            go to 55
         endif ! check on theta within limits

         p1(3) = ptotal*cos(theta)

         pt = ptotal*sin(theta)
         pxreac = pt*cos(phi)
         pyreac = pt*sin(phi)

         if(pxreac/p1(3).gt.0.0)then
            npos = npos + 1
         endif
         if(pxreac/p1(3).lt.0.0)then
            nneg = nneg + 1
         endif


c     Transform azimuthal components to lab frame

         p1(1) = pxreac*cosd(reactionPlaneAngle) -
     +           pyreac*sind(reactionPlaneAngle)
         p1(2) = pxreac*sind(reactionPlaneAngle) +
     +           pyreac*cosd(reactionPlaneAngle)

         pptot(2,i) = p1(1)
         pptot(3,i) = p1(2)
         pptot(4,i) = p1(3)
         idtot(i) = id                
 
C***  now store track info
 
         ub( 1 ) = y
         ub( 2 ) = pt
         ub( 3 ) = phi
 
         evt97(1) = y
         evt97(2) = pt
         evt97(3) = phi
         evt97(4) = mt
         evt97(5) = mass
         evt97(6) = numevt
         call hfn(97, evt97)
 

         if(i.le.10)then
            dchk = cos(phi)*cos(phi)*sin(theta)*sin(theta) + 
     +           cos(theta)*cos(theta)
            dchk = (acos(cos(theta)/sqrt(dchk)))/DEGRAD
            write(6,72)i, pxreac, pyreac, p1(3),
     +      phi/DEGRAD, delta/DEGRAD, theta/DEGRAD,
     +      ptotal, deltaOrig/DEGRAD, dchk
 72         format(1h  ,i4,2x,'px ', f12.5, '  py ', f12.5,
     +                        ',  pz ', f12.5,/,
     +             '  phi ', f12.5, ',  delta ', f12.5,/
     +             '  theta ', f12.5, ',  ptot ', f12.5,
     +             '  deltaOrig ', f12.5, ", dchk ", f12.5)
         endif

      enddo ! loop over mxtot

c      write(6,71)npos, nneg, float(npos-nneg)/float(npos+nneg)
 71   format(1h , ' NPOS ', i10, ',  NEG ', i10,
     +            ', Polarization ', f8.4)
 
      maxtrk = ntdummy
 
      ipopsub = mxtot

      if(iswit(7).eq.8) then

c     Initialize the acceptance logical variable as false
c     For the Muon Arm this variable will be set to true under restricted conditions
c     Only when the acceptance logical variable is true will PISA write out an event

         logaccepted = .FALSE.
      endif

      return
 
      end
