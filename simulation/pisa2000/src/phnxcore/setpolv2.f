      subroutine setpolv2
      implicit none

#include "g77trigdef.inc"
#include "guevgen.inc"
#include "gctrak.inc"

      integer kPart, kLoop
      integer iCall /1/

      real v2Particle
      real pxreac, pyreac
      real pxtemp, pytemp, pztemp, ptran, rapidity, etemp
      real signpx, tran, phi, phiTest, gProd, kCount

      logical noV2 /.true./
      logical noPol /.true./
      logical v2Recombin /.false./
      logical v2ShinIchi /.false./

      save iCall, noV2, noPol, v2Recombin, v2ShinIchi

      if(iCall.eq.1)then
         iCall = 0
         write(6,1)rho000, v2const
 1       format(//,'  SETPOLV2 <I>: RHO000 = ', f8.4,
     +        ',  V2CONST = ', f8.4,//)
         if(abs(v2const).gt.0.0)then
            noV2 = .false.
            if(abs(v2const + 1.0).lt.0.05)then
               v2Recombin = .true.  !  v2const = -1 means use recombination formula for v2
               write(6,2)
 2             format(' V2(pt) from recombination model',/)
            endif
            if(abs(v2const + 2.0).lt.0.05)then
               v2ShinIchi = .true. !  v2const = -2 means use ShinIchi's formula for v2
               write(6,3)
 3             format(' Using PT dependent V2 from ShinIchi',/)
            endif   !
         endif
      
         if(abs(pzconst).gt.0.0)then
            noPol = .false.
         endif

         if(idpolar.ne.0)then
            if(idpolar.gt.0)then
               write(6,4)idpolar
 4             format(/,' Polarized pair decay meson ID = ', i5,/)
            else
               write(6,5)
 5             format(/,' All pair decays are polarized',/)
            endif
            call openpairpolarfile(1)
         endif

      endif  ! check on first call

      do kPart = 1,mxtot
         etemp = pptot(1,kPart)
         pxtemp = pptot(2,kPart)
         pytemp = pptot(3,kPart)
         pztemp = pptot(4,kPart)

         ptran = sqrt(pxtemp*pxtemp + pytemp*pytemp)

         v2Particle = v2const
         if(v2Recombin)then
            v2Particle = 0.22/(1.0 + exp(-5.0*(ptran/2.0 - 0.35)))
     +                   - 0.06
         endif ! use v2 from recombination model

         if(v2ShinIchi)then

c         v2=v2g*exp(-0.5*(yr/v2w)**2)*(0.25-0.01*(pt-5.0)**2)
c         where v2g = 1.5, yr -> rapidity, v2w = 2.5

            if(etemp.lt.pztemp .or. etemp.lt.ptran)then
               stop ' SETPOLV2 <F>: Error in total particle energy'
            endif
            rapidity = 0.5*(log((etemp + pztemp)/(etemp - pztemp)))
            v2Particle = 1.5*(0.25 - 0.01*(ptran - 5.0)*(ptran - 5.0))*
     +                   exp(-0.5*(rapidity*rapidity/6.25))
         endif ! use v2 from ShinIchi's formula


c     Generate azimuthal angle in reaction plane frame 0 to 2*pi radians

c     Distribution function is  a*(1 + 2*v2Particle*cos(2*phi))
c     Probability normalization produces a = 1/(2*pi)

c     For an integrated probability value g the corresponding phi angle is the solution to
c        phi + v2Particle*sin(2*phi) = g/a

c     Iterative solution (could be replaced by Newton's rule method as in gdecay2Polarized.F)
c        assume phiTest = (g/a) = gProd
c        correct to phi = gProd - v2Particle*sin(2*phiTest)
c        check for convergence

         kCount = 0             ! count how many attempts to get theta within limits
 55      continue
         call grndm(tran,1)
         gProd = 2.0*3.14159*tran
         phiTest = gprod        ! radians
         if(noV2)then
            phi = phiTest
         else
            do kLoop=1,1000     ! 1000 convergence iterations
               phi = gProd - v2Particle*sin(2.0*phiTest)
               if(abs(phi - phiTest).le.1.0e-05*phiTest)then
                  go to 50
               endif
               phiTest = phi
            enddo
            write(6,49)phiTest, phi 
 49         format(1h  , ' phiTest ', f12.7, ', phi ', f12.7,/)
            stop ' Phi did not converge'
 50         continue
         endif                  ! check if iterative search is needed because v2Particle is not zero

         pxreac = ptran*cos(phi)
         pyreac = ptran*sin(phi)

c     Transform azimuthal components to lab frame

         pptot(2,kPart) = pxreac*cosd(reactionPlaneAngle) -
     +                    pyreac*sind(reactionPlaneAngle)
         pptot(3,kPart) = pxreac*sind(reactionPlaneAngle) +
     +                    pyreac*cosd(reactionPlaneAngle)


c     Correlate pz and pxreac directions

         call grndm(tran, 1)  ! correlation probability
         if(.not.noPol .and. tran.lt.pzconst)then
            signpx = 1.0
         
            if(abs(pxreac).ne.0.0)then
               signpx = pxreac/abs(pxreac)
               pptot(4,kPart) = signpx*abs(pztemp)              
            endif

         endif

      enddo

      return
      end
