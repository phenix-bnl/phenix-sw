      subroutine mcrap

      implicit none

#include "g77trigdef.inc"

C                AUTHORS: SPS
C                DATE:    5/29 1992
C                GENERATES PARTICLES WITH UNIFORM DISTRIBUTIONS IN
C                RAPIDITY, PHI AND TRANSVERSE MOMENTUM

c     CFM:  12/8/96  Revise to be compatible with standard guevgen usage, but
c                    all input parameters remain the same

c     CFM:  6/2/2002 Revise to use GRNDM, and to have uniform dz option

c     CFM:  7/28/2002  Add Lorentz disable option (negative particle ID)

c     CFM:  9/24/2002  Add temperature for transverse mass distribution

c     CFM:  2/8/2003   Allow for uniform mt dependence, alternate to uniform pt dependence

c     CFM:  4/29/2003  Change the temperature distribution to pt*exp(-mt/T) to correspond to EXODUS

 
#include "gugeom.inc"
#include "gcflag.inc"
#include "guphnx.inc"
#include "guevgen.inc"
#include "subevt.inc"
 
        integer  i, j, id, itr, nwb, nubuf, nvertex, ntdummy
        real     y_min, y_max, pt_min, pt_max, phi_min, phi_max
        real     dz, y, pt, phi, mass, charge, tlife, ub( 10 )
        real     rndm, gamma, beta, pl, etot, p1( 3 ), mt, tran
        real     tran2
        real     temperature, mtStep
        integer  iRndm

        real mtTempFunc, hrndm

        logical zUniform /.false./
 
        character*20 cpart
 
        integer MAX97
        parameter (MAX97 = 10000)
        integer iFill97 /0/
        integer icall /0/
        integer np97
        parameter (np97 = 6)
        character*8 ch97(np97) /'RAPIDITY', 'PTRAN', 'PHI', 'MTRAN',
     +                          'MASS', 'EVENT'/
        real evt97(np97)

        integer transverseMass /0/

      save zUniform     ! should be redundant if already initialized in specification
      save id, y_min, y_max, pt_min, pt_max, phi_min, phi_max, dz, icall
      save temperature, transverseMass

        chevt_name = 'MCRAP: uniform Y and PT distributions'
        numevt = numevt + 1
        if ( icall .eq. 0 ) then         ! initialize program
                call hbookn(97, 'MC Primaries', np97, 'GEANHIST',
     +          25000, ch97)
                icall = 1
 
c        Input parameters

                mxtot   = nint( pmc2( 1 ) ) ! no of particles per event
                id      = nint( pmc2( 2 ) )  ! particle id
                if(id.lt.0)then
                  id = -id
                  LorentzDisable = 1
                endif
                y_min   =       pmc2( 3 )    ! minimum rapidity
                y_max   =       pmc2( 4 )    ! maximum rapidity
                pt_min  =       pmc2( 5 )    ! minimum pt
                pt_max  =       pmc2( 6 )    ! maximum pt
                phi_min =       pmc2( 7 )    ! minimum phi
                phi_max =       pmc2( 8 )    ! maximum phi
                dz      =       pmc2( 9 )   ! vertex z-sigma

                if(pmc2(10).gt.0.5)then
                  zUniform = .true.
                endif
 
                if(dz.eq.0.0)then
                   multi_xyz = 0
                else
                   multi_xyz = 1
                endif

                temperature = pkine2(1)
                if(temperature.gt.0.0) then
                   transverseMass = 1
                else
                   transverseMass = 0
                endif

                if(pt_min.lt.0.0)then
                   pt_min = -pt_min
                   transverseMass = -1
                endif  ! will generate uniform mt first, then pt from mt
 
                mxtot = min( mxtot, max_mxtot )
                call  gfpart( id, cpart, itr, mass, charge,
     1                tlife, ub, nwb )

                write(6,11)mxtot, id, cpart, y_min, y_max, pt_min,
     +                     pt_max, phi_min, phi_max, abs(dz)
11              format(//,'  MCRAP Event generator for ',
     +                    'uniform Y and Pt distribution',/,
     +                 '   particles per event ', i6,/,
     +                 '   GEANT ID type ' ,i6,/,2x,a20,/,
     +                 '   Y_MIN   ',e14.5, ',  Y_MAX   ',e14.5,/,
     +                 '   PT_MIN  ',e14.5, ',  PT_MAX  ',e14.5,/,
     +                 '   PHI_MIN ',e14.5, ',  PHI_MAX ',e14.5,/,
     +                 '   Z-SIGMA ',e14.5)
                if(logacc)then
                   write(6,110)
 110               format('  Forced acceptance for particles ',
     +                    'in DC/PC1/PC3, or in Muon Arm')
                endif
                if(multi_xyz.eq.0)then
                   write(6,12)
12              format('  All particles at (x,y,z) = (0,0,0)')
                else
                   write(6,13)
13              format('  Particles have randomized Z vertex')
                endif
                if(zUniform)then
                   write(6,14)
14              format('  Randomization is Flat')
                else
                   write(6,15)
15              format('  Randomization is Gaussian')
                endif
                if(LorentzDisable.eq.1)then
                  write(6,151)
151               format('  Mass broadening is disabled')
                 else
                  write(6,152)
152               format('  Mass broadening is enabled')
                endif
                if(transverseMass.eq.1)then
                   write(6,153)temperature, pt_min, pt_max
153                format(' Using a transverse mass temperature',
     +                    f6.1,' MeV, over a limited Mt range',/,
     +                    '  mtMin ', f8.4,  ',  mtMax ', f8.4,/)

c                  Generate a dN/dmT spectrum in Mt

                   call hbook1(79,'HRNDM1 for Mt', 10000,
     +                         pt_min, pt_max, 0.0)   ! these are really Mt limits
                   mt = pt_min
                   mtStep = (pt_max - pt_min)/10000.
                   do iRndm = 1, 10000
                      call hf1(79, mt, mtTempFunc(mt))
                      mt = mt + mtStep
                   enddo
                endif
                if(transverseMass.eq.-1)then
                   write(6,154)
154                format(' Generating uniform mt width bins ')
                endif
                write(6,16)
16              format(//)
        end if  ! check on first call
 
        xyz(1) = 0.0
        xyz(2) = 0.0
        xyz(3) = 0.0
        do i = 1, mxtot
 
C***                particle id
 
 
C***        vertex
                if(dz.gt.0.0)then
                   if(zUniform)then
                      call grndm(tran,1)
                      xyz( 3 ) = 2.0*dz*(tran-0.5)
                   else

c       Change from NORRAN routine to GRANOR routine
c       GRANOR is coupled into the uniform GEANT random number sequence
c       GRANOR returns two random numbers, but use only the first here

 5                    continue
                      call granor(tran,tran2)
                      if(abs(tran).gt.2.0)go to 5
                      xyz( 3 )  = dz * tran
                   endif  ! check on zUniform
                   xyz( 1 )        = 0.0
                   xyz( 2 )        = 0.0
 
C***        store vertex info
 
                   nubuf = 0
                   xyzmv(1,i) = xyz(1)
                   xyzmv(2,i) = xyz(2)
                   xyzmv(3,i) = xyz(3)
                endif  !  check on non-zero dz value
c               call gsvert( xyz, 0, 0, ub, nubuf, nvertex )
 
C***        kinematic variables

                call grndm(tran,1)
                y   = (   y_max -   y_min ) * tran + y_min
                call grndm(tran,1)

                if(transverseMass.eq.0)then
                   pt  = (  pt_max -  pt_min ) * tran +  pt_min
                   mt  = sqrt( pt**2 + mass**2 )
                endif ! generating mt from pt where pt has a uniform width

                if(transverseMass.eq.1)then
                   mt = hrndm(79)   ! first call will integrate the histogram
                   if(mt.lt.mass.or.mt.lt.0.99*pt_min.or.
     +                mt.gt.1.01*pt_max)then
                      write(6,157)mt
 157                  format(' bad mt value ', f8.4)
                      stop ' PISA stopping'
                   endif
                   pt = sqrt(mt*mt - mass*mass)
                   if(icall.lt.0)then
                      write(6,1577)mt, pt
 1577                 format(1h , ' Temp. dep. mt ', f8.4,
     +                       ',  pt ',f8.4)
                   endif ! debug print out
                endif ! generating pt from mt Temperature

               if(transverseMass.eq.-1)then
                   mt  = (  pt_max -  pt_min ) * tran +  pt_min
                   if(mt.lt.mass)then
                      write(6,159)mt, mass
 159                  format(' mt less than  mass ', 2f8.5)
                      stop ' PISA stopping'
                   endif
                   pt  = sqrt( mt**2 - mass**2 )
                   if(icall.lt.0)then
                      write(6,1599)mt, pt
 1599                 format(1h , ' Uniform dep. mt ', f8.4,
     +                       ',  pt ',f8.4)
                   endif  ! debug printout 
                endif ! generating pt from mt where mt has a uniform width

                call grndm(tran,1)
                phi = ( phi_max - phi_min ) * tran + phi_min
  
                p1(1) = cosd( phi ) * pt
                p1(2) = sind( phi ) * pt
                p1(3) = mt * sinh( y )

                pptot(1,i) = sqrt(pt*pt + p1(3)*p1(3) +
     +                            mass*mass)
                pptot(2,i) = p1(1)
                pptot(3,i) = p1(2)
                pptot(4,i) = p1(3)
                idtot(i) = id                
 
C***        now store track info
 
                ub( 1 ) = y
                ub( 2 ) = pt
                ub( 3 ) = phi
 
c               call gskine (p1, id, nvertex, ub, 3, ntdummy )

                evt97(1) = y
                evt97(2) = pt
                evt97(3) = phi
                evt97(4) = mt
                evt97(5) = mass
                evt97(6) = numevt
                if(iFill97.le.MAX97)then
                   call hfn(97, evt97)
                   iFill97 = iFill97 + 1
                endif
 
        end do
 
        maxtrk = ntdummy
 
c       if ( idebug .ne. 0 ) call gpkine( 0 )

        ipopsub = mxtot

        if(iswit(7).eq.8) then

c          Initialize the acceptance logical variable as false
c          For the Muon Arm this variable will be set to true under restricted conditions
c          Only when the acceptance logical variable is true will PISA write out an event

           logaccepted = .FALSE.
        endif

        return
 
        end

        real function mtTempFunc(mTransverse)
        implicit none


c       Distribution in transverse mass dN/dmT

c       dN/dmT = mT*Exp(-mT/T) where mT is the transverse mass and T the temperature

c       This is equivalent to dN/dpT = pT*Exp(-mT/T) for a pT distribution


#include "guevgen.inc"

        real mTransverse  ! transverse mass value
        real result

        real temperature

        temperature = 0.001*pkine2(1)

        result = mTransverse*exp(-mTransverse/Temperature)

        mtTempFunc = result
        return

        end
