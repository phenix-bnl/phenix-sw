      subroutine helium_bag(type)
c-------------------------------------------------------------------------------------
c     Original author: Charles F. Maguire
c     Creation date: July 29, 1997
c
c     Purpose: Put in helium bag volume in Central Arm
c              tracking region betwee MVD and DC/PC1
c
c     Qualifications: Oversimplified initial version has
c                     no windows, and accommodates any
c                     nosecone configuration (none,
c                     asymmetric, or symmetric)
c
c     Method: Two bag volumes HEB1 and HEB2
c             Invoked by having 'HBAG' on GEOP line in pisa.kumac file
c             HEB1 from R = 32 to 53.3 cm
c                  from Z = -30 to +30 cm
c                  encompassed by nosecones
c                  simple TUBE
c
c             HEB2 from R = 53.6 to 200.5 cm
c                  from min Z of +/- 50 cm
c                  to max Z of +/- 103.5 cm
c                  PCONE volume
c                  DC Shield volume is inside HEB2, or
c                  HALL if HEB2 does not exist
c             HEB2 tracks along the inner face of the central
c                  magnet steel (*see 4 apr 08 notes below)
c
c     Revision history:
c       C. F. Maguire   July 30, 1997     Extend limit of par array to 15
c       C. F. Maguire   July 31, 1997     Change outer radius: 198 to 200.5 cm
c                                         as per suggestion of K. Shigaki
c       C. F. Maguire   August 7, 1997    Adjustments of HEB2 parameters as
c                                         per suggestions of Kenta Shigaki for
c                                         compatibility with Shield volumes
c       Hubert van Hecke February 2007    Moved parameters into phnx.par, so they
c                                         can be fit around  the SVX readout wheels.
c       Hubert van Hecke 4 Apr 2008       Added input argument TYPE that allows
c                  switching between old and new bags. TYPE='HBAG' describes the old
c                  bags, TYPE='HBGN' are smaller bags that clear the Silicon detector.
c       HvH May 2010: take a notch out of HEB2 to avoid overlap with PBSH lead shield.
c
c--------.---------.---------.---------.---------.---------.---------.--
      implicit none
      character*4 type
c
c     local variables
c
      integer nmed
      integer npar
      integer ivolu
      integer i, heb2_dim

      parameter (heb2_dim = 8)
      real par(3+3*heb2_dim)
      real  heb1_halfz,  heb1_rin,    heb1_rout,
     &      heb2_z(heb2_dim), heb2_rin(heb2_dim), heb2_rout(heb2_dim)
      namelist /helium_bag_par/   heb1_halfz, heb1_rin, heb1_rout,
     &                            heb2_z,     heb2_rin, heb2_rout
      namelist /helium_bagn_par/  heb1_halfz, heb1_rin, heb1_rout,
     &                            heb2_z,     heb2_rin, heb2_rout

c---------------------------------------------------------------------
c     geometry description logical unit
      integer itf_lun
      common /interface/itf_lun

c--------------------------------------------------------------------------
      write( *,* )'helium_bag ',type,' - reading pars from phnx.par'

      if (type.eq.'HBAG'
     +  .or. type.eq.'HBG1'
     +  .or. type.eq.'HBG2') then

        rewind(itf_lun)
        read( itf_lun, nml = helium_bag_par, err = 998 )

      elseif (type.eq.'HBGN') then

        rewind(itf_lun)
        read( itf_lun, nml = helium_bagN_par, err = 997 )

      else

        go to 996

      endif

      nmed = 1  ! magnetic Helium

c     Note: Max field for Helium set at 5 Tesla is good for single coil.
c           Dual coil goes up to to 10 Tesla, but dual coil will mean
c           a Dalitz rejector volume conflicting with simple Helium
c           volumes here.

      ! inner bag HEB1
      if( type.eq.'HBAG'
     +  .or.type.eq.'HBG1'
     +  .or.type.eq.'HBGN' ) then

        npar = 3
        par(1) = heb1_rin    ! inner radius 32.0
        par(2) = heb1_rout   ! outer radius 53.3
        par(3) = heb1_halfz  ! half-length in Z 26.0 or 30.0
        call gsvolu('HEB1','TUBE', nmed, par, npar, ivolu)
        if(ivolu.lt.1)then

          write(6,1)ivolu
 1        format('helium_bag - error in creating HEB1',
     +      ' return code = ',i5,/)
          stop ' PISA stopping'

        else

          call gsatt('HEB1', 'SEEN', 1)
          call gsatt('HEB1', 'COLO', 5)
C         call gspos('HEB1',1,'HALL',0.0, 0.0, 0.0,1,'ONLY')
C...      Change b/c RLT volume seems to be inside heliumbag
          call gspos('HEB1',1,'HALL',0.0, 0.0, 0.0,1,'MANY')
          write(6,2)ivolu
 2        format(
     +      ' helium_bag - HEB1 created',
     +      ' with volume number ',i4)

          endif

        endif

      ! inner bag HBG2
      if( type.eq.'HBAG'
     +  .or.type.eq.'HBG2'
     +  .or.type.eq.'HBGN' ) then

        npar = 3 + 3*heb2_dim ! number of parameters for polycone
        par(1) = 0.0          ! lower azimuth
        par(2) = 360.0        ! upper azimuth
        par(3) = heb2_dim     ! number of Z planes

        do i = 1,heb2_dim
          par(3*i+1) = heb2_z(i)
          par(3*i+2) = heb2_rin(i)
          par(3*i+3) = heb2_rout(i)
        enddo

        call gsvolu('HEB2','PCON', nmed, par, npar, ivolu)

        if(ivolu.lt.1)then

          write(6,3)ivolu
 3        format('helium_bag - error in creating HEB2',
     +      ' return code = ',i5,/)
          stop ' PISA stopping'

        else

          call gsatt('HEB2', 'SEEN', 1)
          call gsatt('HEB2', 'COLO', 5)
C         call gspos('HEB2',1,'HALL',0.0, 0.0, 0.0,1,'ONLY')
C...      Change b/c RLT volume seems to be inside heliumbag
          call gspos('HEB2',1,'HALL',0.0, 0.0, 0.0,1,'MANY')
          write(6,4)ivolu
 4        format(' helium_bag - HEB2 created',
     +      ' with volume number ',i4)
        endif

      endif

      return

 998  stop 'helium_bag - Read error in helium_bag_par segment of geometry.'
 997  stop 'helium_bag - Read error in helium_bagn_par segment of geometry.'
 996  stop 'helium_bag - unrecognized input argument.'
      end
