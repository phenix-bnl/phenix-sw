      integer function mfm_initialize(nmaps, interp, fscale)
      implicit none
c
c     STAF utility interface to Magnetic Field Map reading software used in PISA
c
c     Author: Charles F. Maguire
c     Creation date: November, 15, 1997
c
c     Calling map:
c          Called by any STAF user but not called by PISA or PISORP users
c          Calls gufld to initialize magnetic field map arrays
c          Magnetic field map either in current working directory or in AFS area
c       
c     Calling variables
c
c     nmaps        integer number of map files to read
c     interp       integer interpolation factor as either 1 (linear) or 2 (quadratic)
c                  20 means September 2001 2D map, 30 means November 2001 3D map
c     fscale       floating point magnetic field scaling factor (usually 1.0)

c     Global variables
c
      integer nmaps
      integer interp
      real fscale

c     PISA include for initializing option variables used by GUFLD
#include "mfm.inc"

c     Local variables
      integer nmap_max        ! maximum number of maps
      parameter(nmap_max = 1) ! present (11/97) limit
      integer ifirst /0/      ! initial call test
      real xyz(3) /3*0.0/     ! initial field map point
      real field(3) /3*-1.0/  ! initial field map value (should be calculated by gufld)
      real brtest, bztest     ! radial and longitudinal field test values
      save ifirst

c     Begin execution
      mfm_initialize = -1    ! default return value
      ifirst = ifirst + 1
      if(ifirst.gt.1) then
        write(6,1)ifirst
 1      format('mfm_initialize - Repeated call number ', i4,
     +  ' ignored',//)
        return
      endif  ! check on first call

c     check input values
      if(nmaps.ne.1)then
        write(6,10)nmaps,nmap_max
10      format('mfm_initialize -: Requested number of maps = ',i2,
     +  ' exceeds current limit = ',i2)
        write(6,11)nmap_max
11      format('  Number of field maps set to limit = ',i2,/)
      endif
      ipvolu_opt(3,1) = min0(nmaps,nmap_max)

c     NOTE: BFIELD routine defaults to at least one map if ipvolu(3,1) = 0
      if(interp.eq.1)then
        cpvolu_opt(1,1) = 'LIN'    ! linear interpolation of magnetic field map
      elseif(interp.eq.2)then
        cpvolu_opt(1,1) = 'QUAD'   ! bi-quadratic interpolation (normal usage)
      elseif(interp.eq.20)then
        cpvolu_opt(1,1) = '2D01'   ! 2D simulation map from February 1997
      elseif(interp.eq.30)then
        cpvolu_opt(1,1) = '3D01'   ! Run1 and Run2 magnetic field map
      elseif(interp.eq.33)then
        cpvolu_opt(1,1) = '3D03'   ! Run3 magnetic field map
      elseif(interp.eq.34)then
        cpvolu_opt(1,1) = '3D++'   ! Run4 magnetic field map
      elseif(interp.eq.35)then
        cpvolu_opt(1,1) = '3D+-'   ! Run4 magnetic field map
      else
        write(6,2)interp
 2      format('mfm_initialize - Interpolation parameter value = ',i4,
     +  ' outside allowed range (1,2,20,30,33,34)',//)
        mfm_initialize = -2
        return
      endif  !  check on allowed values for interpolation parameter

c     set scale factor
      rpvolu_opt(2,1) = fscale
           
      call gufld(xyz,field)
      brtest = sqrt(field(1)*field(1) + field(2)*field(2))
      bztest = abs(field(3))

c     do checking that returned field values are not obviously wrong
      if(fscale.eq.0.0)then
        if(brtest.gt.0.001.or.bztest.gt.0.001)then
          write(6,3)field
 3        format('mfm_initialize - Returned field values  = ',
     +    3e14.5,/,'   inconsistent with 0 scale factor')
          mfm_initialize = -3
          return
        endif  ! check for 0 scale factor inconsistency
      else
        if(brtest.gt.0.001)then
          write(6,4)field
 4        format('mfm_initialize - Returned field values = ',
     +    3e14.5,/,'   have incorrect radial component')
          mfm_initialize =-4
          return
        endif
        if(bztest.lt.0.001*fscale)then
          write(6,5)field
 5        format('mfm_initialize - Returned field values = ',
     +    3e14.5,/,'   have incorrect Z component')
          mfm_initialize =-5
        endif
      endif  !  sanity checks of field values
      mfm_initialize = 0
      return
      end
