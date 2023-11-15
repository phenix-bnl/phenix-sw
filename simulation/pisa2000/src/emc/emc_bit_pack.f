      subroutine emc_bit_pack(i1, iwall, itype, dele, pos_x,
     +                        pos_y, pos_z, tof, ind1, ind2,
     +                        numed, spart, ncycle, itrack,
     +                        iword)
      implicit none
c
c
c     Purpose is to bit-pack the EMCal PISA output into 4 integers
c     This will reduce the size of the PISA output file by factor of 4
c
c     Author: Charles F. Maguire, initial version August 10, 1995
c
c     Revisions
c     C.F. Maguire   August 17, 1996    Extensions for PbGlass walls
c                                       Different max and min values
c                                       Switch IND1 and IND2 roles
c                                       Different range for PbGl DELE
c
c     Calling variables (all input except for iword)
c
      integer  i1        ! detector counter
      integer  iwall     ! wall number
      integer  itype     ! counter type
      real     dele      ! energy loss (GeV)
      real     pos_x     ! x position
      real     pos_y     ! y position
      real     pos_z     ! z position
      real     tof       ! time-of-flight
      integer  ind1      ! super-module pointer
      integer  ind2      ! cell pointer
      integer  numed     ! tracking medium number
      integer  spart     ! particle ID number
      integer  ncycle    ! number of ancestry cycles
      integer  itrack    ! ancestor track number
      integer  iword(4)  ! output bit packed words with all above info

      integer ishft      ! bit shifting function works on ALL UNIX compilers
c
c     local variables
c
      real     rpos        ! Radial position
      real  r_max, r_min   ! Max and min R values  (according to wall)
      real r_min_sc, r_max_sc  ! For Pb Scint
      parameter (r_min_sc = 510.0)
      parameter (r_max_sc = 560.0)
      real r_min_gl, r_max_gl  ! For Pb Glass
      parameter (r_min_gl = 565.0)
      parameter (r_max_gl = 615.0)
      real  r_step         ! Step size in R
      parameter (r_step = 0.05) ! corresponds to 10 bits
      integer iword_r

      real z_max, z_min    ! Max and min Z values
      parameter (z_max = 200.0)
      parameter (z_min = -200.0)
      real z_step               ! Step size in Z coordinate
      parameter (z_step = 0.10) ! corresponds to 12 bits
      integer iword_z

      real x_max, x_min    ! Max and min X values (one side, as per wall)
      real x_max_sc, x_min_sc      ! Pb Scint
      parameter (x_max_sc = 560.0) ! Corresponds to 0 degrees PHI
      parameter (x_min_sc = 280.0) ! Corresponds to 56.25 degrees PHI
      real x_max_gl, x_min_gl      ! Pb Glass
      parameter (x_max_gl = 610.0) ! Corresponds to 0 degrees PHI
      parameter (x_min_gl = 490.0) ! Corresponds to 56.25 degrees PHI
      real x_step               ! Step size in X coordinate
      parameter (x_step = 0.07) ! Corresponds to 12 bits
      real apos_x               ! Absolute value of pos_x
      integer iword_x

      integer iword_quad        ! XY quadrant, mapped down one unit

      real dele_max, dele_min   ! Max DELE value (as per wall), and min
      real dele_max_sc          ! Pb Scint
      parameter (dele_max_sc = 0.0327) ! corresponds to 15 bits at 1 keV
      real dele_max_gl          ! Pb Glass
      parameter (dele_max_gl = 0.256) ! correspond to 15 bits at 8 keV
      parameter (dele_min = 0.0)
      real dele_step       ! Step size in DELE
      real dele_step_sc    ! Step size in DELE for Pb Scint
      parameter (dele_step_sc = 1.0e-6) ! 1 keV
      real dele_step_gl    ! Step size in DELE for Pb Glass
      parameter (dele_step_gl = 8.0e-6) ! 8 keV
      integer iword_dele

      real tof_max, tof_min      ! Max and min TOF value (ns)
      parameter (tof_min = 17.0) ! Corresponds to 510 cm position
      parameter (tof_max = 345.) ! corresponds to 15 bits
      real tof_step       ! Step size in TOF (as in PISA)
      parameter (tof_step = 0.010) ! 10 ps
      integer iword_tof

      integer iwall_max
      parameter (iwall_max = 8)
      integer iword_iwall

      integer itype_max
      parameter (itype_max = 3)
      integer iword_itype

      integer i1_max
      parameter (i1_max = 8)
      integer iword_i1

      integer ind1_max              ! According to wall
      integer ind1_max_sc           ! Pb Scint
      parameter (ind1_max_sc = 18)
      integer ind1_max_gl           ! Pb Glass
      parameter (ind1_max_gl = 192)
      integer iword_ind1

      integer ind2_max              ! According to wall
      integer ind2_max_sc           ! Pb Scint
      parameter (ind2_max_sc = 144)
      integer ind2_max_gl           ! Pb Glass
      parameter (ind2_max_gl = 24)
      integer iword_ind2

      integer itrack_max
      parameter (itrack_max = 32767)  ! corresponds to 15 bits
      integer iword_itrack

      integer spart_max
      parameter (spart_max = 62)      ! normal particles
      integer iword_spart

      integer ncycle_max
      parameter (ncycle_max = 31)
      integer iword_ncycle

      integer iword_numed

      integer iword1, iword2, iword3, iword4  ! local values of iword array

      integer  iover_dele  ! DELE overflow
      integer  iover_track ! Track number overflow
      integer  iover_tof   ! TOF overflow
      integer  iover_spart ! Particle ID overflow
      integer  iover_ncycle ! Cycle number overflow
c
c     begin execution
c
      iword1 = 0 ! initialize
c
c     IWORD(1) 30 bits used, 2 bits spare
c              R value between 510 and 560 cm, 10 bits at 0.05 cm
c              Z value between -200 and +200 cm, 12 bits at 0.10 cm
c              IWALL between 1 and 8, 3 bits (map one unit down)
c              ITYPE between 1 and 3, 2 bits
c              I1 beween 1 and 7, 3 bits (map one unit down)
c
      rpos = sqrt(pos_x*pos_x + pos_y*pos_y)
      if(iwall.lt.7)then
         r_min = r_min_sc
         r_max = r_max_sc
      else
         r_min = r_min_gl
         r_max = r_max_gl
      endif  ! check on Pb Scint or Pb Glass
      if(rpos.ge.r_min.and.rpos.le.r_max)then
         iword_r = (rpos - r_min)/r_step     ! using 10 bits
      else
         write(6,1)pos_x,pos_y,rpos
 1       format(' EMC_BIT_PACK <W>: R_POS value out of range',/,
     +          ' POS_X, POS_Y, R_POS ',3e12.4)
         iword_r = 0
      endif  ! safety check on r_pos
      iword1 = iword_r

      if(pos_z.ge.z_min.and.pos_z.le.z_max)then
         iword_z = (pos_z - z_min)/z_step   ! using 12 bits
      else
         write(6,2)pos_z
 2       format(' EMC_BIT_PACK <W>: Z_POS value out of range',
     +          '  Z_POS ',e12.4)
         iword_z = 0
      endif  ! safety check on z_pos
      iword1 = iword1 + ishft(iword_z,10)  ! shift left 10 bits

      if(iwall.ge.1.and.iwall.le.iwall_max)then
         iword_iwall = iwall - 1   ! using 3 bits
      else
         write(6,3)iwall
 3       format(' EMC_BIT_PACK <W>: IWALL value out of range',
     +          '  IWALL ',i10)
         iword_iwall = 0
      endif ! check on iwall value
      iword1 = iword1 + ishft(iword_iwall,22)  ! shift left 22 bits

      if(itype.ge.1.and.itype.le.itype_max)then
         iword_itype = itype    ! using 2 bits
      else
         write(6,4)itype
 4       format(' EMC_BIT_PACK <W>: ITYPE value out of range',
     +          '  ITYPE ',i10)
         iword_itype = 0
      endif ! check on itype value
      iword1 = iword1 + ishft(iword_itype,25)  ! shift left 25 bits

      if(i1.ge.1.and.i1.le.i1_max)then
         iword_i1 = i1 - 1  ! using 3 bits
      else
         write(6,5)i1
 5       format(' EMC_BIT_PACK <W>: I1 value out of range',
     +          '  I1 ',i10)
         iword_i1 = 0
      endif ! check on i1 value
      iword1 = iword1 + ishft(iword_i1,27)  ! shift left 27 bits

      iword2 = 0 ! initialize
c
c     IWORD(2) 30 bits used, 2 bits spare
c              X value between 280 and 560 cm, 12 bits at 0.07 cm
c              XYQUAD  XY quadrant between 0 and 3, 2 bits
c              TRACK between 1 and 32767, 15 bits
c              Track overflow bit, 1 bit
c              

      apos_x = abs(pos_x)
      if(iwall.lt.7)then
         x_min = x_min_sc
         x_max = x_max_sc
      else
         x_min = x_min_gl
         x_max = x_max_gl
      endif  ! check on Pb Scint or Pb Glass
      if(apos_x.ge.x_min.and.apos_x.le.x_max)then
         iword_x = (apos_x - x_min)/x_step   ! using 12 bits
      else
         write(6,6)pos_x
 6       format(' EMC_BIT_PACK <W>: X_POS value out of range',
     +          '  X_POS ',e12.4)
         iword_x = 0
      endif ! check on pox_x value
      iword2 = iword_x

      if(pos_x.ge.0.0.and.pos_y.ge.0.0)then
         iword_quad = 0  ! first quadrant
      elseif(pos_x.lt.0.0.and.pos_y.ge.0.0)then
         iword_quad = 1  ! second quadrant
      elseif(pos_x.lt.0.0.and.pos_y.le.0.0)then
         iword_quad = 2  ! third quadrant
      elseif(pos_x.ge.0.0.and.pos_y.le.0.0)then
         iword_quad = 3  ! fourth quadrant
      endif ! check on quadrant
      if(iword_quad.gt.0)then
         iword2 = iword2 + ishft(iword_quad,12)
      endif ! check on quadrant word not 0

      if(itrack.le.itrack_max)then
         iword_itrack = itrack  ! using 15 bits
         iover_track = 0
      else
         iword_itrack = 0
         iover_track = 1        ! track number overflow indicator
      endif  
      iword2 = iword2 + ishft(iword_itrack,14)
      if(iover_track.gt.0)then
         iword2 = iword2 + ishft(iover_track,29)
      endif ! check on track overflow word not 0

      iword3 = 0 ! initialize
c
c     IWORD(3) 32 bits used
c              DELE value between 0 and 32.7 MeV, 15 bits at 1 keV PbSc
c              DELE value between 0 and 256 MeV, 15 bits at 8 keV PbGl
c              DELE overflow bit
c              TOF value between 17 and 345 ns, 15 bits at 10 ps
c              TOF overflow bit
c              
      iword_dele = 0
      if(iwall.lt.7)then
         dele_max = dele_max_sc
         dele_step = dele_step_sc
      else
         dele_max = dele_max_gl
         dele_step = dele_step_gl
      endif  ! check if Pb Scint or Pb Glass
      if(dele.lt.dele_min)then
         write(6,10)dele,dele_min
 10      format(' EMC_BIT_PACK <W>: DELE less than DELE_MIN',
     +          ' DELE, DELE_MIN ',2e12.4)
      elseif(dele.le.dele_max)then
         iword_dele = (dele - dele_min)/dele_step  ! using 15 bits
         iover_dele = 0
      elseif(dele.gt.dele_max)then
         iover_dele = 1        ! DELE overflow indicator
      endif  
      iword3 = iword_dele
      if(iover_dele.gt.0)then
         iword3 = iword3 + ishft(iover_dele,15)
      endif ! check on dele overflow word not 0

      iword_tof = 0
      if(tof.lt.tof_min)then
         write(6,9)tof,tof_min
 9       format(' EMC_BIT_PACK <W>: TOF less than TOF_MIN',
     +          ' TOF, TOF_MIN ',2e12.4,/,
     +          ' Super-relativistic particle?')
      elseif(tof.le.tof_max)then
         iword_tof = (tof - tof_min)/tof_step ! using 15 bits
         iover_tof = 0
      elseif(tof.gt.tof_max)then
         iover_tof = 1        ! TOF overflow indicator
      endif  
      iword3 = iword3 + ishft(iword_tof,16)
      if(iover_tof.gt.0)then
         iword3 = iword3 + ishft(iover_tof,31)
      endif ! check on tof overflow word not 0

      iword4 = 0 ! initialize
c
c     IWORD(4) 28 bits used, 4 spare
c              IND1 value between 1 and 18, 5 bits
c              IND2 value between 1 and 155, 8 bits
c              SPART value between 1 and 62, 6 bits
c              SPART overflow bit
c              NCYCLE value between 1 and 32, 5 bits
c              NCYCLE overflow bit
c              NUMED pointer, 2 bits (map NUMED value)
c              
      if(iwall.lt.7)then
         ind1_max = ind1_max_sc
         if(ind1.ge.1.and.ind1.le.ind1_max)then
            iword_ind1 = ind1  ! 5 bits
         else
            write(6,7)ind1
 7          format(' EMC_BIT_PACK <W>: IND1 out of range',
     +          '  IND1 ',i10)
         endif
      else
         ind2_max = ind2_max_gl
         if(ind2.ge.1.and.ind2.le.ind2_max_gl)then
            iword_ind1 = ind2  ! Note switch of variable for PbGl
         else
            write(6,8)ind2
         endif
      endif ! Check on Pb Scint or Pb Glass
      iword4 = iword_ind1

      if(iwall.lt.7)then
         ind2_max = ind2_max_sc
         if(ind2.ge.1.and.ind2.le.ind2_max)then
            iword_ind2 = ind2  ! 8 bits
         else
            write(6,8)ind2
 8          format(' EMC_BIT_PACK <W>: IND2 out of range',
     +             '  IND2 ',i10)
         endif
      else
         ind1_max = ind1_max_gl
         if(ind1.ge.1.and.ind1.le.ind1_max)then
            iword_ind2 = ind1  ! Note switch of variable for PbGl
         else
            write(6,7)ind1
         endif         
      endif ! Check on Pb Scint or Pb Glass
      iword4 = iword4 + ishft(iword_ind2,5)

      if(spart.ge.1.and.spart.le.spart_max)then
         iword_spart = spart  ! 6 bits
         iover_spart = 0
      else
         iover_spart = 1
      endif
      iword4 = iword4 + ishft(iword_spart,13)
      if(iover_spart.gt.0)then
         iword4 = iword4 + ishft(iover_spart,19)
      endif

      if(ncycle.ge.0.and.ncycle.le.ncycle_max)then
         iword_ncycle = ncycle  ! 5 bits
         iover_ncycle = 0
      else
         iover_ncycle = 1
      endif
      iword4 = iword4 + ishft(iword_ncycle,20)
      if(iover_ncycle.gt.0)then
         iword4 = iword4 + ishft(iover_ncycle,25)
      endif

c
c     NUMED mapping needs work
c
      if(numed.eq.98)then
         iword_numed = 1
      else
         iword_numed = 0
      endif
      iword4 = iword4 + ishft(iword_numed,26)

      iword(1) = iword1
      iword(2) = iword2
      iword(3) = iword3
      iword(4) = iword4

      return
      end
