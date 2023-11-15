cMM   02.Jun.2000
c
c	file emc_coord.inc
c
c     provides shift values for the 'emc_pbgl_coord' routine
c     ==> retracted geometry
c
      real*4 emc_rpos
      real*4 emc_dtof
c
      real*4 x_shift      ! shift value x
      real*4 y_shift      ! shift value y
      real*4 z_shift      ! shift value z
c
      COMMON/EMC_COORD/ x_shift,y_shift,z_shift,
     +                  emc_rpos,emc_dtof
cMM
