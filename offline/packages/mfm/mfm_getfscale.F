      integer function mfm_getfscale(fscale)
      implicit none

c     PHOOL utility interface to Magnetic Field Map reading software
c     Author: CHarles F. Maguire
c     Creation date: January 24, 2002
c
c     Calling map:
c          Called by any PHOOL function
c
c     Calling variables
c     fscale   floating point magnetic field scaling factor

#include "mfm.inc"

      real    fscale
      fscale = rpvolu_opt(2,1)
      mfm_getfscale = 1
      return
      end

