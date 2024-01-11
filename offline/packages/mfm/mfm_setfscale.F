      integer function mfm_setfscale(fscale)
      implicit none

c
c     PHOOL utility interface to Magnetic Field Map reading software
c     Author: CHarles F. Maguire
c     Creation date: January 24, 2002
c     Calling map:
c          Called by any PHOOL function
c     Calling variables
c     fscale   floating point magnetic field scaling factor
c     Revision History

c     Global variables
      real    fscale

#include "mfm.inc"
      
      rpvolu_opt(2,1) = fscale
      mfm_setfscale = 1
      return 
      end
