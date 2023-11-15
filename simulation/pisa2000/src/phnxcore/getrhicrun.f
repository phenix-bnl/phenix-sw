      integer function getrhicrun(isubrun)

c     Function to return the RHICRUN and RHICSUBRUN numbers

      implicit none
#include "guphnx.inc"

      integer isubrun
 
      isubrun = rhicsubrun
      getrhicrun = rhicrun

      return
      end
