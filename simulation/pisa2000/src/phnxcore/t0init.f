      subroutine t0init
      implicit none


c     global variables

#include "subevt.inc"
#include "kincut.inc"

c     local variables

      integer icall /0/
      real t0phnx   ! nano-seconds
      real tran     ! GEANT random number


c     begin execution

      if(t0width.eq.0.0.and.t0cent.eq.0.0)then
         return
      endif  ! check if 0 values (ns) for centroid and width

      icall = icall + 1   ! should match full event number
      call grndm(tran,1) 
      t0phnx = t0cent + 2.0*t0width*(tran-0.5)  ! in ns to get t0 +/- t0width
      t0start = t0phnx*1.0e-09  ! put in common block as seconds
      write(6,1)t0phnx, icall
1     format(/,'  T0INIT: t0phnx = ',e12.4,' ns,  ICALL = ',i6,/)

      return
      end
