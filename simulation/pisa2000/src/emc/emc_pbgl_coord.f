

*     author: Markus Merschmeyer, 02.06.2000


      subroutine emc_pbgl_coord(x,y,z,sector,l_momentum)
      

*     called by emc_gustep.f

*     The routine rotates a vector from the phenix coordinate system into the
*     coordinate system of the pbgl sector which is hit (sector7 or sector 8).

*     This must be done to calculate the proper angle of the photon before
*     Henner's parametrization for the Cherenkov effect in PbGl can be used.

*     Phenix coordinate system:

*                     ^ y
*                     |    z (N)
*                     |   /
*                     |  /
*                     | /
*      x (W) <--------|/

*     Coordinate system of Sector 7 (east arm!!!)

*                     ^ y   x (N)	- the coordinate system of sector 8 is
*                     |   /		  tilted for about 22 degrees counter-
*                     |  /		  clockwise around the x axis with respect
*                     | /		  to the system of sector 7
*                     |/		
*                     |--------> z (E)


*      x, y, z		: input coordinates, overwritten with coordinates
*			  in rotated system on output
*      sector		: number of the pbgl sector for which the transformation
*			  is to be calculated
*      l_momentum       : if 1 the sector 8 22-degree rotation matrix is used
*                          for calculating the angle between cherenkov photon
*                          momentum and the module axis
*                         if 0 the sector 8 shift-dependent matrix is used
*                          for calculating the position of the cherenkov photon


      implicit none

c     include rotation matrix cosinus & sinus values

#include "emc_coord.inc"



      integer	i,sector
      integer   l_first/1/      ! indicates first time routine is used
      integer   l_momentum      ! indicates momentum or location rotation
      real*4	x,y,z		! input coordinates
      real*4	xvec(3)		! vector for calculation
      real*4	sec7crd(3,3)	! sector 7 rotation matrix
      real*4    sec8crd(3,3)	! sector 8 rotation matrix

      real*4    shift_x

      common /coord/ shift_x,sec7crd,sec8crd
      save   /coord/


c     if routine is used for the first time, define
c     the rotation matrices

      if(l_first.eq.1) then

       l_first = 0

c     rotation matrix for the coord. system of sector 7
c     (matrix is not altered by carriage shifts)

        sec7crd(1,1)=0.0
        sec7crd(1,2)=0.0
        sec7crd(1,3)=1.0
        sec7crd(2,1)=0.0
        sec7crd(2,2)=1.0
        sec7crd(2,3)=0.0
        sec7crd(3,1)=-1.0
        sec7crd(3,2)=0.0
        sec7crd(3,3)=0.0

c     momentum rotation matrix for the coord. system of sector 8
c     (this matrix is a product (A*B) of the rotation
c      matrix for sector 7 (A) and a rotation matrix (B)
c      for the angle 22deg around the z axis)

        sec8crd(1,1)=0.0
        sec8crd(1,2)=0.0
        sec8crd(1,3)=1.0
        sec8crd(2,1)=-0.374607
        sec8crd(2,2)=0.927184
        sec8crd(2,3)=0.0
        sec8crd(3,1)=-0.927184
        sec8crd(3,2)=-0.374607
        sec8crd(3,3)=0.0

        shift_x = x_shift
        write(*,*) 'emc_pbgl_coord, x-shift: ',shift_x

      endif ! (end) rotation matrix definition



c     initialize

      xvec(1) = x
      xvec(2) = y
      xvec(3) = z
      x = 0.0
      y = 0.0
      z = 0.0

c     calculate new coordinates

      if (sector.eq.7) then
       if(l_momentum.eq.1) then
        do i=1,3,1
          x = x + sec7crd(1,i)*xvec(i)	! new x value
          y = y + sec7crd(2,i)*xvec(i)	! new y value
          z = z + sec7crd(3,i)*xvec(i)	! new z value
        enddo
       elseif(l_momentum.eq.0) then ! location transformation

        xvec(1) = xvec(1) - shift_x

        do i=1,3,1
          x = x + sec7crd(1,i)*xvec(i)  ! new x value
          y = y + sec7crd(2,i)*xvec(i)  ! new y value
          z = z + sec7crd(3,i)*xvec(i)  ! new z value
        enddo
       else
        stop '==>emc_pbgl_coord: invalid kind of transformation!!!'
       endif
      else if (sector.eq.8) then
       if(l_momentum.eq.1) then     ! momentum transformation
        do i=1,3,1
          x = x + sec8crd(1,i)*xvec(i)	! new x value
          y = y + sec8crd(2,i)*xvec(i)	! new y value
          z = z + sec8crd(3,i)*xvec(i)	! new z value
        enddo
       elseif(l_momentum.eq.0) then ! location transformation

        xvec(1) = xvec(1) - shift_x

        do i=1,3,1
          x = x + sec8crd(1,i)*xvec(i)  ! new x value
          y = y + sec8crd(2,i)*xvec(i)  ! new y value
          z = z + sec8crd(3,i)*xvec(i)  ! new z value
        enddo
       else
        stop '==>emc_pbgl_coord: invalid kind of transformation!!!'
       endif
      else
        stop '==>emc_pbgl_coord: invalid sector number!!!'
      endif

      return
      
      end
