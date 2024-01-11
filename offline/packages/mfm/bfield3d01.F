      subroutine bfield3d01(xyz, field)

      implicit none

#include "g77trigdef.inc"

c
c     Original Author: Charles F. Maguire (bfield3d version)
c     Creation Date: February 26, 2000
c
c     Purpose: call the initialization/interpolation of the BNL 3D map field
c
c     Revision History
c        C.F. Maguire  September 3, 2000   Change for April 2000 3D field map
c        T. K. Ghosh   December 3, 2001    Change for the November 2001 3D field map
c
c     Calling Map: called by gufld
c                  calls read3droot01 (reading of ROOT NTUPLE file for 3D map)
c
c     Calling Parameters:
c
c     xyz(3)      Input of space point
c     field(3)    Output of Bx, By, and Bz values (kG)
c
      real xyz(3)
      real field(3)
c
c      BNL 3D map (November 01)
c     -476 =< Z <= +152 cm in 4 cm steps
c     0 =< R <= 400 cm in 4 cm steps
c     0 =< Phi <= 357 in 3 degree steps
c

      integer n3dz, i3dz
      parameter (n3dz=158)
      integer n3dr, i3dr
      parameter (n3dr=91)
      integer n3dphi, i3dphi
      parameter (n3dphi=120)
      integer n3dtotal, ientry
      parameter (n3dtotal = 1286640)

      real field3d(n3dr, n3dz, n3dphi, 3)
      real point3d(n3dr, n3dz, n3dphi, 3)

      integer ivect
      real vector3d(n3dtotal*6)
      ! common /magfield/ field3d, point3d, vector3d

      real z, phi, r
      real zmin /-476.0/
      real zmax /+152.0/
      real rmin / 0.0/
      real rmax / 360.0/
      real phimin /0.0/
      real phimax /357.0/
      real br
      real bphi

      real rstep
      parameter (rstep = 4.0)
      real phistep
      parameter (phistep = 3.0)
      real zstep
      parameter (zstep = 4.0)




      real rvalue, phivalue, zvalue

      integer ifirst /1/

      if(ifirst.eq.1)then

         call vzero(field3d,n3dr*n3dz*n3dphi*3) ! initialization of field array
         call vzero(point3d,n3dr*n3dz*n3dphi*3) ! initialization of point array
         ifirst = 0
         write (6,1)
 1       format('bfield3d01 - Reading 3D Magnetic Field Map Root File',
     +          ', November 2001 version')
         call read3droot01(vector3d,n3dtotal)
c
c        Transfer to 3D array
c
         ivect = 1

             do ientry= 1,n3dtotal


                  z = vector3d(ivect)
                  r = vector3d(ivect+1)
                  phi = vector3d(ivect+2)

                  i3dr = (r-rmin)/rstep+1
                  i3dz = (z-zmin)/zstep+1
                  i3dphi = (phi-phimin)/phistep+1
c                  if(i3dr.gt.89)then
c                  write(*,*) " test for grid", i3dr, i3dz, i3dphi
c                  endif
                  point3d(i3dr,i3dz,i3dphi,1) = r
                  point3d(i3dr,i3dz,i3dphi,2) = phi
                  point3d(i3dr,i3dz,i3dphi,3) = z

                  field3d(i3dr,i3dz,i3dphi,3) = vector3d(ivect+3)
                  br = vector3d(ivect+4)
                  bphi  = vector3d(ivect+5)
                  field3d(i3dr,i3dz,i3dphi,1) = br*cosd(phi) -
     +                                          bphi*sind(phi)
                  field3d(i3dr,i3dz,i3dphi,2) = br*sind(phi) +
     +                                          bphi*cosd(phi)
                  ivect = ivect+6

         enddo ! loop over total entry


      endif  ! initialization sequence

      field(1) = 0.0
      field(2) = 0.0
      field(3) = 0.0

      rvalue = sqrt(xyz(1)*xyz(1) + xyz(2)*xyz(2))
      zvalue = xyz(3)
      if ((xyz(2).eq.0.0).and.(xyz(1).eq.0.0)) then
        phivalue = 0.0
      else
        phivalue = atan2d(xyz(2), xyz(1))
      endif
      if(phivalue.lt.0.0)phivalue = 360.0 + phivalue

      call field3dmap01(n3dr, n3dz, n3dphi, field3d, point3d,
     +                  rvalue, zvalue, phivalue, field,
     +                  rmin, rmax, zmin, zmax, phimin, phimax)

      return
      end







