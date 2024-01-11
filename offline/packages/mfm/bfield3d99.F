      subroutine bfield3d99(xyz, field)

      implicit none

#include "g77trigdef.inc"

c
c     Original Author: Charles F. Maguire
c     Creation Date: February 26, 2000
c
c     Purpose: call the initialization/interpolation of the BNL 3D map field
c
c     Revision History
c        C.F. Maguire  September 3, 2000   Change for April 2000 3D field map
c
c     Calling Map: called by gufld
c                  calls read3droot (reading of ROOT NTUPLE file for 3D map)
c
c     Calling Parameters:
c
c     xyz(3)      Input of space point
c     field(3)    Output of Bx, By, and Bz values (kG)
c
      real xyz(3)
      real field(3)
c
c     Parameters of BNL 3D map (October 1999)
c     -200 =< Z <= +200 cm in 4 cm steps
c     0 =< R <= 400 cm in 4 cm steps
c     0 =< Phi <= 367 in 3 degree steps
c

      integer n3dz, i3dz
      parameter (n3dz=101)
      integer n3dr, i3dr
      parameter (n3dr=101)
      integer n3dphi,i3dphi
      parameter (n3dphi=120)
      integer n3dtotal
      parameter (n3dtotal = n3dz*n3dr*n3dphi)

      real field3d(n3dr, n3dz, n3dphi, 3)
      real point3d(n3dr, n3dz, n3dphi, 3)

      integer ivect
      real vector3d(n3dz*n3dr*n3dphi*3)

      ! common /magfield/ field3d, point3d, vector3d

      real z, phi, r
      real zmin /-200.0/
      real zmax /+200.0/
      real rmin / 0.0/
      real rmax / 400.0/
      real phimin /0.0/
      real phimax /357.0/
      real delphi /3.0/
      real delz /4.0/
      real delr /4.0/
      real br
      real bphi

      real rvalue, phivalue, zvalue

      integer ifirst /1/

      if(ifirst.eq.1)then
         ifirst = 0
         write (6,1)
 1       format('bfield3d99 - Reading 3D Magnetic Field Map Root File')
         call read3droot(vector3d,n3dtotal)
c
c        Transfer to 3D array
c
         ivect = 1
         r = rmin
         do i3dr = 1,n3dr
            z = zmin
            do i3dz = 1,n3dz
               phi = phimin
               do i3dphi = 1,n3dphi
                  point3d(i3dr,i3dz,i3dphi,1) = r
                  point3d(i3dr,i3dz,i3dphi,2) = phi
                  point3d(i3dr,i3dz,i3dphi,3) = z
                  field3d(i3dr,i3dz,i3dphi,3) = vector3d(ivect)
                  br = vector3d(ivect+1)
                  bphi  = vector3d(ivect+2)
                  field3d(i3dr,i3dz,i3dphi,1) = br*cosd(phi) -
     +                                          bphi*sind(phi)
                  field3d(i3dr,i3dz,i3dphi,2) = br*sind(phi) +
     +                                          bphi*cosd(phi)
                  phi = float(i3dphi)*delphi
                  ivect = ivect + 3
               enddo  !  loop over phi
               z = zmin + float(i3dz)*delz
            enddo ! loop over z
            r = rmin + float(i3dr)*delr
         enddo ! loop over r

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

      call field3dmap(n3dr, n3dz, n3dphi, field3d, point3d,
     +                rvalue, zvalue, phivalue, field,
     +                rmin, rmax, zmin, zmax, phimin, phimax)

      return
      end
