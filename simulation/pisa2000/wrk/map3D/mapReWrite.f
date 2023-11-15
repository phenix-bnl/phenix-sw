      program mapReWrite
      implicit none
c
c     Author: Charles F. Maguire
c     Creation Date: September 3, 2000
c
c     Purpose: Rewrite ASCII text file for 3D field map into
c              loop order: phi, z, r
c
c              Backward compatibility for previous field map versions
c
c              Reformatted ASCII output is then converted into a ROOT
c              NTUPLE file using the ReadAll.C macro interactively in ROOT
c
c     Parameters of Stony Brook 3D map (April 2000)
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

      real point3d(n3dr, n3dz, n3dphi, 3)
      real field3d(n3dr, n3dz, n3dphi, 3)

      real z, r, phi, bz, br, bphi
      real bkey  ! 0 if bphi is 0, 1 otherwise

      real rVal, zVal, phiVal
      real rStart /0.0/
      real zStart /-200.0/
      real phiStart /0.0/
      real rDel /4.0/
      real zDel /4.0/
      real phiDel /4.0/

      integer iOut

c
c     Original input file (z,r,phi sequence, with r,z,phi)
c
      open(unit=1, file='ascii3d.map', status='old',
     +     err=99, access='sequential', form='formatted')

c
c     Reformatted output file (r,z,phi sequence, omit r,z,ph)
c
      open(unit=2, file='simuAll.dat', status='unknown',
     +     err=99, access='sequential', form='formatted')

c
c     diagnostic output for Z = 0 points
c
      open(unit=6, file='simuAll.out', status='unknown',
     +     err=99, access='sequential', form='formatted')

      do i3dz = 1, n3dz
         do i3dr = 1, n3dr
            do i3dphi = 1,n3dphi
               read(1,2,err=97,end=95)z, r, phi, bz, br, bphi
2              format(3f6.3,3f9.3)
               point3d(i3dr, i3dz, i3dphi, 1) = r
               point3d(i3dr, i3dz, i3dphi, 2) = z
               point3d(i3dr, i3dz, i3dphi, 3) = phi

               field3d(i3dr, i3dz, i3dphi, 1) = br
               field3d(i3dr, i3dz, i3dphi, 2) = bz
               field3d(i3dr, i3dz, i3dphi, 3) = bphi

            enddo  !  loop over phi
         enddo  !  loop over r
      enddo  ! loop over z

      iOut = 0
      rVal = rStart
      do i3dr = 1, n3dr
         zVal = zStart
         do i3dz = 1, n3dz
            phiVal = phiStart
            do i3dphi = 1,n3dphi
               bkey = 1.0
               if(field3d(i3dr,i3dz,i3dphi,3).eq.0.0)then
                  bkey = 0.0
               endif

               iout = iout + 1
               write(2,3)field3d(i3dr, i3dz, i3dphi, 2),
     +                   field3d(i3dr, i3dz, i3dphi, 1),
     +                   field3d(i3dr, i3dz, i3dphi, 3), bkey
3              format(3(2x,f9.1),3x,f3.1)

               if(zVal.eq.0.0.and.phiVal.eq.0.0)then
                  write(6,4)field3d(i3dr, i3dz, i3dphi, 2),
     +                      field3d(i3dr, i3dz, i3dphi, 1),
     +                      field3d(i3dr, i3dz, i3dphi, 3),
     +                      rVal, iout
 4                format(3(2x,f9.1),3x,f7.1,3x,i10)
               endif

               phiVal = phiVal + phiDel
            enddo  !  loop over phi
            zVal = zVal + zDel
         enddo  ! loop over z
         rVal = rVal + rDel
      enddo  !  loop over r

      stop '  normal end'

95    continue
      stop ' eof'
97    continue
      stop ' err'
99    continue
      stop ' no file'

      end
