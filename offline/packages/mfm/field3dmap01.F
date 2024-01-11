      subroutine field3dmap01(n3dr, n3dz, n3dphi, field3d, point3d,
     +                        rvalue, zvalue, phivalue, field,
     +                        rmin, rmax, zmin, zmax, phimin, phimax)
c
c     Original Author: Charles F. Maguire (Vanderbilt)
c     Creation Date: January 5, 1999
c
c     Purpose: return map value from 3D test map
c
c     Qualifications: Restricted to small region of Central Arm
c                     Assumes East-West (azimuthal) symmetry in Stony Brook version
c
c
c     Revision History
c     C.F. Maguire     February 28, 2000  Add second generation 3D map
c     T. K. Ghosh      December 18, 2001  for November 2001 3D Map file
c     C.F. Maguire     November  8, 2002  Fixed interpolation index bug for By2 in dBydphi
c     C.F. Maguire     November 27, 2002  Fixed interpolation index bug for Bz2 in dBzdphi
      implicit none

      integer n3dz
      integer n3dr
      integer n3dphi

      real field3d(n3dr, n3dz, n3dphi, 3)
      real point3d(n3dr, n3dz, n3dphi, 3)
      real rvalue, zvalue, phivalue
      real field(3)
      real x1, x2
c
c     Limits of map file are part of calling paramaters  2/28/2000
c
      real rmin
      real phimin
      real zmin
      real rmax
      real phimax
      real zmax

      integer indexr, indexrlo, indexrhi
      integer indexphi, indexphilo, indexphihi
      integer indexz, indexzlo, indexzhi

      real dBxdr, dBxdz, dBxdphi
      real dBydr, dBydz, dBydphi
      real dBzdr, dBzdz, dBzdphi

      real Bx1, By1, Bz1
      real Bx2, By2, Bz2
      real dr, dz, dphi
      real drvalue, dzvalue, dphivalue

      real rstep
      parameter (rstep = 4.0)
      real phistep
      parameter (phistep = 3.0)
      real zstep
      parameter (zstep = 4.0)

      indexr = (rvalue - rmin)/rstep
      indexphi = (phivalue - phimin)/phistep
      indexz = (zvalue - zmin)/zstep

      indexr = indexr + 1
      indexphi = indexphi + 1
      indexz = indexz + 1

c      write(6,*)" test beg", rvalue, phivalue, zvalue
c
c     Set default field values
c
      field(1) = 0.0
      field(2) = 0.0
      field(3) = 0.0

c
c     Check inner limits
c     Note that INDEXR = 0 for  0 < R < RMIN is allowed
c
      if(indexr.lt.0.or.indexphi.lt.1.or.indexz.lt.1)then
         return !  outside of inner limits
      endif

c
c     Check outer limits
c
      if(indexr.gt.n3dr.or.indexphi.gt.n3dphi.or.
     +   indexz.gt.n3dz)then
         return  ! outside of limit so return 0 field value
      endif

c
c     Special case for INDEXR = 0
c
      if(indexr.eq.0)then
         indexr = 1  ! temporary (?) shortcut
      endif  ! check on INDEXR = 0

c
c     Get bracketing (low, high) indicies
c
      if(indexr.gt.0)then
         if(indexr.lt.n3dr)then
            indexrlo = indexr
         else
            indexrlo = indexr - 1
         endif
         indexrhi = indexrlo + 1
         if(indexz.lt.n3dz)then
            indexzlo = indexz
         else
            indexzlo = indexz - 1
         endif
         indexzhi = indexzlo + 1         
         if(indexphi.lt.n3dphi)then
            indexphilo = indexphi
         else
            indexphilo = indexphi - 1
         endif
         indexphihi = indexphilo + 1

c
c     Interpolate for Bx value
c
         Bx1 = field3d(indexrlo, indexzlo, indexphilo, 1)
         Bx2 = field3d(indexrhi, indexzlo, indexphilo, 1)
          x1 = point3d(indexrhi, indexzlo, indexphilo, 1)
          x2 = point3d(indexrlo, indexzlo, indexphilo, 1)
         dr = point3d(indexrhi, indexzlo, indexphilo, 1) -
     +        point3d(indexrlo, indexzlo, indexphilo, 1)
      
c         write(6,*)"tk",rvalue,phivalue,zvalue,x1,x2,dr,Bx1,Bx2
c         write(6,*)"low grid",indexrlo,indexzlo,indexphilo
         
         if(dr.le. 0.0) return !added by TKG as special case for ndr>76 there are empty points
         if(dr.gt. 100.0) return !added by TKG as special case for ndr>76 there are empty points
         if(dr.lt.0.9*rstep.or.dr.gt.1.1*rstep)then
            write(6,11)dr
 11         format(' DR = ',e12.4)
            stop ' DR invalid?'
         endif
         dBxdr = (Bx2 - Bx1)/dr
         drvalue = rvalue - point3d(indexrlo, indexzlo, indexphilo, 1)

         Bx2 = field3d(indexrlo, indexzhi, indexphilo, 1)
         dz = point3d(indexrlo, indexzhi, indexphilo, 3) -
     +        point3d(indexrlo, indexzlo, indexphilo, 3)
         if(dz.lt.0.9*zstep.or.dz.gt.1.1*zstep)then
            write(6,12)dz, rvalue, rmin, zvalue, zmin,
     +                 phivalue, phimin,
     +                 rstep, phistep, zstep
 12         format(' DZ = ',e12.4,
     +      ' rv, rmin ',2e12.5,/
     +      ' zv, zmin ',2e12.5,/
     +      ' pv, pmin ',2e12.5,/
     +      ' rs, ps, zs ', 3e12.5) 
            write(6,121)point3d(indexrlo, indexzlo, indexphilo, 3),
     +          point3d(indexrlo, indexzhi, indexphilo, 3),
     +          Bx1, Bx2
 121        format(' zlo, zhi ', 2e12.5,
     +             ' bx1, bx2 ', 2e12.5)
            write(6,122)indexrlo, indexphilo,
     +           indexzlo, indexzhi
 122        format(' rlo, plo ',2i12, ' zlo, zhi', 2i12)
            stop ' DZ invalid?'
         endif
         dBxdz = (Bx2 - Bx1)/dz    
         dzvalue = zvalue - point3d(indexrlo, indexzlo, indexphilo, 3)

         Bx2 = field3d(indexrlo, indexzlo, indexphihi, 1)
         dphi = point3d(indexrlo, indexzlo, indexphihi, 2) -
     +          point3d(indexrlo, indexzlo, indexphilo, 2)
         if(dphi.lt.0.9*phistep.or.dphi.gt.1.1*phistep)then
            write(6,13)dphi,phivalue,indexphihi,indexphilo,
     +                 point3d(indexrlo, indexzlo, indexphihi, 2),
     +                 point3d(indexrlo, indexzlo, indexphilo, 2)
 13         format(' DPHI = ',e12.4, e12.4, 2i5,1x,2e12.4)
            stop ' DPHI invalid?'
         endif
         dBxdphi = (Bx2 - Bx1)/dphi 
         dphivalue = phivalue -
     +               point3d(indexrlo, indexzlo, indexphilo,2)

         field(1) = Bx1 + dBxdr*drvalue + dBxdz*dzvalue + 
     +              dBxdphi*dphivalue

c
c     Interpolate for By value
c
         By1 = field3d(indexrlo, indexzlo, indexphilo, 2)
         By2 = field3d(indexrhi, indexzlo, indexphilo, 2)
         dBydr = (By2 - By1)/dr

         By2 = field3d(indexrlo, indexzhi, indexphilo, 2)
         dBydz = (By2 - By1)/dz

         By2 = field3d(indexrlo, indexzlo, indexphihi, 2)
         dBydphi = (By2 - By1)/dphi

         field(2) = By1 + dBydr*drvalue + dBydz*dzvalue + 
     +              dBydphi*dphivalue

c
c     Interpolate for Bz value
c
         Bz1 = field3d(indexrlo, indexzlo, indexphilo, 3)
         Bz2 = field3d(indexrhi, indexzlo, indexphilo, 3)
         dBzdr = (Bz2 - Bz1)/dr

         Bz2 = field3d(indexrlo, indexzhi, indexphilo, 3)
         dBzdz = (Bz2 - Bz1)/dz

         Bz2 = field3d(indexrlo, indexzlo, indexphihi, 3)
         dBzdphi = (Bz2 - Bz1)/dphi

         field(3) = Bz1 + dBzdr*drvalue + dBzdz*dzvalue + 
     +              dBzdphi*dphivalue

    
      endif  ! check on INDEXR > 0

c      write(6,*)"fvalue", field(1),field(2),field(3)

      return
      end





