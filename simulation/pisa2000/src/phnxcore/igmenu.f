*CMZ :  2.04/00 01/06/93  11.49.51  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   08/05/93
      subroutine igmenu
      implicit none
c
c     dummy entries for unresolved SGI calls
c
      integer ifirst1/0/
      integer ifirst2/0/
      save ifirst1, ifirst2
      if(ifirst1.eq.0)then
         ifirst1 = 1
         write(6,*)'  Call to IGMENU ??'
      endif
      return
      entry freq
      if(ifirst2.eq.0)then
         ifirst2 = 1
         write(6,*)'  Call to FREQ ??'
      endif
      return
      end
