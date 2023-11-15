*CMZ :  2.04/00 18/11/94  18.14.34  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   18/11/94
      subroutine gdgirl
      implicit none
c
c     dummy subroutine for GEANT321
c
      integer ifirst /0/
      save ifirst
      if(ifirst.eq.0)then
         write(6,*)'  Call to GDGIRL ?'
         ifirst = 1
      endif  ! first time call check
      return
      end
