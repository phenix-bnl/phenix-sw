      subroutine serve (arg1, arg2, arg3, arg4, index)
c
c     Used by beam gas event generator
c
      implicit none
C
      real*4 arg1, arg2, arg3, arg4, difference, quotient
C
      integer*4 index
C
      if (arg1 .lt. arg2 .or. arg1 .gt. arg3) then
         index = -1
         return
      else
         difference = arg1 - arg2
         quotient = difference / arg4
         index = int (quotient) + 1
         return
      endif
      end
