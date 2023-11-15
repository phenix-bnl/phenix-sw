*-- Author : K.Shigaki
      subroutine CRK_ROT(pos, phi)
*********************
*  FILE crk_rot.f
*********************

      implicit none

#include "g77trigdef.inc"


c created by KS, 14.Feb.96

c var
      real pos(3), phi
      real x, y
c begin
      x = pos(1) * cosd(phi) - pos(2) * sind(phi)
      y = pos(1) * sind(phi) + pos(2) * cosd(phi)
      pos(1) = x
      pos(2) = y
c end
      return
      end
