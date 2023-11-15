        subroutine        rect_to_polar( x )

      implicit none

#include "g77trigdef.inc"

        real*8        x( 6 )
 
        x( 4 )  = sqrt( x( 1 )**2 + x( 2 )**2 + x( 3 )**2 )
 
        if ( x( 4 ) .gt. 0.0d0 ) then
                x( 5 ) = acosd( x( 3 ) / x( 4 ) )
 
                if ( x( 1 ) .ne. 0.0d0  .or.  x( 2 ) .ne. 0.0d0 ) then
                        x( 6 ) = atan2d( x( 2 ), x( 1 ) )
                else
                        x( 6 ) = 0.0d0
                end if
 
                if ( x( 6 ) .lt. 0.0 ) x( 6 ) = x( 6 ) + 360.0d0
 
        else
                x( 5 )         = 0.0d0
                x( 6 )         = 0.0d0
        end if
 
        end
