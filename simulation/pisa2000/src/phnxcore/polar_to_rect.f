        subroutine        polar_to_rect( x )

      implicit none

#include "g77trigdef.inc"

#include "gugeom.inc"
        real*8        x( 6 )
 
        x( 1 )  = x( 4 ) * sind( x( 5 ) ) * cosd( x( 6 ) )
        x( 2 )  = x( 4 ) * sind( x( 5 ) ) * sind( x( 6 ) )
        x( 3 )  = x( 4 ) * cosd( x( 5 ) )
 
        end
