        subroutine  dimuon( mass, sqrt_s, p1, p2 )

      implicit none

#include "g77trigdef.inc"

#include "gugeom.inc"
        real        mass, sqrt_s, p1( 0:6 ), p2( 0:6 ), rndm
 
        real*8      dmass, dsqrt_s, dp1( 0:6 ), dp2( 0:6 )
        real*8      B, C, pt, xf, y, mt, pm( 0:3 ), beta( 3 ), theta
        real*8      m_muon / 0.105658d0 /
 
        integer     i
 
 
        do i = 0, 3
                p1( i ) = 0.0
                p2( i ) = 0.0
        end do
 
 
        if ( mass .lt. 2.0 * m_muon ) return
 
        dmass       = dble( mass )
        dsqrt_s     = dble( sqrt_s )
 
        dp1( 0 )    = dmass / 2.0d0
        dp1( 4 )    = sqrt( dp1( 0 )**2 - m_muon**2 )
        dp1( 5 )    = acosd( 2.0d0 * dble( rndm( 0 ) ) - 1.0d0 )
        dp1( 6 )    = 360.0d0 * dble( rndm( 0 ) )
 
        call polar_to_rect( dp1( 1 ) )
 
        dp2( 0 )    =  dp1( 0 )
        dp2( 1 )    = -dp1( 1 )
        dp2( 2 )    = -dp1( 2 )
        dp2( 3 )    = -dp1( 3 )
        dp2( 4 )    =  dp1( 4 )
        dp2( 5 )    =  180.0d0 - dp1( 5 )
        dp2( 6 )    =  360.0d0 - dp1( 6 )
 
 
        B  = max( 0.8d0, -1.025d0 * log( dmass ) + 3.50d0 )
        C  = B + 0.20d0
 
        pt = -log( 1.0d0 - dble( rndm( 0 ) ) ) / B
        mt = sqrt( dmass**2 + pt**2 )
        theta   = 360.0d0 * dble( rndm( 0 ) )
        pm( 1 ) = pt * cosd( theta )
        pm( 2 ) = pt * sind( theta )
 
        xf   = 1.0d0 -
     &  ( 1.0d0 - dble( rndm( 0 ) ) )**( 1.0d0 / ( C + 1.0d0 ) )
        if ( rndm( 0 ) .lt. 0.5 ) xf = - xf
 
        pm( 3 ) = xf * dsqrt_s * 0.5d0
        pm( 0 ) = sqrt( dmass**2 + pm( 1 )**2
     &          + pm( 2 )**2 + pm( 3 )**2 )
 
        beta( 1 )        = -pm( 1 ) / pm( 0 )
        beta( 2 )        = -pm( 2 ) / pm( 0 )
        beta( 3 )        = -pm( 3 ) / pm( 0 )
 
        call        lt4( beta, dp1( 0 ), dp1( 0 ) )
        call        lt4( beta, dp2( 0 ), dp2( 0 ) )
 
        call        rect_to_polar( dp1( 1 ) )
        call        rect_to_polar( dp2( 1 ) )
 
        do i = 0, 6
                p1( i )        = real( dp1( i ) )
                p2( i )        = real( dp2( i ) )
        end do
 
        end
