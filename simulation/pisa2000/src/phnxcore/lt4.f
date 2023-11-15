*CMZ :  2.04/00 01/06/93  15.31.43  by  Charles F. Maguire
*CMZ :  2.01/00 08/10/92  10.27.39  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   08/10/92
        SUBROUTINE LT4( BETA, P_IN, P_OUT )
 
C        3-DIMENSIONAL LORENTZ TRANSFORMATION
 
        IMPLICIT        REAL*8        ( A-H, O-Z )
 
        REAL*8  BETA( 3 ), P_IN( 0:3 ), P_OUT( 0:3 )
 
 
        BETA2   = BETA( 1 ) * BETA( 1 )
     X          + BETA( 2 ) * BETA( 2 )
     X          + BETA( 3 ) * BETA( 3 )
 
        IF ( BETA2 .LE. 0.0D0 ) THEN
 
                P_OUT( 0 ) = P_IN( 0 )
                P_OUT( 1 ) = P_IN( 1 )
                P_OUT( 2 ) = P_IN( 2 )
                P_OUT( 3 ) = P_IN( 3 )
 
                RETURN
        END IF
 
        GAMMA        = 1.0D0 / SQRT( 1.0D0 - BETA2 )
        FACT1        = ( GAMMA - 1.0D0 ) / BETA2
 
        SCAP        = P_IN( 1 ) * BETA( 1 )
     X                + P_IN( 2 ) * BETA( 2 )
     X                + P_IN( 3 ) * BETA( 3 )
 
        FP        = FACT1 * SCAP - GAMMA * P_IN( 0 )
 
        P_OUT( 1 )        = P_IN( 1 ) + FP * BETA( 1 )
        P_OUT( 2 )        = P_IN( 2 ) + FP * BETA( 2 )
        P_OUT( 3 )        = P_IN( 3 ) + FP * BETA( 3 )
        P_OUT( 0 )        = GAMMA * ( P_IN( 0 ) - SCAP )
 
        END
