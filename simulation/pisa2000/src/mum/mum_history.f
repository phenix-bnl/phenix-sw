*CMZ :  2.04/00 22/06/93  14.37.10  by  Surender Saini
*-- Author :
*-- Author :    Soren P. Sorensen 28/09/92
*CMZ :          18/04/93  12.49.23  by  Surender Saini
*-- Author :    Surender Saini   18/04/93
 
      subroutine mum_history(it, nt, th )

c    *************************************************************
c    *                                                           *
c    *  MUMHIST (vsn 1.00)  track the history of a hit           *
c    *                                                           *
c    *  Called by ==> :: < mum_user >                            *
c    *  IN   :: it                                               *
c    *  OUT  :: nt, th                                           *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/04/93 12.49.23        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************

        implicit  none
 
c ---------------
*KEEP,GCBANK.
#include "gcbank.inc"
*KEEP,GCNUM.
#include "gcnum.inc"
*KEND.
c ---------------
 
        integer   it, nt, th( * )
        integer   jk, iv, jv
        integer   lgkine, lgvert

        nt  = 0                        ! # of parent tracks
*        jk  = lq( jkine - it )         ! track pointer
        jk  = lgkine( jkine, it )
        iv  = nint( q( jk + 6 ) )      ! vertex of origin
 
        do while ( iv .ge. 2 )
*            jv       = lq( jvertx - iv )      ! vertex pointer
            jv       = lgvert(jvertx,iv)
            nt       = nt + 1                 ! history counter
            th( nt ) = nint( q( jv + 5 ) )    ! parent track #
*            jk       = lq( jkine - th( nt ) ) ! track pointer
            jk       = lgkine( jkine, th( nt ) )
            iv       = nint( q( jk + 6 ) )    ! vertex of origin
        end do
 
        end
