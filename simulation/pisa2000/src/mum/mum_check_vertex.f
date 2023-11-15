*CMZ :  2.04/00 22/06/93  14.40.42  by  Surender Saini
*-- Author :
*-- Author :    Soren P. Sorensen 28/09/92
*CMZ :          18/04/93  12.36.58  by  Surender Saini
*-- Author :    Surender Saini   18/04/93
 
      subroutine mum_check_vertex

c    *************************************************************
c    *                                                           *
c    *  MUMCHECK (vsn 1.00)  check_vertex muon_arm routine       *
c    *                                                           *
c    *  Called by ==> ::  < mum_user >                           *
c    *  IN   :: none                                             *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/04/93 12.36.58        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************

 
c ------------------------
*KEEP,GCBANK.
#include "gcbank.inc"
*KEEP,GCNUM.
#include "gcnum.inc"
*KEND.
c ------------------------
 
        integer   it, nt
        integer   jk, iv, jv
        integer   lgkine, lgvert
 
        print *
        print *, ' check vertices '
 
        do iv = 2, nvertx
            print *
            print '( i6 )', iv
 
*            jv   = lq( jvertx - iv )      ! vertex pointer
            jv   = lgvert(jvertx,iv)
            jt   = nint( q( jv + 5 ) )    ! parent
            nt   = nint( q( jv + 7 ) )    ! no of tracks
*            jk   = lq( jkine - jt )       ! track pointer
            jk   = lgkine( jkine, jt )
 
            print '( i6, 5f12.4 )', jt, ( q( jk + i ), i = 1, 5 )
 
            do j = 1, nt
                jt = nint( q( jv + 7 + j ) )
*                jk = lq( jkine - jt )       ! track pointer
                jk = lgkine(jkine,jt)
                print '( i6, 5f12.4 )', jt,
     x                 ( q( jk + i ), i = 1, 5 )
            end do
 
        end do
 
      return
        end
