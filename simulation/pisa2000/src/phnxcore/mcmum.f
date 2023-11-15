      subroutine mcmum
      implicit none

#include "g77trigdef.inc"

c     authors: sps
c     date:    5/29 1992
c     generates muons
 
#include "gugeom.inc"
#include "gcflag.inc"
#include "guphnx.inc"
#include "guevgen.inc"
 
      integer  i, j, id, itr, nwb, nubuf, nvertex, ntdummy
      integer  np, nt, ni, idd, ii, it, ip
 
      real     p_min, p_max, th_min, th_max, phi_min, phi_max
      real     y, pt, phi, mass, charge, tlife, ub( 10 )
      real     rndm, gamma, beta, pl, etot, p1( 3 ), mt
      real     dpp, dtt, h, p, th, pp_min, pp_max, u
 
      character*20          cpart
            
      integer icall /0/
            
      chevt_name = 'MCMUM from Soren Sorensen'
      numevt = numevt + 1
      
      ! initialize program
      if ( icall .eq. 0 ) then 
        icall = 1
              
c       Input parameters
        mxtot   = nint( pmc2( 1 ) )    ! no of particles per event
        idd     = nint( pmc2( 2 ) )    ! 0: mu+ and mu-, 1: mu+ -1:mu-
        p_min   =       pmc2( 3 )        ! minimum momentum
        p_max   =       pmc2( 4 )        ! maximum momentum
        th_min  =       pmc2( 5 )        ! minimum theta
        th_max  =       pmc2( 6 )        ! maximum theta
        phi_min =       pmc2( 7 )        ! minimum phi
        phi_max =       pmc2( 8 )        ! maximum phi
              
        mxtot   = min( mxtot, max_mxtot )
              
        pp_min  = 1.0 / p_max
        pp_max  = 1.0 / p_min
              
      end if
       
      do i = 1, mxtot
        p   = 1.0 / ( pp_min + ( pp_max - pp_min ) * rndm( 0 ) )
        phi = phi_min + ( phi_max - phi_min ) * rndm( 0 )
        th  = th_min + (th_max-th_min) * (1.0-rndm(0)**(2.0/3.0))
        pt  = p * sind( th )
         
        if ( idd .eq. 0 ) then
          if ( rndm( 0 ) .gt. 0.5 ) then
            id = 5
          else
            id = 6
          end if
        else if ( idd .gt. 0 ) then
          id  = 5
        else
          id  = 6
        end if
              
        call  gfpart( id, cpart, itr, mass, charge,
     1    tlife, ub, nwb )
           
c       particle id
        idtot( i )  = id
         
c       vertex
        xyz( 1) = 0.0
        xyz( 2) = 0.0
        xyz( 3) = 0.0 
        
c       store vertex info         
        nubuf = 0
        call gsvert( xyz( 1), 0, 0, ub, nubuf, nvertex )
         
c       kinematic variables
        phi = 360.0 * rndm( 0 )
 
c       store track info
        p1( 1 ) = pt * cosd( phi )
        p1( 2 ) = pt * sind( phi )
        p1( 3 ) = p  * cosd( th )
        ub( 1 ) = p
         
        call gskine( p1, id, nvertex, ub, 1, ntdummy )
           
      end do
         
      ! maximum primary track number
      maxtrk = ntdummy 
      if ( idebug .ne. 0 ) call gpkine( 0 )
      end
            
