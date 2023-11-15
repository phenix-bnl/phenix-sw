c     $Id: mcdim.f,v 1.3 2008/05/21 08:22:10 hpereira Exp $
c     Author :    Charles F. Maguire   08/10/92
      subroutine mcdim
      implicit none
 
c     authors: sps
c     date: 5/29 1992
c     generates unlike sign dimuons
#include "gcflag.inc"
#include "guphnx.inc"
#include "guevgen.inc"
            
      integer  i, j, id, itr, nwb, nubuf, nvertex, ntdummy, m
      integer  id1, id2
       
      real     mass, sqrt_s, p1( 0:6 ), p2( 0:6 ), ecut, tmin, tmax
      real     y_min, y_max, pt_min, pt_max, phi_min, phi_max
      real     y, pt, phi, charge, tlife, ub( 10 )
      real     rndm, gamma, beta, pl, etot, mt, u
       
      character*20          cpart
            
      integer icall /0/
            
      chevt_name = 'MCDIM from Soren Sorensen'
      numevt = numevt + 1
      
      ! initialize program
      if ( icall .eq. 0 ) then  
        icall = 1
              
c       Input parameters
        ! no of particles per event
        mxtot   = nint( pmc2( 1 ) ) 
        sqrt_s  = pmc2( 2 )
        ecut    = pmc2( 3 )
        tmin    = pmc2( 4 )
        tmax    = pmc2( 5 )
        mxtot   = min( mxtot, max_mxtot )
      end if
       
      m = 0
       
      do i = 1, mxtot
              
        if ( mod( i, 2 ) .eq. 1 ) then
           
          u        = rndm( 0 )
                
          if ( u .lt. 1.0/3.0 ) then
            mass         = 1.020
          else if ( u .lt. 2.0/3.0 ) then
            mass        = 3.097
          else
            mass        = 9.460
          end if
                
10        m = m + 1
          call dimuon( mass, sqrt_s, p1, p2 )
                
          if (p1( 4 ) .gt. ecut .and. p1( 5 ) .gt. tmin .and.
     x      p1( 5 ) .lt. tmax .and.
     x      p2( 4 ) .gt. ecut .and. p2( 5 ) .gt. tmin .and.
     x      p2( 5 ) .lt. tmax ) then
               
          else
             
            if ( m .gt. 10000 ) then
              print *,
     x          ' too many trials for muon acceptance '
              return
            end if
             
            go to 10
             
          end if
           
          m = 0
           
c         particle id
           
          if ( rndm( 0 ) .lt. 0.5 ) then
            id1 = 5
            id2 = 6
          else
            id1 = 6
            id2 = 5
          end if
           
c         vertex        
          xyz( 1 ) = 0.0
          xyz( 2 ) = 0.0
          xyz( 3 ) = 0.0
           
c         store vertex info
          call gsvert( xyz, 0, 0, ub, nubuf, nvertex )
           
c         now store track info
          nubuf    = 2
          ub( 1 )  = real( ( i + 1 ) / 2 )
          ub( 2 )  = mass
 
          call gskine( p1( 1 ), id1, nvertex,
     1      ub, nubuf, ntdummy )
 
        else
                
          call gsvert( xyz, 0, 0, ub, nubuf, nvertex )
                
          nubuf    = 2
          ub( 1 )  = real( ( i + 1 ) / 2 )
          ub( 2 )  = mass
                
          call gskine( p2( 1 ), id2, nvertex,
     1      ub, nubuf, ntdummy )
                
        end if
      end do
           
      maxtrk = ntdummy
      if ( idebug .ne. 0 ) call gpkine( 0 )
            
      end
            
