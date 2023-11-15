*CMZU:  2.04/00 04/08/93  16.34.39  by  S R Tonse
*-- Author :    Surender Saini   18/06/93
 
      subroutine mu_spgen

c    *************************************************************
c    *                                                           *
c    *  MU_SPGEN (vsn 1.00) Generates particles of given PID and *
c    *                      momentum in a given theta range.     *
c    *  Called by ==> ::  GUEVGEN                                *
c    *  IN   ::                                                  *
c    *  OUT  ::                                                  *
c    *                                                           *
c    *  written  by ::  Surender Saini, 18/06/93 13.47.36        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************

c Special generator for muon arm simulations. Generates particles of
c a given PID and momentum , in a given THETA range and produced at
c a given Z with x_vertex and y_vertex calculated from random theta
c value. The generator also has a delta_momentum for each successive
c event.

 
       implicit none
 
c --------------
*KEEP,CMCEVT.
#include "cmcevt.inc"
*KEEP,GUEVGEN.
#include "guevgen.inc"
*KEEP,GCFLAG.
#include "gcflag.inc"
*KEND.
c --------------
 
      real*4 pai, cdtr, crtd
      common/uconst/pai,cdtr,crtd

      real rndm(2),rvrtex(3),ppp(3)
      real theta,theta_f,theta_l,dth,zvrtx,pmom,delmom
      real sthcph,sthsph,ctheta,thetr,phi
      real rperp,rvrt
 
      integer  mfirst, ntracc, jtr, nprflg,ntrig
      integer  ipid,mc_event_tot,mc_track_evt,mc_track_trig
      integer  nvtx, ntdum
 
      data mfirst/1/
 
c ------------------------------------------------------------------
 
 
      if(mfirst .eq. 1)then
c Inputs to the generator
 
       ipid         = pmc1(1)            ! Geant Particle_id
       mc_event_tot = nint(pmc2(1))      ! # of events
       mc_track_evt = nint(pmc2(2))      ! # of particles / event
       mc_track_trig= nint(pmc2(3))      ! # of particles / trig
       theta_f      = nint(pmc2(9))      ! Theta first ( or min )
       theta_l      = nint(pmc2(10))     ! Theta last  ( or max )
       pmom         = pmc2(11)           ! Incident momentum
       zvrtx        = pmc2(12)           ! vertex_Z
       nprflg       = pmc2(13 )          ! print_flag
       delmom       = pmc2(14 )          ! additive delta momentum
 
 
       dth = theta_l - theta_f
       mc_event_no = 1
       jtr = 0
       ntracc = 0
       mfirst = 0
 
      end if
 
      ntrig = 0
 
      do while (ntrig .lt. mc_track_trig)
 
 
       jtr = jtr + 1
 
       if( jtr .gt. mc_track_evt)then
        mc_event_no = mc_event_no + 1
        jtr = 1
        ntracc = 0
      pmom = pmom + delmom
       end if
 
       if ( mc_event_no .gt. mc_event_tot ) then
        maxtrk = 0
        ieorun = 1
        ieotri = 1
        return
       end if
 
       call grndm(rndm,2)
 
       theta  = theta_f + dth * rndm(1)
       thetr  = theta*cdtr
       phi    = 2.*pai*rndm(2)
       sthcph = sin(thetr)*cos(phi)
       sthsph = sin(thetr)*sin(phi)
       ctheta = cos(thetr)
 
       ppp(1) = pmom * sthcph
       ppp(2) = pmom * sthsph
       ppp(3) = pmom * ctheta
 

       rperp  = zvrtx * tan(thetr)
       rvrt   = sqrt( rperp**2 + zvrtx**2 )
 
       rvrtex(1) = rvrt * sthcph
       rvrtex(2) = rvrt * sthsph
       rvrtex(3) = rvrt * ctheta
 
 
        if( ntrig .lt. mc_track_trig)then
 
         ntrig = ntrig + 1
         call gsvert( rvrtex, 0, 0, 0., 0, nvtx)
         call gskine( ppp, ipid, nvtx, 0., 0, ntdum)
         ntracc = ntracc + 1
        end if
 
        mc_track_no = jtr
 
       if(mod(jtr,nprflg) .eq. 0 .or. jtr .eq. mc_track_evt)then
        write(6,'(2x,''Tracks stored = '',4i6)')jtr,mc_event_no,ipid
       end if
 
      end do
 
         maxtrk = ntrig
 
      return
      end
