      subroutine gutrak
      implicit none

C*********  for each track in the event   ***************

#include "gckine.inc"
#include "gcflag.inc"

      integer icall1/0/    ! for beam gas events
      integer icall2/0/    ! neutron tracking call
      integer icall4/0/    ! neutrino tracking call

      ! begin execution
      if(iswit(4).ge.1)then

        ! beam gas event control 
        ! (only muons, charged pions, kaons, neutrons, or protons are passed)
        if(icall1.eq.0)then
          
          icall1 = 1
          write(6,*) 'gutrak - Beam gas event control'
          write(6,*) 
     +      'gutrak - Only muons, charged pions, kaons, and nucleons ',
     +      'are tracked'
            
        endif  ! initialization check

        ! reject backward going particles if ISWIT(4) = 2
        if(iswit(4).eq.2.and.pvert(3).gt.0.0)then
          return
        endif  
        
        if(ipart.lt.5.or.ipart.eq.7.or.ipart.gt.14)then
          return
        endif !  check on allowed particles 
      endif  ! check on beam gas event control


      ! set up to check neutrons and neutrinos only (for now)
      if(ipart.ne.4.and.ipart.ne.13)then

        ! quick call to gtrack
        call gtrack
        return
        
      endif  ! check on not being a neutrino or a neutron

c     have either a neutrino or a neutron here
c     check the ISWIT array

      ! select certain particles for tracking
      if(iswit(2).ne.0) then     
        
        ! don't track neutrons
        if(iswit(2).eq.1) then     
          if(icall2.eq.0)then
            icall2=icall2+1
            write(6,*) 'gutrak - Neutrons are not tracked'
          endif        ! check on ICALL2 = 0 with ISWT2 = 1
          if(ipart.eq.13) return
        endif  ! check on ISWT2 = 1
      endif  ! check on ISWT2 <> 0
      
      if(iswit(2).eq.0)then
        if(icall2.eq.0)then
          icall2=icall2+1
          write(6,*) 'gutrak - Neutrons are tracked'
        endif        ! check on ICALL2 = 0 with ISWT2 = 0
      endif  ! check on ISWT2 = 0
      
      ! select certain particles for tracking
      if(iswit(3).ne.0) then      
      
        ! don't track neutrinos
        if(iswit(3).eq.1) then     
          if(icall4.eq.0)then
            icall4=icall4+1
            write(6,*) 'gutrak - Neutrinos are not tracked'
          endif  ! check on ICALL4 = 0
          if(ipart.eq.4) return
        endif  ! check on ISWT3 = 1
      endif  ! check on ISWT3 <> 0
      
      if(iswit(3).eq.0)then
        if(icall4.eq.0)then
          icall4=icall4+1
          write(6,*) 'gutrak - Neutrinos are tracked'
        endif  ! check on ICALL4 = 0
      endif  ! check on ISWT3 = 0
      
      call gtrack
999   return
      end
            
