c     $Id: fy_put_dst.f,v 1.8 2009/02/24 17:34:18 hpereira Exp $

      subroutine fy_put_dst
            
c  to write out the fcl forward calorimeter output zebra banks to
c  a zebra fz file

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpylink.inc"

      integer lk
      integer dcode /19/

      integer ilayer
      integer fcllayers

      save fcllayers

c      write(*,*) 'in fy_put_dst'


      if(cvolu_opt(2,19) .eq. 'P_ID' .and. cvolu_opt(4,19) .eq. 'ELEM')
     &  then

C  at beginning of run write out parameters associated with Si INR

        if (cudst_otag_typ .eq. 'PARA') then
              
          call u_put_ds(ixdiv_fr,lfy_para,'PISA','SFCL','PARA',' ')
        
          lk = lfy_para + 1
          fclLayers = iqf(lk)
          call u_put_ds(ixdiv_fr,lfy_paru,'PISA','SFCL','PARU',' ')
          
        elseif (cudst_otag_typ .eq. 'EVNT') then

c event data written to DST
c      version of fcl.f not conforming to write a "PARA" data structure first
c      so there is no   FCLLAYERS  value
c      for now set FCCLLAYERS = 1

          fclLayers = 90
          do iLayer = 1,fclLayers
            
            lk = lFY_FCL(iLayer,1)
            call u_put_ds(
     +        ixdiv_fe,lk,
     +        'PISA','SFCL','FCL1',' ')
                  
            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and.   
     +        iqf(lk+1).gt.0)then
                   
              call dstrootout(
     +          dcode, 0, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
                    
            endif  !  check on root output    
                  
          enddo  ! loop over layers
                
        endif ! check on check on para or evnt call
      endif  ! check on elem output

      return
      end
