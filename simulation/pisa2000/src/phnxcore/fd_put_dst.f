c     $Id: fd_put_dst.f,v 1.10 2009/02/24 17:34:14 hpereira Exp $

      subroutine fd_put_dst
      implicit none
            
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpdlink.inc"

      integer lk
      integer dcode /14/

      integer iLayer

      integer siliLayers
      save siliLayers

      integer sili_sideLayers
      save sili_sideLayers

      if(cvolu_opt(2,3) .eq. 'P_ID' .and. cvolu_opt(4,3) .eq. 'ELEM')
     &  then

C  at beginning of run write out parameters associated with Si INR

        if (cudst_otag_typ .eq. 'PARA') then
          call u_put_ds(ixdiv_fr,lfd_para,'PISA','SINR','PARA',' ')

          lk = lfd_para + 1
          siliLayers = iqf(lk)
                
          lk = lfd_para + 2
          sili_sideLayers = iqf(lk)
                
          if(siliLayers.lt.0. or. siliLayers.gt.20)then
            write(6,1234)siliLayers
 1234       format(/, ' fd_put_dst <E> invalid siliLayers = ',
     &        i10,/)
            stop ' exiting PISA'
          endif
                
          if(sili_sideLayers.lt.0. or. sili_sideLayers.gt.20)then
            write(6,2234)sili_sideLayers
 2234       format(/, ' fd_put_dst <E> invalid sili_sideLayers = ',
     &        i10,/)
            stop ' exiting PISA'
          endif
          
          call u_put_ds(ixdiv_fr,lfd_paru,'PISA','SINR','PARU',' ')
                
          if(root_output.eq.1)then
            lk = lfd_para
            call parrootout(dcode, iqf(lk+1), qf(lk+6))
          endif
          
        elseif (cudst_otag_typ .eq. 'EVNT') then

          ! event data written to DST

          do iLayer = 1, siliLayers + sili_sideLayers
                  
            call u_put_ds(ixdiv_fe,lFD_SIL(iLayer,1),
     +        'PISA','SINR','SIL1',' ')
              
            lk = lfd_sil(ilayer,1)
                  
            if(
     +        root_output.eq.1 .and. 
     +        lk .gt. 0 .and.
     +        iqf(lk+1).gt.0  )then
                  
              call dstrootout(
     +          dcode, 0, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
                    
            endif  !  check on root output          
          enddo  ! loop over layers
        endif ! check on check on para or evnt call
      endif  ! check on elem output


      return
      end
