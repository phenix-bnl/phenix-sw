c     $Id: fs_put_dst.f,v 1.7 2009/02/24 17:34:17 hpereira Exp $

      subroutine fs_put_dst

      implicit none
 
c -------------------
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpslink.inc"
c --------------------
              
      integer lk,isub
      integer dcode /18/  ! used as switch in the dstrootout function in the rootPISA.cc file
      
      if(cvolu_opt(2,18).eq.'P_ID' .and. cvolu_opt(4,18).eq.'ELEM')then

C  at beginning of run write out parameters associated with RXN 

        if (cudst_otag_typ .eq. 'PARA') then
                
          call u_put_ds(ixdiv_fr,lfs_para,'PISA','RXNR','PARA',' ')
          call u_put_ds(ixdiv_fr,lfs_paru,'PISA','RXNR','PARU',' ')
                
        elseif (cudst_otag_typ .eq. 'EVNT') then

c event data written to DST

          do isub = 1,48   
            
            ! 48 sensitive volumes
            call u_put_ds(
     +        ixdiv_fe,lfs_RXN(isub),
     +        'PISA','RXNR','RXN1',' ')
              
            lk = lfs_RXN(isub)
            
            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and.   
     +        iqf(lk+1).gt.0)then

              call dstrootout(
     +          dcode, 0, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
                    
            endif !  check on root output           
          enddo ! loop over sensitive volumes

        endif ! check on check on para or evnt call        
      endif  ! check on elem output
 
      return
      end
