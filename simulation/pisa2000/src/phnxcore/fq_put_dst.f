c     $Id: fq_put_dst.f,v 1.5 2009/02/24 17:34:16 hpereira Exp $

      subroutine fq_put_dst

c     this is the ntc which is new for run2 (2001)

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpqlink.inc"

      integer lk
      integer dcode /16/
      integer isub  ! Sensitive volume index
      
      if(cvolu_opt(2,16).eq.'P_ID' .and. cvolu_opt(4,16).eq.'ELEM')then

C  at beginning of run write out parameters associated with NTC 

        if (cudst_otag_typ .eq. 'PARA') then
                
          call u_put_ds(ixdiv_fr,lfq_para,'PISA','NTCR','PARA',' ')
          call u_put_ds(ixdiv_fr,lfq_paru,'PISA','NTCR','PARU',' ')
                
        elseif (cudst_otag_typ .eq. 'EVNT') then
          
c event data written to DST
          
          do isub = 1,4         ! 4 sensitive volumes: NTNE, NTNW, NTSE, NTSW
                  
            call u_put_ds(
     +        ixdiv_fe,lFQ_NTC(isub,1),
     +        'PISA','NTCR','NTC1',' ')

            lk = lfq_ntc(isub,1)
            
            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and.   
     +        iqf(lk+1).gt.0)then
            
              call dstrootout(dcode, 0, iqf(lk+1), iqf(lk+2), qf(lk+2))
                  
            endif  !  check on root output           
          enddo  ! loop over sensitive volumes
        endif ! check on check on para or evnt call
      endif  ! check on elem output

      return
      end
