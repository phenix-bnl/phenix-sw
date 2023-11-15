c     $Id: fh_put_dst.f,v 1.7 2009/02/24 17:34:15 hpereira Exp $

      subroutine fh_put_dst

c     this is the hbd which is part of the 2001 upgrades studies

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fphlink.inc"

      integer lk
      integer dcode /15/
      integer isub  ! Sensitive volume index

      if(cvolu_opt(2,15).eq.'P_ID' .and. cvolu_opt(4,15).eq.'ELEM')then

C  at beginning of run write out parameters associated with HBD 

        if (cudst_otag_typ .eq. 'PARA') then
                
          call u_put_ds(ixdiv_fr,lfh_para,'PISA','HBDR','PARA',' ')
          call u_put_ds(ixdiv_fr,lfh_paru,'PISA','HBDR','PARU',' ')
                
        elseif (cudst_otag_typ .eq. 'EVNT') then

c event data written to DST
*CAA Loop over sensitive volumes with data stored in zebra bank lFH_HBD.
*CAA isub from 1-576 because 1-560 are 560 = 2 detectors * 8 sectors * 35 padrows,
*CAA 561-568 is HBD sensitive radiator gas, 569-576 is HBD CsI.
          do isub = 1,576
            call u_put_ds(
     +        ixdiv_fe,lFH_HBD(isub,1),
     +        'PISA','HBDR','HBD',' ')

            lk = lFH_HBD(isub,1)
                  
            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and. 
     +        iqf(lk+1).gt.0)then
                  
              call dstrootout(
     +          dcode, 0, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
                    
            endif  !  check on root output           
          enddo  ! loop over subvolumes that use the bank indexed by lFH_HBD1

        endif ! check on check on para or evnt call
      endif  ! check on elem output

      return
      end
