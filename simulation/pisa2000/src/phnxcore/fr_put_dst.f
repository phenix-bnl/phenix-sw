c     $Id: fr_put_dst.f,v 1.5 2009/02/24 17:34:16 hpereira Exp $

      subroutine fr_put_dst

c     this is the tpc which is part of the 2001 upgrades studies  (was previously named hbd)

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fprlink.inc"

      integer lk
      integer dcode /17/
      integer isub  ! sensitive volume index

      if(cvolu_opt(2,17).eq.'P_ID' .and. cvolu_opt(4,17).eq.'ELEM')then
        
        ! at beginning of run write out parameters associated with HBD 
        if (cudst_otag_typ .eq. 'PARA') then
                
          call u_put_ds(ixdiv_fr,lfr_para,'PISA','HBDR','PARA',' ')
          call u_put_ds(ixdiv_fr,lfr_paru,'PISA','HBDR','PARU',' ')
                
        elseif (cudst_otag_typ .eq. 'EVNT') then

c event data written to DST
*CAA Loop over sensitive volumes with data stored in zebra bank lFR_HBD.
*CAA isub from 1-576 because 1-560 are 560 = 2 detectors * 8 sectors * 35 padrows,
*CAA 561-568 is HBD sensitive radiator gas, 569-576 is HBD CsI.

          do isub = 1,576
                  
            lk = lfr_hbd(isub,1)
            call u_put_ds(
     +        ixdiv_fe, lk, 
     +        'PISA','HBDR','HBD',' ')


            if(
     +        root_output.eq.1 .and. 
     +        lk.gt.0 .and.   
     +        iqf(lk+1).gt.0)then
              
              call dstrootout(
     +          dcode, 0, iqf(lk+1),
     +          iqf(lk+2), qf(lk+2))
                    
            endif  !  check on root output           
          enddo  ! loop over subvolumes that use the bank indexed by lFR_HBD1

        endif ! check on check on para or evnt call
      endif  ! check on elem output

      return
      end
