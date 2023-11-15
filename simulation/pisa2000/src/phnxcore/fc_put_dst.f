c     $Id: fc_put_dst.f,v 1.4 2009/02/24 17:34:14 hpereira Exp $
      subroutine fc_put_dst
      implicit none
            
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpclink.inc"

      integer dcode /6/
      integer lk

      if(cvolu_opt(2,5) .eq. 'P_PZ') then
              
        ! at beginning of run write out parameters associated with RICH
        if (cudst_otag_typ .eq. 'PARA') then
                
          call u_put_ds(ixdiv_fr,lfc_para,'PISA','CRK ','PARA',' ')
          call u_put_ds(ixdiv_fr,lfc_paru,'PISA','CRK ','PARU',' ')
                
        elseif (cudst_otag_typ .eq. 'EVNT') then
                
          ! event data written to DST
          call u_put_ds(ixdiv_fe,lFC_Cal(1),'PISA','CRK ','CCAL',' ')
          call u_put_ds(ixdiv_fe,lFC_Tra(1),'PISA','CRK ','CTRA','L')

          lk = lfc_cal(1)
          if(
     +      root_output.eq.1 .and. 
     +      lk.gt.0 .and. 
     +      iqf(lk+1).gt.0) then
                
            call dstrootout(
     +        dcode, iqf(1), iqf(lk+1),
     +        iqf(lk+2), qf(lk+2))
                  
          endif  ! check if ROOT output
        endif
      endif

      return
      end
