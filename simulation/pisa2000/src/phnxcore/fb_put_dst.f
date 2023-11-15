c     $Id: fb_put_dst.f,v 1.4 2009/02/24 17:34:14 hpereira Exp $
      subroutine fb_put_dst

C     to write out the PISA BBC detector output zebra banks to
C     a Zebra FZ file

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpblink.inc"

      integer lk
      integer dcode /3/    ! value for BBC (WBS order, after FKIN and PRI)

      if (cudst_otag_typ .eq. 'PARA') then

        ! at beginning of run write out parameters associated with BBC
        lk = lfb_para
        if(root_output.eq.1)then
          call parrootout(dcode, iqf(lk+2), qf(lk+2))
        endif
              
        call u_put_ds(ixdiv_fr,lfb_para, 'PISA','BBC ','PARA',' ')
              
      endif   ! check on call with a 'para' tag
            
      if (cudst_otag_typ .eq. 'EVNT') then

C       event data written to DST  (WHAT HAPPENS TO PISORP IF LFB_CAL = 0 ?)

        if (lfb_cal(1) .ne. 0)
     &    call u_put_ds(ixdiv_fe,lfb_cal(1),
     &    'PISA','BBC ','BCAL',' ')

        lk = lfb_cal(1)
        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and. 
     +    iqf(lk+1).gt.0) then
              
          call dstrootout(
     +      dcode, 0, iqf(lk+1), 
     +      iqf(lk+2), qf(lk+2))
                
        endif ! check on ROOT output
      endif   ! check on call with an 'evnt' tag
      return
      end
