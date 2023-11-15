c     $Id: fv_put_dst.f,v 1.4 2009/02/24 17:34:17 hpereira Exp $

      subroutine fv_put_dst

c  to write out the pisa vertex detector output zebra banks to
c  a zebra fz file
c  srtonse 28-jul-1992
c  update by jhk 9/28/92 for the vertex detector

      implicit none

#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "fpvlink.inc"
#include "subevt.inc"

      integer dcode /4/   ! WBS order, after FKIN and PRI
      integer lk

      if (cudst_otag_typ .eq. 'PARA') then

C       at beginning of run write out parameters associated with VER

        call u_put_ds(ixdiv_fr,lfv_para,'PISA','VER ','PARA',' ')

        if(root_output.eq.1) then
                
          lk = lfv_para
          call parrootout(dcode, iqf(lk+2), qf(lk+2))

        endif


        ! No user parameter:

      endif ! check on call with a 'para' tag
            
      if (cudst_otag_typ .eq. 'EVNT') then

C     event data written to DST  (WHAT HAPPENS TO PISORP IF LFV_CAL = 0 ?)

        lk = lfv_cal(1)
        call u_put_ds(ixdiv_fe,lk, 'PISA','VER ','VCAL',' ')

        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and.   
     +    iqf(lk+1).gt.0)then
              
          call dstrootout(
     +      dcode, 0, iqf(lk+1),
     +      iqf(lk+2), qf(lk+2))
          
        endif  ! check on ROOT output
        
      endif ! check on call with an 'evnt' tag
      return
      end
