c     $Id: ff_put_dst.f,v 1.4 2009/02/24 17:34:15 hpereira Exp $

      subroutine ff_put_dst

c  to write out the PISA - TOF output to
c     ZEBRA banks, to a Zebra FZ file

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fpflink.inc"

      integer lk
      integer dcodep /6/
      integer dcode /8/



      if (cvolu_opt(2,7).eq.'P_ID')then

c     at beginning of run write out parameters associated with TOF

        if (cudst_otag_typ .eq. 'PARA')then
          call u_put_ds(ixdiv_fr,lff_para,'PISA','TOF ','PARA',' ')
          call u_put_ds(ixdiv_fr,lff_paru,'PISA','TOF ','PARU',' ')
          lk = lff_para
          if(root_output.eq.1)then
            call parrootout(dcodep, iqf(lk+2), qf(lk+2))
          endif

c     event data written to DST

        elseif (cudst_otag_typ .eq. 'EVNT')then
                
          if(cvolu_opt(4,7) .eq. 'FCAL')
     +      call U_PUT_DS(ixdiv_fe,lFF_CAL(1),'PISA','TOF ','FCAL',' ')
         
          lk=lff_cal(1)
          if(
     +      root_output.eq.1 .and. 
     +      lk.gt.0 .and. 
     +      iqf(lk+1).gt.0 ) then
          
            call dstrootout(
     +        dcode, 0, iqf(lk+1),
     +        iqf(lk+2), qf(lk+2))
                
          endif  !  check on root output
        endif  !  check on FCAL option
      endif  !  check on EVNT or PARA call
            
      return
      end
