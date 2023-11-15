c     $Id: fx_put_dst.f,v 1.4 2009/02/24 17:34:18 hpereira Exp $

      subroutine fx_put_dst

c  to write out the primary kinematics bank

      implicit none
#include "udst.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpxlink.inc"

#include "guphnx.inc"


c     local variables

      integer lk
      character*4 bnam

      integer dcode /2/    ! value for PRI (order as in e_put_dst routine)
 

c event data written to DST

 
      bnam = 'FPRI'
      lk = lfx_kine(1)

      if(zebra_output.eq.1)then
        call u_put_ds(ixdiv_fe,lk,'PISA','PRI ',bnam,' ')
      endif

       if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and.   
     +    iqf(lk+1).gt.0)then
              
          call dstrootout(
     +      dcode, 0, iqf(lk+1), 
     +      iqf(lk+2), qf(lk+2))
      endif

      return
      end
