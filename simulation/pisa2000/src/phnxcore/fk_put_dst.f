c     $Id: fk_put_dst.f,v 1.4 2009/02/24 17:34:15 hpereira Exp $

      subroutine fk_put_dst
c     to write out the resticted kinematics bank
    
      implicit none
            
#include "udst.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpklink.inc"
#include "subevt.inc"
#include "guphnx.inc"
 

c     local variables

      integer lk
      character*4 bnam

      integer dcode /1/   ! value for FKIN (order of appearence in e_put_dst)
 

c event data written to DST

 
      bnam = 'FKIN'
      lk = lfk_kine(1)

      if(zebra_output.eq.1)then
         call u_put_ds(ixdiv_fe,LK,'PISA','KIN ',BNAM,' ')
      endif

      if(
     +  root_output.eq.1 .and.
     +  lk.gt.0 .and.
     +  iqf(lk+1).gt.0)then
        call dstrootout(dcode, 0, iqf(lk+1), iqf(lk+2), qf(lk+2))
      endif ! check on ROOT output

      return
      end
