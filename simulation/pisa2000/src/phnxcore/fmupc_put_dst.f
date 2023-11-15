c     $Id: fmupc_put_dst.f,v 1.5 2009/02/24 17:34:15 hpereira Exp $

      subroutine fmupc_put_dst
      implicit none

c     MuPC Zebra data structure output

c     Original author: Wei Xie

#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fmupclink.inc"

      integer dcode /22/        ! WBS order, after KIN and PRI
      integer isector

C detector parameters:
      integer nmchmb           
      parameter (NMCHMB = 2)  
 
 

C  ...and names.. (these should be declared only in one place, not declared
C   locally in n places)

      INTEGER IC, LK
      CHARACTER*4 BNAM
      CHARACTER*4  uPC1Gas(2),  uPC2Gas(2), uPC3Gas(2)
      DATA uPC1Gas /'uG1S', 'uG1N'/
      DATA uPC2Gas /'uG2S', 'uG2N'/
      DATA uPC3Gas /'uG3S', 'uG3N'/

      ! at beginning of run write out parameters associated with PAD set
      do ic = 1,nmchmb
        bnam = upc1gas(ic)
        lk = lfmupc1(ic,1)
        call u_put_ds(ixdiv_fe,LK,'PISA','MUPC ',bnam,' ')

        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and.   
     +    iqf(lk+1).gt.0)then
              
        call dstrootout(
     +    dcode, IC, iqf(lk+1),
     +    iqf(lk+2), qf(lk+2))
              
        endif ! don't bother with empty sector
      enddo

      ! Pad 2 event data
      do ic = 1,nmchmb
        bnam = upc2gas(ic)
        lk = lfmupc2(ic,1)
        call u_put_ds(ixdiv_fe,LK,'PISA','MUPC ',BNAM,' ')
              
        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and.   
     +    iqf(lk+1).gt.0)then
              
        call dstrootout(
     +    dcode, IC+2, iqf(lk+1),
     +    iqf(lk+2), qf(lk+2))
        endif  ! check if sector has hits or last subevent
      end do
      
      ! Pad3 event data
      do ic = 1,nmchmb
        bnam = upc3gas(ic)
        lk = lfmupc3(ic,1)
              
        call u_put_ds(ixdiv_fe,LK,'PISA','MUPC ',BNAM,' ')
        if(
     +    root_output.eq.1 .and. 
     +    lk.gt.0 .and.   
     +    iqf(lk+1).gt.0)then
              
        call dstrootout(
     +    dcode, IC+4, iqf(lk+1),
     +    iqf(lk+2), qf(lk+2))
              
        endif  ! check if ROOT output requested
        
      enddo
      return
      end
