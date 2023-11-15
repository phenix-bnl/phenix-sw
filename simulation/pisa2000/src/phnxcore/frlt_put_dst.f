c     $Id: frlt_put_dst.f,v 1.4 2009/02/24 17:34:16 hpereira Exp $

      subroutine frlt_put_dst
      implicit none
      
#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "frltlink.inc"
      
      integer dcode /23/        ! WBS order, after KIN and PRI
      integer isector
      
      ! detector parameters:
      integer nmchmb           
      parameter (nmchmb = 1)  

      ! ...and names.. (these should be declared only in one place, not declared
      ! locally in n places)
      integer lk
      character*4 bnam

      character*4 rltRPC1_gas/'rtg1'/
      character*4 rltRPC2_gas/'rtg2'/
      character*4 rltRPC3_gas/'rtg3'/

      bnam = rltrpc1_gas
      lk = lfrltrpc1(1,1)
            
      call u_put_ds(ixdiv_fe,LK,'PISA','RLT ',bnam,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lk.gt.0 .and.   
     +  iqf(lk+1).gt.0)then
            
        call dstrootout(
     +    dcode, 1, iqf(lk+1),
     +    iqf(lk+2), qf(lk+2))
       
      endif  
      
      
      ! RPC 2 event data
      bnam = rltrpc2_gas
      lk = lfrltrpc2(1,1)
            
      call u_put_ds(ixdiv_fe,LK,'PISA','RLT ',BNAM,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lk.gt.0 .and.   
     +  iqf(lk+1).gt.0)then
      
        call dstrootout(
     +    dcode, 2, iqf(lk+1),
     +    iqf(lk+2), qf(lk+2))

      endif              
      
C...  RPC 3 event data
      

      BNAM = rltRPC3_gas
      LK = LFrltRPC3(1,1)
            
      call u_put_ds(ixdiv_fe,LK,'PISA','RLT ',BNAM,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lk.gt.0 .and.   
     +  iqf(lk+1).gt.0)then
  
        call dstrootout(
     +    dcode, 3, iqf(lk+1),
     +    iqf(lk+2), qf(lk+2))
  
      endif                     ! check if ROOT output requested
      
      return
      end
