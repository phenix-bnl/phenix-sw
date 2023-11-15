c     $Id: fncc_put_dst.f,v 1.9 2018/07/05 16:50:12 lajoie Exp $
      subroutine fncc_put_dst
      implicit none

c     mupc zebra data structure output

c     original author: vasily dzhordzhadze
c     modified by ondrej chvala

#include "udst.inc"
#include "fstore.inc"
#include "guphnx.inc"
#include "sublink.inc"
#include "subevt.inc"
#include "fncclink.inc"

      integer dcode /24/        ! WBS order, after KIN and PRI
      integer dcode_abs /27/
      integer dcode_abs_fplt /28/
      integer dcode_ent /29/


C  ...and names.. (these should be declared only in one place, not declared
C   locally in n places)

      integer ic, lk, lka1, lka2, lka3, lka4, lka5
      integer lka6, lka7, lka8, lka9, lka10
      character*4 bnsec, bnsec1, bnsec2
      character*4 bnsec3, bnsec4, bnsec5
      character*4 bnsec6, bnsec7, bnsec8, bnsec9
      character*4 bnsec10
      data bnsec /'STSI'/
      data bnsec1 /'W_DS'/
      data bnsec2 /'SCDS'/
      data bnsec3 /'SPSS'/
      data bnsec4 /'INTS'/
      data bnsec5 /'COVS'/
      data bnsec6 /'FPLT'/
      data bnsec7 /'BPLT'/
      data bnsec8 /'SKIN'/
      data bnsec9 /'PLAT'/
      data bnsec10 /'W_DS'/

      ! STRIPS
      ic = 1
      lk = lfncc1(1,1)
      call u_put_ds(ixdiv_fe,LK,'PISA','NCC ',BNSEC,' ')
      
      if(
     +  root_output.eq.1 .and. 
     +  lk.gt.0 .and.   
     +  iqf(lk+1).gt.0)then

      call dstrootout(
     +  dcode, IC, iqf(lk+1),
     +  iqf(lk+2), qf(lk+2))

      endif  

c      !  ABSORBER HITS
      lka1 = lfncc2(1,1)
      call u_put_ds(ixdiv_fe,LKA1,'PISA','EXAB',BNSEC1,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka1.gt.0 .and.   
     +  iqf(lka1+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs, IC, iqf(lka1+1),
     +  iqf(lka1+2), qf(lka1+2))
            
      endif  ! check if sector has hits or last subevent


      lka2 = lfncc2(2,1)
      call u_put_ds(ixdiv_fe,LKA2,'PISA','EXAB',BNSEC2,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka2.gt.0 .and.   
     +  iqf(lka2+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs, IC, iqf(lka2+1),
     +  iqf(lka2+2), qf(lka2+2))
            
      endif  ! check if sector has hits or last subevent
            

      lka3 = lfncc2(3,1)
      call u_put_ds(ixdiv_fe,LKA3,'PISA','EXAB',BNSEC3,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka3.gt.0 .and.   
     +  iqf(lka3+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs, IC, iqf(lka3+1),
     +  iqf(lka3+2), qf(lka3+2))
            
      endif  ! check if sector has hits or last subevent


      lka4 = lfncc2(4,1)
      call u_put_ds(ixdiv_fe,LKA4,'PISA','EXAB',BNSEC4,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka4.gt.0 .and.   
     +  iqf(lka4+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs, IC, iqf(lka4+1),
     +  iqf(lka4+2), qf(lka4+2))
            
      endif  ! check if sector has hits or last subevent

      lka5 = lfncc2(5,1)
      call u_put_ds(ixdiv_fe,LKA5,'PISA','EXAB',BNSEC5,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka5.gt.0 .and.   
     +  iqf(lka5+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs, IC, iqf(lka5+1),
     +  iqf(lka5+2), qf(lka5+2))
            
      endif  ! check if sector has hits or last subevent 

      lka6 = lfncc2(6,1)
      call u_put_ds(ixdiv_fe,LKA6,'PISA','MPAL',BNSEC6,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka6.gt.0 .and.   
     +  iqf(lka6+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs_fplt, IC, iqf(lka6+1),
     +  iqf(lka6+2), qf(lka6+2))
            
      endif  ! check if sector has hits or last subevent 

      lka7 = lfncc2(7,1)
      call u_put_ds(ixdiv_fe,LKA7,'PISA','MPAL',BNSEC7,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka7.gt.0 .and.   
     +  iqf(lka7+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs_fplt, IC, iqf(lka7+1),
     +  iqf(lka7+2), qf(lka7+2))
            
      endif  ! check if sector has hits or last subevent 

      lka8 = lfncc2(8,1)
      call u_put_ds(ixdiv_fe,LKA8,'PISA','MPAL',BNSEC8,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka8.gt.0 .and.   
     +  iqf(lka8+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs_fplt, IC, iqf(lka8+1),
     +  iqf(lka8+2), qf(lka8+2))
            
      endif  ! check if sector has hits or last subevent 

      lka9 = lfncc2(9,1)
      call u_put_ds(ixdiv_fe,LKA9,'PISA','MPAL',BNSEC9,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka9.gt.0 .and.   
     +  iqf(lka9+1).gt.0)then
            
      call dstrootout(
     +  dcode_abs_fplt, IC, iqf(lka9+1),
     +  iqf(lka9+2), qf(lka9+2))
            
      endif  ! check if sector has hits or last subevent 
     
c      !  ENTRY PARTICLES
      lka10 = lfncc3(1,1)
      call u_put_ds(ixdiv_fe,LKA10,'PISA','EXNT',BNSEC10,' ')
            
      if(
     +  root_output.eq.1 .and. 
     +  lka10.gt.0 .and.   
     +  iqf(lka10+1).gt.0)then
            
      call dstrootout(
     +  dcode_ent, IC, iqf(lka10+1),
     +  iqf(lka10+2), qf(lka10+2))
            
      endif  ! check if sector has hits or last subevent

      
           
      return
      end
            
