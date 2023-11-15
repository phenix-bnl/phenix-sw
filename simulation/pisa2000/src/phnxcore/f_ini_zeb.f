*CMZ :  2.04/00 02/06/93  20.51.52  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      subroutine f_ini_zeb
      implicit    none

c Init ZEBRA F store

*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEND.
      call mzstor(ixstor_f,'/FSTORE/',' ',lqf_fence,lqf(1),
     1  lqf(2),lqf(2),lqf(pf_store_div12),lqf(pf_store_size))

      call mzdiv(ixstor_f,ixdiv_fr,'FR',pf_rdiv_size,pf_rdiv_max,'CLR')
      call mzdiv(ixstor_f,ixdiv_fe,'FE',pf_ediv_size,pf_ediv_max,'C')
c     call mzlogl(ixstor_f,0)
      call mzlogl(ixstor_f,-1)     ! cfm: re-set for "terse" logging
      return
      end
