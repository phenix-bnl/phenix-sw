*CMZ :  2.04/00 23/11/94  13.25.39  by  Charles F. Maguire
*CMZ :  2.03/00 11/10/92  02.49.55  by  Charles F. Maguire
*-- Author :  Y. Akiba
      subroutine crk_out(LUN)
      implicit none
      integer LUN
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPCLINK.
#include "fpclink.inc"
*KEND.

      call FZOUT(LUN,ixdiv_fe,lFC_Cal(1),1,' ',5,1,'CCAL')
      call FZOUT(LUN,ixdiv_fe,lFC_Tra(1),0,'L',5,1,'CTRA')

      return
      end
