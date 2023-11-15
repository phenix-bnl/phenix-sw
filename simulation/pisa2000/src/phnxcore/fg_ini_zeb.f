*CMZ :  2.04/00 16/12/94  14.26.54  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.42  by  Charles F. Maguire
*-- Author :
*-- Author :
      subroutine fg_ini_zeb
      implicit    none

c ZEBRA init for PHNX GEANT

*KEEP,STOSTO.
#include "stosto.inc"
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,FGLINK.
#include "fpglink.inc"
*KEND.

      call mzlink(ixstor_f,'/FGLINK/',lfg_link,lfg_lref,lfg_last)
      return
      end
