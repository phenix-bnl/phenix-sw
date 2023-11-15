c     $Id: head_put_dst.f,v 1.4 2009/02/24 17:34:18 hpereira Exp $
      subroutine head_put_dst

C  to write out the GEANT HEAD zebra bank
C  SRTonse  1-SEP-1992

      implicit none
*KEEP,GCBANK.
#include "gcbank.inc"
*KEND.

      call u_put_ds(ixdiv,jhead,'PISA','HEAD','    ','L')
      return
      end
