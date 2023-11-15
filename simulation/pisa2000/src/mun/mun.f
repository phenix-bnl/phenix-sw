*CMZ :  2.04/00 27/10/94  21.28.19  by  Charles F. Maguire
*CMZ :  2.03/00 22/06/93  13.59.07  by  Surender Saini
*-- Author :    Surender Saini   12/04/93
      subroutine mun( full,nh)

c    *************************************************************
c    *                                                           *
c    *  MUN (vsn 1.00)   muon_arm dummy routine ; not used       *
c    *                                                           *
c    *  Called by ==> :: < GUGEOM >                              *
c    *  IN   :: full, nh                                         *
c    *  OUT  :: none                                             *
c    *                                                           *
c    *  written  by ::  Surender Saini, 12/04/93 04.42.50        *
c    *  modified by ::  Charles F. Maguire 10/26/1994            *
c    *                                                           *
c    *************************************************************

c This is now a working subroutine which installs the Muon ID subsystem

 
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPNLINK.
#include "fpnlink.inc"
*KEND.
 
      character*4 full
      integer     nh

c     begin execution

      call muident
      call mubpip

      return
      end
