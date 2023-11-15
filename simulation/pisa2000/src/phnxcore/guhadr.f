c $Id: guhadr.f,v 1.5 2008/05/21 08:22:09 hpereira Exp $
 
      subroutine guhadr
 
      IMPLICIT NONE

c    *************************************************************
c    *                                                           *
c    *  GUHADR (vsn 1.00) User routine to generate on hadronic   *
c    *                    interaction                            *
c    *  Called by ==> :: GTHADR, GTNEU{                          *
c    *  IN   ::                                                  *
c    *  OUT  ::                                                  *
c    *                                                           *
c    *  written  by ::  Surender Saini, 16/05/93 10.15.20        *
c    *  modified by ::                                           *
c    *                                                           *
c    *************************************************************

c  User routine to generate one hadronic interaction
c     ==> Called by : GTHADR,GTNEUT
c 16-OCT-1992 / S. Saini

c  Use GHEISHA only if IHADR < 3  ( default )
c      GHEISHA and HADRIN/NUCRIN  if IHADR = 3
c      FLUKA and HADRIN/NUCRIN if IHADR=4

c May 2, 2001 C.F. Maguire  Add MICAP option with IHADR = 5

c --------------------
#include "gcphys.inc"
c --------------------
      integer*4 ihfirst
      data ihfirst/1/
 
c  ---------------------------------------------------
      if(ihfirst .eq. 1)then
       write(6,*) 'guhadr - hadron code : ',ihadr
       ihfirst = 0
      end if
 
      if (ihadr .eq. 4)then
       call flufin
      endif
      if (ihadr .eq. 5)then
       call gfmfin
      endif
      if (ihadr .eq. 1)then 
       call gheish
      end if
 
      return
      end
