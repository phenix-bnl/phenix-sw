c $Id: guphad.f,v 1.4 2008/05/21 08:22:09 hpereira Exp $

      subroutine guphad
 
      IMPLICIT NONE

c    *************************************************************
c    *                                                           *
c    *  GUPHAD (vsn 1.00) User routine to generate one hadronic  *
c    *                    interaction                            *
c    *  Called by ==> :: GTHADR,GTNEUT
c    *  IN   ::                                                  *
c    *  OUT  ::                                                  *
c    *                                                           *
c    *  written  by ::  Surender Saini, 16/05/93 10.04.21        *
c    *  modified by ::  SRTonse 8/4/93 minor, not funtional changes
c    *                                                           *
c    *************************************************************

c  User routine to generate one hadronic interaction
c     ==> Called by : GTHADR,GTNEUT
c 16-OCT-1992 / S. Saini

c  Use GHEISHA only if IHADR < 3  ( default )
c      GHEISHA and HADRIN/NUCRIN  if IHADR = 3
c      FLUKA and HADRIN/NUCRIN if IHADR=4

c --------------
#include "gcphys.inc"
c --------------
 
      LOGICAL first
      data first/.TRUE./
c  ---------------------------------------------------
 
      if(first)then
        if (ihadr .eq. 5)then
          write(6,*) 'guphad - Hadron Shower Code: FLUKA'
          write(6,*) 'guphad - Neutrons below 20 MeV done by MICAP'
        endif
        if (ihadr .eq. 4)then
          write(6,*) 'guphad - Hadron Shower Code :: FLUKA'
          write(6,*) 'guphad - Neutrons below 20 MeV done by GHEISHA'
        endif
        if (ihadr .eq. 1)then 
          write(6,*) 'guphad - Hadron Shower Code :: GHEISHA'
        end if
       first = .FALSE.
      end if
 
      if (ihadr .eq. 4)then
       call fldist
      endif
      if (ihadr .eq. 5) then
       call gfmdis
      endif
      if (ihadr. eq. 1) then
       call gpghei
      end if
 
      return
      end
