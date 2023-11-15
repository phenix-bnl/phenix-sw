c $Id: gutrev.f,v 1.3 2008/05/21 08:22:09 hpereira Exp $
      subroutine gutrev
      implicit none
      
      ! User routine to control tracking of one event
#include "guphnx.inc"
      integer icall /0/

      if(do_trak.ne.0)then
        if(icall.eq.0) write(*,*) 'gutrev - tracking requested'
        icall=1
        call gtreve
      else
        if(icall.eq.0) write(*,*) 'gutrev - no tracking requested'
        icall=1
      endif
      return
      end
