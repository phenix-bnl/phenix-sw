c... Defines action to take at each tracking step for
c... relative luminosity telescope. Basically just calls stepping 
C... for each RPC module.c
C...
c... Authors: M. C. McCain and L. A. Linden Levy
C...
C...  ============================================================
C...  CHANGELOG:
C...
C...  ============================================================
      SUBROUTINE RLT_stp

      IMPLICIT NONE

#include "gckine.inc"
#include "gctmed.inc"
#include "guphnx.inc"
      
      if ( ISVOL .gt. 0 .and. CHARGE .ne. 0.0) then
         if (NUMED.EQ.4301) then
            CALL RLTRPC1_stp 
         else if (NUMED.EQ.4302) then
            CALL RLTRPC2_stp 
         else if (NUMED.EQ.4303) then
            CALL RLTRPC3_stp 
         endif
      endif

      return
      END
