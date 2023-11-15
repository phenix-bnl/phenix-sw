      subroutine itr_digi

      IMPLICIT NONE
 
#include "gcflag.inc"
#include "guphnx.inc"

      logical logdbg
      save logdbg

      if (idebug.gt.0.and.logdbg) call gphits('ITR ','*   ')

      if (cvolu_opt(1,4).eq.'FULL'.or.cvolu_opt(1,4).eq.'VOLS') then
c     call the routines for the old 1994 geometry

         write(6,*) 'ITR_DIGI-F: Obsolete geometry selected.'
         write(6,*) 'Use the IT96 switch instead.  Nothing done.'

      elseif (cvolu_opt(1,4).eq.'PC1N') then
c     call the routines for the new PC1 geometry only.

         call pc1out            ! Update of PD1OUT for new PC1 geometry

      elseif (cvolu_opt(1,4).eq.'IT96') then
c     call the routines for the new PC1 geometry and new DC geometry

         call pc1out
         call dchout96          ! for "sectorless" DC

      endif

      return
      end
