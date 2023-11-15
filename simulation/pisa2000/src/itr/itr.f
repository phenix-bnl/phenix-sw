      subroutine itr(full,nh)


c       Standard PISA geometry start-up for the DCs and the PCs

c       SRTonse  1-JUN-1992 11:54 Code written by Nikolai Smirnoff for
c       Intermediate tracker (drift & pad chambers) converted to PISA style.
c       detectors have IDTYPE = 4

c      CFM revisions 5-JUL-1992 in order to conform better to PISA standards
c      N.Sm revision 28-DEC-1992, to reduce the "activity" of this routine
c      to  CALL: DCGEOM, PD1GEM, DCHDET, PD1DET.

c      NS/CFM complete revision July 19, 1994

c      Revised by K.Filimonov on September 6th, 1995.
c      The choice of a new geometry for pad chambers is introduced.
c      No more call of ITGEOM.  Calls DCGEOM, PD1GEOM, DCHDET, PD1DET
c      directly without intermediate subroutine.

c     Revised by Jeffery Mitchell on February 20, 1996.
c     A new switch is introduced in order to run the updated, sectorless
c     drift chamber geometry while keeping the old 18-sector geometry
c     intact until all PISORP code has been rewritten to reflect the new
c     geometry.

c     Revised by Jeffery Mitchell on October 3, 1996.
c     Eliminated all pre-1996 geometries.


      implicit none

#include "guphnx.inc"
 
      integer*4 nh              !set before call in gugeom
      character*4 full          ! set before call in gugeom

c ******************************************************

c   Sets up the geometry and the senstive detector volumes for the DCs and PCs

c ******************************************************

c   Local variables

      integer jflbnk,nmdcch,nmpd1c

C  only book volumes if input parameters are OK

      if (cvolu_opt(1,4).eq.'PC1N') then

c     Calls for new PC1 geometry, but no DC geometry

         write(6,*) ' ITR <I>: new PC1/no DC geometry activated'
       
         call pc1gem(nmpd1c,jflbnk)
         call pc1det(nmpd1c)

      elseif (cvolu_opt(1,4).eq.'IT96') then

c     CALLS FOR NEW DC AND PC1 GEOMETRY
c     Calls new DCGEOM96, DCHDET96, and new PC1GEM, PC1DET
       
         write(6,*) ' ITR <I>: new DC and PC1 geometry activated'
 
         call dcgeom96(nmdcch,jflbnk)
         call pc1gem(nmpd1c,jflbnk)
 
         call dchdet96(nmdcch)
         call pc1det(nmpd1c)
 
      else
         write(6,*) ' ITR <I>: No volumes defined'
      endif   ! cvolu_opt


c     Old DCSHIELD call  (Kenta Shigaki prototype versions are OBSOLETE)
c     The DC Shields (renamed Photon Shields) are installed via the GEOP control line

      if(cvolu_opt(2,4).eq.'SHL1'.or.
     +   cvolu_opt(2,4).eq.'SHL2'.or.
     +   cvolu_opt(2,4).eq.'SHL3'.or.
     +   cvolu_opt(2,4).eq.'SHL4'.or.
     +   cvolu_opt(2,4).eq.'SHL5')then
         call dcshield(cvolu_opt(2,4))
      endif  ! check if DCSHIELD requested

      return
      end
