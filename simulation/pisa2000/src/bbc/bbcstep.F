c $Id: bbcstep.F,v 1.3 2008/05/21 08:21:54 hpereira Exp $
      subroutine bbcstep
      
C=====================================================================

C    DESCRIPTION: PROTOTYPE OF DETECTOR SPECIFIC GUSTEP ROUTINE
C                 FILLS HIT DATA STRUCTURE FOR BBC SET

C    MAP: CALLED BY GUSTEP
C         CALLS GSAHIT

C    PROCEDURE: CHECKS ON STATUS OF INWVOL AND ISTOP VARIABLES
C               THESE ARE GEANT TAGS ON PARTICLE'S TRACKING STATE
C               INWVOL = 0   ENTERING A NEW VOLUME
C                        1   EXITING A VOLUME
C                        2   EXITING MOTHER VOLUME
C               ISTOP  = 0   NOT STOPPED
C                      = 1   LOST IDENTITY
C                      = 2   FELL BELOW THRESHOLD
C=====================================================================
C     GLOBAL SPECIFICATIONS:
C---------------------------------------------------------------------
      IMPLICIT      NONE

#include "geant321/gcking.inc"
#include "gckine.inc"
#include "gcbank.inc"
#include "gctrak.inc"
#include "gcflag.inc"
#include "gctmed.inc"
#include "gcsets.inc"
#include "gcvolu.inc"
#include "namlvl.inc"
#include "guphnx.inc"
#include "secubuf.inc"

C=====================================================================
C    EXTERNAL SPECIFICATIONS:
C---------------------------------------------------------------------

C=====================================================================
C    LOCAL VARIABLES:
C---------------------------------------------------------------------

      integer     ihit             ! return from gsahit
      real        hit_all(11)      ! standard hit variables
      real        slengin          ! path length at the entrance
      logical     first /.true./   ! initial call switch

      save hit_all, slengin


C=====================================================================
C    EXECUTABLE STATEMENTS FOLLOW BELOW
C---------------------------------------------------------------------

      if(first)then
         first = .false.
         write(6, * ) 'bbcstep - first call'
      endif

C---------------------------------------------------------------------
C     CHECK ON ENTERING A NEW VOLUME

      if(inwvol.eq.1)then
c toru: save the entering position, time, momentm for all tracks.

         hit_all(1) = vect(1)              ! x-position master frame
         hit_all(2) = vect(2)              ! y-position master frame
         hit_all(3) = vect(3)              ! z-position master frame
         hit_all(4) = 0.0                  ! initialize energy loss
         hit_all(5) = tofg*1.e9            ! tof in nanoseconds
         hit_all(6) = ipart                ! particle id
         hit_all(7) = vect(4)*vect(7)      ! px-component master frame
         hit_all(8) = vect(5)*vect(7)      ! py-component master frame
         hit_all(9) = vect(6)*vect(7)      ! pz-component master frame
         hit_all(10) = 0.0                 ! initialize path length in detector
         hit_all(11) = getot               ! total energy
         slengin = sleng                   ! path length so far
      endif

C---------------------------------------------------------------------
C     CHECK ON PARTICLE STILL IN SAME VOLUME

      if(inwvol.eq.0)then
        hit_all(4) = hit_all(4) + destep   ! increment energy loss
        if(istop.ne.0) then
          hit_all(10)=sleng-slengin
          call gsahit(iset,idet,itra,numbv,hit_all,ihit)
          if(ihit.le.0)then
            write(6, '(a,2(1x,a,i5))' )
     1               'bbcstep - gsahit reports error',
     2               '   ISET ',iset,
     3               '   IDET ',idet
         endif  !  check on error return
        endif
      endif

C---------------------------------------------------------------------
C     CHECK ON PARTICLE EXITING

      if(inwvol.gt.1) then
        hit_all(4) = hit_all(4) + destep  ! add last energy loss
        hit_all(10)= sleng-slengin
        call gsahit(iset,idet,itra,numbv,hit_all,ihit)
        if(ihit.le.0)then
          write(6, '(a,2(1x,a,i5))' )
     1      'bbcstep - gsahit reports error',
     2      '   iset ',iset,
     3      '   idet ',idet
        endif  !  check on error return
      endif
      return
      end
