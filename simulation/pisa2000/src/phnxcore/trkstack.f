      subroutine trkstack(ltrack)

      implicit none

#include "g77trigdef.inc"


c    Initial version from CFM on August 2, 1994

c    Purposes is to store track numbers in an internal stack
c    Routine has a first event initialization for the ZEBRA IO characteristics
c    Routine is then initialized subevent-by-subevent by a call from GU_DIGI
c    Routine is called on a hit-by-hit basis from the user output subroutines
c    Routine is called at the end of the subevent from GU_DIGI to create and
c            fill the output ZEBRA bank called FKIN

c    LTRACK = -111 and LTRACK = -999 indicate the GU_DIGI calls
c    LTRACK > 0 indicates a user call
c    Any other value of LTRACK will cause an error message, and then a return

c maximum number of subevents for the link pointers
#include "sublink.inc"      
            
c FKIN bank format and helper variables
#include "fpklink.inc"         
            
c Parameters and Declarations for the FOPI ZEBRA Store (FSTORE)
#include "fstore.inc"          
            
c common block to allow GSKING, GSSTAK to allow storage of user words for secondaries
#include "secubuf.inc"          
#include "gcnum.inc"  
            
c with hit detector name, global variable
#include "guhitdet.inc"         

      ! local variables
      integer track_err /0/          ! count number of track error inputs
      integer track_err_limit        ! limit on number of track error inputs
      parameter (track_err_limit = 100)
      integer ltrack                 ! track number  (INPUT)
      integer jtrack                 ! parent track number for gfkine
      integer newstore               ! new ancestor store flag
      integer spart, snvert          ! for gfkine call
      real pvrtx(4), svrtx(3)        ! for gfkine call
 
      integer mxtrack                ! maximum number of tracks
      parameter (mxtrack = 20000)
      integer ktrack(mxtrack)        ! currently stored track numbers
      integer ifound                 ! indicate if track previously stored
      integer mtrack                 ! count of currently stored tracks
      integer mtrack2                ! count of currently stored tracks
      integer last_track             ! last track stored
      integer iloop                  ! loop index over MTRACK
      integer iloop2                 ! loop index over MTRACK2 (inside iloop)
      integer ioikin                 ! ZEBRA booking
      integer nmbook                 ! number of entries to book
      integer ipoint                 ! pointer to ZEBRA bank
      integer jpoint                 ! pointer to ZEBRA bank
      integer ifirst /0/             ! initialization indicator

      integer kcount                 ! count of entries in FKIN
      integer ngener                 ! average number of generations
      parameter (ngener = 25)      
      integer kcount_max             ! maximum value of kcount
      integer ntrack_sav             ! track number of calling particle
 
      real ptot, pthet, pphi         ! vertex kinematic variables
      real vrad, vthet, vphi         ! vertex kinematic variables
 
      character*20 chform            ! ZEBRA formatting
 
      save mtrack, ktrack, ifirst, ioikin, last_track

c     begin execution

      if(ifirst.eq.0)then

c     beginning of run initialization

         ifirst = 1

c     FKIN data bank as 11 words per entry

         chform = '1I / 2I 7F 2I'
         call mzform('FKIN', chform, ioikin)
      endif  !  start of run initialization
 
 
      if(ltrack.eq.-111)then

c     subevent initialization call from GUDIGI

         mtrack = 0
         last_track = 0
         return  ! initialization call complete
      endif  ! subevent initialization
 
      if(ltrack.gt.0)then

c     call by user

         if(mtrack.eq.0)then
            mtrack = 1
            ktrack(1) = ltrack
            last_track = ltrack
            return  ! finished first store
         else
            if(ltrack.eq.last_track)then
               return
            else
               last_track = ltrack
            endif ! check on same track as prior call

c    check if this track was already stored before the previous call

            do iloop = 1,mtrack
               if(ltrack.eq.ktrack(iloop))then
                  return  ! already stored
               endif ! check if track already stored
            enddo ! loop over previously stored tracks

c     track not previously stored

            mtrack = mtrack + 1
            if(mtrack.le.mxtrack)then
               ktrack(mtrack) = ltrack
               return  ! stored new track number
            else
               write(6,*)' '
               write(6,*)' TRKSTACK <E>:  MXTRACK = ',
     1                   mxtrack,' IS TOO SMALL'
               stop ' PISA STOPPING'
            endif  ! safety check
         endif ! check on mtrack = 0
      endif  ! check on call by user
 
      if(ltrack.eq.-999) then

c     subevent completion call from GUDIGI

        nmbook = (ngener*mtrack + 1)*mfk_kine   ! number to book (>= 10)

c     We dont now how many entries to book until we do the complete ancestry.
c     We assume an average of 10 generations per track for initial booking.
c     We could to an MZPUSH at the end to save space; no yet done.

        kcount_max = ngener*mtrack              ! maximum number of entries
        lfk_kine(1) = 0
        call MZBOOK(ixdiv_fe,
     $    LFK_KINE(1),           !return bank address
     $    LFK_KINE(1),           !supporting link
     $    1,                     !JBIAS=1 ---> top level bank
     $    'FKIN',                !bank name
     $    0,                     !# of links
     $    0,                     !# of down links
     $    NMBOOK,                !# of data words
     $    IOIKIN,                !I/O characteristics
     $    -1)                    !do not clear memory contents
        IPOINT = LFK_KINE(1) + 1           ! counts the number of records
        JPOINT = IPOINT                    ! first position
         
        kcount = 0                  ! count how many entries so far
        
c       mtrack2 adds on ancestor tracks
        
        mtrack2 = mtrack
        do iloop = 1,mtrack
          call gfkine(ktrack(iloop),svrtx,pvrtx,spart,snvert,
     1      ubuf,nubuf)
          ptot = sqrt(pvrtx(1)*pvrtx(1) + pvrtx(2)*pvrtx(2) +
     1      pvrtx(3)*pvrtx(3))
          if(ptot.gt.0.0)then
            pthet = acosd(pvrtx(3)/ptot)
            pphi = atan2d(pvrtx(2), pvrtx(1))
          else
            pthet = 0.0
            pphi = 0.0
          endif
          
          ! globar vertex    
          vrad = sqrt(svrtx(1)*svrtx(1) + svrtx(2)*svrtx(2))
          
          !if(vrad.gt.0.0.and.svrtx(3).ne.0.0)then
          if(vrad.gt.0.0) then
            vthet = acosd(svrtx(3)/sqrt(vrad*vrad +
     1        svrtx(3)*svrtx(3)))
            vphi = atan2d(svrtx(2), svrtx(1))
          else
            vthet = 0.0
            vphi = 0.0
          endif
          
          ntrack_sav = -ktrack(iloop)
          iqf(ipoint + ofsk_tr) = ktrack(iloop)
          iqf(ipoint + ofsk_id) = spart
          qf(ipoint + ofsk_mo) = ptot
          qf(ipoint + ofsk_th) = pthet
          qf(ipoint + ofsk_ph) = pphi
          qf(ipoint + ofsk_rv) = vrad
          qf(ipoint + ofsk_zv) = svrtx(3)
          qf(ipoint + ofsk_tv) = vthet
          qf(ipoint + ofsk_pv) = vphi
C     DLW: New code here.  We initialize every thing to 0, defaulting to primary
C     If the event generator created this track, NUBUF = 1 or 3, and we can use
C     the user buffer to fill in some info.
          iqf(ipoint + ofsk_pn) = 0 ! indicate not yet found
          iqf(ipoint + ofsk_pi) = 0 ! default as no parent ID

          jtrack = 0
          snvert = 0
          spart = 0


c    check if the user buffer has useful information (ubuf is not reset)
          
          if(nubuf.eq.3)then
             
c     NUBUF is set to 3 in GUKINE from the GEN_EVT event generator
c     These are "orphan" tracks since they have no parent track number from
c     the event generator.  For these tracks UBUF(1) = 0
             
             iqf(ipoint + ofsk_pn) = ubuf(1)
             iqf(ipoint + ofsk_pi) = ubuf(2) ! parent ID number
             go to 30
          endif                 ! check on nubuf = 3
          
          newstore = -1         ! initial store requested
          do while (nubuf.eq.1.or.nubuf.eq.2)
             
c     NUBUF is set to 1 in GUSTEP for some secondaries stored on the GEANT stack
c     NUBUF is set to 2 in GUSTEP for muons (UBUF(2) is then a tag variable)
             
c     NEWSTORE variable tags when a parent information needs storing
             
             if(newstore.ne.0)then
                iqf(ipoint + ofsk_pn) = ubuf(1) ! parent track number
             endif
             jtrack = ubuf(1)   ! parent track number given to gfkine
             if(jtrack.gt.0)then
                call gfkine(jtrack,svrtx,pvrtx,spart,snvert,
     1               ubuf,nubuf)

c    Protect against particles that are their own parents!
                
                if(nubuf.eq.1.or.nubuf.eq.2) then
                   if(jtrack.eq.ubuf(1)) then
                      print *,' trkstack.f: particle is own parent, skipping...'
                      nubuf = 0
                   end if
                end if

                if(newstore.ne.0)then
                   iqf(ipoint + ofsk_pi) = spart ! parent id number
                endif
                
c     Have stored parent track number and parent ID of this track
c     Now check if this ancestor track has been stored previously
                
                ifound = 0
                do iloop2 = 1, mtrack2
                   if(jtrack.eq.ktrack(iloop2))then
                      
c     ancestor track already stored, break out of this do loop
                      
                      ifound = 1
                      go to 20
                   endif        ! check if ancestor track already stored
                enddo           ! loop over previously stored tracks
 20             continue        ! branch point for breaking out of mtrack2 loop
                if(ifound.eq.0)then

c     ancestor track not previously stored, put in array

                   mtrack2 = mtrack2 + 1
                   if(mtrack2.le.mxtrack)then
                      ktrack(mtrack2) = jtrack
                   else
                      write(6,*)' '
                      write(6,*)' TRKSTACK <E>:  MXTRACK = ',
     1                     mxtrack,' IS TOO SMALL'
                      stop ' PISA STOPPING IN ANCESTOR CHECK'
                   endif        ! check for being below maximum
                endif           ! check if ancestor previously stored
                newstore = 0
                if(ifound.eq.0) then ! DLW: store only if not already found (each parent now appears once only)

c     previously not stored or a primary particle
c     get new pointer index for ancestor or primary particle

                   newstore = 1 ! request subsequent parent store
                   ipoint = ipoint + mfk_kine
                   kcount = kcount +1
                   if(kcount.gt.kcount_max)then
                      stop ' TRKSTACK <E>: too many generations'
                   endif
                   ptot = sqrt(pvrtx(1)*pvrtx(1) + pvrtx(2)*pvrtx(2)
     1                  + pvrtx(3)*pvrtx(3))
                   if(ptot.gt.0.0)then
                      pthet = acosd(pvrtx(3)/ptot)
                      pphi = atan2d(pvrtx(2), pvrtx(1))
                   else
                      pthet = 0.0
                      pphi = 0.0
                   endif
                   vrad = sqrt(svrtx(1)*svrtx(1) + svrtx(2)*svrtx(2))
                   if(vrad.gt.0.0.and.svrtx(3).ne.0.0)then
                      vthet = acosd(svrtx(3)/sqrt(vrad*vrad +
     1                     svrtx(3)*svrtx(3)))
                      vphi = atan2d(svrtx(2), svrtx(1))
                   else
                      vthet = 0.0
                      vphi = 0.0
                   endif        ! check on kinematics
                   iqf(ipoint + ofsk_tr) = jtrack
                   iqf(ipoint + ofsk_id) = iabs(spart)
                   qf(ipoint + ofsk_mo) = ptot
                   qf(ipoint + ofsk_th) = pthet
                   qf(ipoint + ofsk_ph) = pphi
                   qf(ipoint + ofsk_rv) = vrad
                   qf(ipoint + ofsk_zv) = svrtx(3)
                   qf(ipoint + ofsk_tv) = vthet
                   qf(ipoint + ofsk_pv) = vphi
                   iqf(ipoint + ofsk_pn) = 0 ! indicate not yet found
                   iqf(ipoint + ofsk_pi) = 0 ! default as no parent ID
                   if(nubuf.eq.3)then ! track is primary and event gen supplied further info
                      iqf(ipoint + ofsk_pn) = ubuf(1)
                      iqf(ipoint + ofsk_pi) = ubuf(2)
                   endif        ! check if reached primary vertex
                endif           ! check if new ancestor or primary
             else
                nubuf = 0       ! break out of do while loop
             endif              ! check on jtrack greater than 0
          end do                ! loop over NUBUF = 1 or NUBUF = 2
 30       continue              ! branch point for NUBUF = 3 conditional
          ipoint = ipoint + mfk_kine
          kcount = kcount + 1
          if(kcount.gt.kcount_max)then
             stop ' TRKSTACK <E>: too many generations'
          endif
       enddo                    ! loop over all tracks
       iqf(jpoint) = kcount + 1 ! extra entry for NTRACK value
       iqf(ipoint + ofsk_tr) = -999 ! signal for the last entry
       iqf(ipoint + ofsk_id) = ntrack ! the number of tracks in JKINE
       last_track = 0           ! should be set anyway in the -111 call
       return                   ! end-of-subevent call
      endif                     ! end of subevent completion call
 

c     LTRACK value < 1 but not recognized

      write(6,1)ltrack, hits_det
 1    format(/,2x,'TRKSTACK <E>: Illegal track number = ',i12,/,
     +         2x,'Detector identifier ',a4,/,
     +         2x,'Track is being skipped',/)
      if(track_err.le.track_err_limit)then
         track_err = track_err + 1
         return
      else
         write(6,2)track_err_limit
2     format(' PISA stopping, exceeding track error limit = ',i6)
         stop ' Track error limit reached'
      endif
      end
