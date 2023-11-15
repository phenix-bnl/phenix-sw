c     $Id: gukine.f,v 1.11 2009/08/24 21:31:45 winter Exp $

      subroutine gukine
      implicit none
      
c     authors: s.r. tonse and c.f. maguire (fopi relic)
c     sets up subevent filling of primary particles into geant common blocks

c     global variables
#include "gcflag.inc"
#include "gckine.inc"
#include "gclist.inc"
#include "guphnx.inc"
#include "guevgen.inc"
#include "sublink.inc"
#include "fpxlink.inc"
#include "fstore.inc"
#include "subevt.inc"
#include "udst.inc"
#include "gcunit.inc"
 
c     local variables
      integer   ident, ier, i
      integer   evt_status, idxprt
      integer   nttrck, nremain, nstore, nvtx, nubuf
 
c     KINE and VERT user space
c     local variables moved into GUKINE
      Integer NUKINE, NUVERT
      Parameter (NUKINE = 0)
      Parameter (NUVERT = 0)
      Real UVERT (10), UKINE (10)


c     FPRI ZEBRA bank additions
      character*20 chform            ! ZEBRA formatting
      integer ifirst /0/             ! initialization key
      integer ioixin /0/             ! MZFORM return value
      integer nmbook                 ! number of words to book
      integer jpoint                 ! first position
      integer ipoint                 ! counts number of records
      integer itrack_sub             ! track number in subevent

c     Executable code 
      if(ifirst.eq.0) then
        
        !FPRI data bank as 5 words per entry
        chform = '1I / 2I 3F'
        call mzform('FPRI', chform, ioixin)
        ifirst = 1
        
      endif


c       Handle PISA standard event input. GUEVGEN reads complete events.
c         Breaking of events into sub-events is handled uniformly here for
c         all generators. GUEVGEN also produces vertex positions for the
c         complete event which must be used to create the vertex for
c         each sub-event.
c       ==================================================================
 
      if (end_EVTFLG) then

c       if last event complete, call event input routine to fetch
c       complete event from user-chosen source of events.
c       =========================================================
        call guevgen (evt_status)

c       evt_status has to be set in by the user event routines

        if (evt_status .ne. -1) then
          if (evt_status .eq. 2) then
            write (*, 900)
            ieotri = 1       ! terminate this event
          else
            write (*, 901) evt_status
            ieotri = 1       ! terminate this event
            ieorun = 1       ! terminate this run
          end if
          goto 9999
        end if

c       check that the predicted number of subevents will be less than the
c       limit of maxsub which can be handled by pisorp

        if(ipopsub.gt.0)then
          if((1 + mxtot/ipopsub) .gt. maxsub)then
            write(*,902)(1 + mxtot/ipopsub), maxsub
            write(*,903)ipopsub, 1 + mxtot/maxsub
            ieotri = 1       ! terminate this event
            goto 9999
          endif               ! safety check on predicted number of subevents
        endif                  ! check on ipopsub > 0
              

c       start new event, fzout will write "start of new evt" marker
c       ============================================================
        end_evtflg = .false.
        budst_new_evt = .true.
        ievprt = 1
        nsub_evt = 0
        ntru_evt = numevt
        call t0init            ! randomize start time for geant (tofg)
        call event_hist        ! event histogramming array (j. sullivan request)
      end if
            

c     determine how many particles for this sub-event
c     ===============================================
      nsub_evt = nsub_evt + 1
      nremain = mxtot - ievprt + 1
      if (nremain .le. ipopsub) then
        nstore = nremain
        end_evtflg = .true.
      else
        nstore = ipopsub
      end if

      ! Book the FPRI bank
      ! clear event division
      call mzwipe(ixdiv_fe)     
      nmbook = (nstore + 1)*mfx_kine
      
      !safety set
      lfx_kine(1) = 0           
      
      call MZBOOK(
     +  ixdiv_fe,
     +  LFX_KINE(1),           !return bank address
     +  LFX_KINE(1),           !supporting link
     +  1,                     !JBIAS=1 ---> top level bank
     +  'FPRI',                !bank name
     +  0,                     !# of links
     +  0,                     !# of down links
     +  NMBOOK,                !# of data words
     +  IOIXIN,                !I/O characteristics
     +  -1)                    !do not clear memory contents
      IPOINT = LFX_KINE(1) + 1  ! counts the number of records
      JPOINT = IPOINT           ! first position
      iqf(jpoint) = nstore
      
      if(nsub_evt.eq.1 ) then
        if (numevt.le.10) then
          write(LOUT,*) 'gukine - event: ', numevt
        elseif(numevt.le.1000.and.mod(numevt,100).eq.0) then
          write(LOUT,*) 'gukine - event: ', numevt
        elseif(mod(numevt,1000).eq.0) then
          write(LOUT,*) 'gukine - event: ', numevt
        endif
      endif
                   
      if(numevt.lt.3.or.(50*(numevt/50).eq.0))then
        write(LOUT, '(a,4(/,1x,(a,i6)) )' ) 
     +    ' gukine - ', 
     +    'total particles:',mxtot,
     +    'subevent number:',nsub_evt, 
     +    'number stored this subevent: ',nstore, 
     +    'Number remaining: ', nremain -
     +    nstore
        write(LOUT,*)
      endif   

      ! randomize in the xy direction if requested
      if(budst_new_evt.and.ixy_rndm.gt.0) then
        call xy_rndm
        if(ixy_change.eq.0)then
          ixy_change = 1
          write(*, '(/,a,i2)' ) '   xy randomization with ixyrnd ='
     +    //' ', ixy_rndm
        endif
      endif       

      ! load vert and kine for next sub-event
      if(multi_xyz.ne.1)then
        call gsvert (xyz, 0, 0, uvert, nuvert, nvtx)
      endif 
       
      ! Store tracks for this sub-event in KINE.
      itrack_sub = 0
      do idxprt = ievprt, ievprt + nstore - 1

         if(multi_xyz.eq.1)then ! pass multiple vertices to geant
            call gsvert( xyzmv(1,idxprt), 0, 0, uvert, nuvert, nvtx)
         endif    

C         ! check on sensible parent id number      
C C        write(*,*) 'gukine - particle ', idxprt,' has id_parent = ',
C C     +       id_parent(idxprt)
C         if(id_parent(idxprt).gt.0)then
C           nubuf = 3 ! will indicate an orphan track
C           ukine(1) = 0 ! event generator does not furnish a parent track
C           ukine(2) = id_parent(idxprt) ! parent id from event generator          
C         else
C           nubuf = nukine
C         endif   

C DLW: change the logic for the user buffer.  Use NUBUF=3 for *all* primaries.  The 
C difference now is that if the generator does not supply parent PID, then we explicitly
C set the second user word to zero.  This replicates the convention in trkstack, while
C allowing the user buffer to be used in a consistent manner.  Note that this explicitly 
C assumes id_parent is initialized to zero before the generator runs, but then again, so 
C does the previous code.

        nubuf = 3
        ukine(1) = 0 ! no parent track id should come from the generator (it wouldn't make sense anyway)
        ukine(2) = id_parent(idxprt) ! if generator provides a parent PID, use it.
                      
        call gskine(
     +    pptot(2, idxprt), idtot (idxprt), nvtx,
     +    ukine, nubuf, nttrck)
        
        ! store output  
        iqf(ipoint + ofsx_tr) = idxprt
        iqf(ipoint + ofsx_id) = idtot(idxprt)
        qf(ipoint + ofsx_px) = pptot(2,idxprt)
        qf(ipoint + ofsx_py) = pptot(3,idxprt)
        qf(ipoint + ofsx_pz) = pptot(4,idxprt)
        itrack_sub = itrack_sub + 1
        iqf(ipoint + ofsx_trsub) = itrack_sub
        ipoint = ipoint + mfx_kine
        
        ! check on ZDC installation
        if(lgeom(2).eq.2)then

c         A.Ster addition to pass primary particle id to ZDC
c    (    works correctly if # of tracks in subevent = 1)
c    T    O BE FIXED LATER (how to get this for more then more particle / subevent)

          call zdc_gukine(idxprt)
        endif
        
      end do
       
      IEVPRT = IEVPRT + NSTORE
      MAXTRK = NTTRCK           ! Last track stored

      ! if user requests it, print out kine bank
      if (idebug .ne. 0) then
        call gpkine(0)
      end if
       
 9999 Continue       
  900 format (1h , 'gukine - error ', i3, ' in guevgen, aborting event')
  901 format (1h , 'gukine - eor flag ', i3, ' in guevgen, end-of-run')
  902 format (1h , 'gukine - error:  predicted number of subevents = ',
     1 i3, '  exceeds current limit in phool = ',i3)
  903 format (1h , 'gukine - you need to increase the ',
     1  'number of tracks per subevent from ', i4,
     2  ' to ', i4)
      Return
      end
      
