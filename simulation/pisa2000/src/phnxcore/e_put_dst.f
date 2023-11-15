c     $Id: e_put_dst.f,v 1.35 2011/03/29 16:06:40 arbint Exp $
c     Steering procedure to write event dst. using ZEBRA  SRTonse 20-JUL-1992

      subroutine  e_put_dst
      implicit   none
      

#include "guphnx.inc"
#include "gctime.inc"
#include "gcflag.inc"
#include "udst.inc"
#include "subevt.inc"
#include "event.inc"
#include "evntcode.inc"
#include "guevgen.inc"
#include "pythia_e.inc"
#include "gugeom.inc"

      integer icall /0/
      integer isq_grndm
      integer isq_start /0/  
      integer itime_evt /0/

      integer kpart
      character*4 volume
      
c     Begin execution
      budst_new_evt = .true.

c     Check if user requests that only accepted events be written out.
c     Then, if event is accepted, write out as normal
c     otherwise only write out special header
      
      if (cudst_otag_typ .eq. 'EVNT' )  then

        ! checking for central arm acceptance condition
        if(logacc.AND..not.logaccepted) then
          return
        endif                  

        ! checking for MuID layer acceptance condition
        if(muidwrite.gt.0 .and. .not.muidaccepted) then
            return
         endif                 

        ! relic check because Muon Arm still needs ZEBRA output for their geometry file
        if(zebra_output.eq.0) then
          iudst_pr_uhead(1) = pisaInputRunNumber
          iudst_pr_uhead(2) = pisaOutputRunNumber
          iudst_pr_uhead(3) = pisaProjectNumber
          iudst_pr_uhead(4) = versionNumber  
        endif  

        iudst_pr_uhead(5) = IDRUN      ! GEANT run number
        iudst_pr_uhead(6) = NTRU_EVT   ! true event number
        iudst_pr_uhead(7) = idevt      ! GEANT event number
        iudst_pr_uhead(8) = NSUB_EVT   ! sub-event in event number
        iudst_pr_uhead(9) = nsubevents ! total number of subevents
        iudst_pr_uhead(10)= nptls      ! total number of particles (needs to be initialized)
        iudst_pr_uhead(11)= ipopsub    ! particles per subevent (needs to be intialized)
        iudst_pr_uhead(12)= bimevt*1000.
        iudst_pr_uhead(13)= atarg
        iudst_pr_uhead(14)= ztarg
        iudst_pr_uhead(15)= aproj
        iudst_pr_uhead(16)= zproj
        iudst_pr_uhead(17)= sqrt_s*1000.
        iudst_pr_uhead(18)= bmin*1000.
        iudst_pr_uhead(19)= bmax*1000.
        iudst_pr_uhead(20)= t0start*1.e+15    ! femtoseconds (integer)
	      iudst_pr_uhead(21) = xyz(1)*10000.    ! event vertex x
	      iudst_pr_uhead(22) = xyz(2)*10000.    ! event vertex y
	      iudst_pr_uhead(23) = xyz(3)*10000.    ! event vertex z
        iudst_pr_uhead(24) = igdate           ! date as YYMMDD (Y2K problem)
        iudst_pr_uhead(25) = igtime           ! time as HHMM
        iudst_pr_uhead(26) = nrndm(1)         ! first random number seed
        iudst_pr_uhead(27) = nrndm(2)         ! second random number seed
        
c       Get starting GEANT random number sequence value (1 to 215 range)
        if( icall.eq.0) then
          icall = 1
          isq_start = isq_grndm(nrndm(1), nrndm(2))
          iudst_pr_uhead(28) = isq_start    
        else
          iudst_pr_uhead(28) = isq_start
        endif
        call timel(itime_evt)
        iudst_pr_uhead(29) = itime_evt

c       Event generator specific
        iudst_pr_uhead(30) = event_code

        iudst_pr_nuh = 40
        if(event_code.eq.hijing) then
          ! number of binary collisions, introduced for HIJING1.37
          iudst_pr_uhead(31) = ntry 
        endif
        
        if((event_code.eq.rv_phi).or.(event_code.eq.sngl_phi)) then
          iudst_pr_uhead(31) = nrvphi_evt
          iudst_pr_uhead(32) = nrvacc_evt
        endif
        
        if((event_code.eq.rv_jpsi).or.(event_code.eq.sngl_jpsi)) then
          iudst_pr_uhead(31) = nrvjpsi_evt
          iudst_pr_uhead(32) = nrvacc_evt
        endif
        
        if(event_code.eq.rv_chi) then
          iudst_pr_uhead(31) = nrvchi_evt
          iudst_pr_uhead(32) = nrvacc_evt
        endif
        
        if(event_code.eq.sngl_neutral) then
          iudst_pr_uhead(31) = neutral_evt
          iudst_pr_uhead(32) = nrvacc_evt
        endif

        if(event_code.eq.pythia) then
          iudst_pr_nuh = 70

c         Pythia event header
          iudst_pr_uhead(31)=pyth_proc_id
          fudst_pr_uhead(32)=pyth_bjork(1)
          fudst_pr_uhead(33)=pyth_bjork(2)
          fudst_pr_uhead(34)=pyth_partstu(1)
          fudst_pr_uhead(35)=pyth_partstu(2)
          fudst_pr_uhead(36)=pyth_partstu(3)
          fudst_pr_uhead(37)=pyth_qsqr
          fudst_pr_uhead(38)=pyth_ptrans

c         Intermediate particles
          do kpart = 1,4
            iudst_pr_uhead(39+(kpart-1)*5) = intr_part_id(kpart)
            fudst_pr_uhead(40+(kpart-1)*5) = intr_part_p(1,kpart)
            fudst_pr_uhead(41+(kpart-1)*5) = intr_part_p(2,kpart)
            fudst_pr_uhead(42+(kpart-1)*5) = intr_part_p(3,kpart)
            fudst_pr_uhead(43+(kpart-1)*5) = intr_part_p(4,kpart)
          enddo
        endif  
        
c       Locations 71 to 83 in header go to retraction positions
        iudst_pr_uhead(71) = iEastWest
        do kpart = 1,3
          fudst_pr_uhead(71+kpart) = shiftEast(kpart)
          fudst_pr_uhead(74+kpart) = shiftWest(kpart)
          fudst_pr_uhead(77+kpart) = rotateEast(kpart)
          fudst_pr_uhead(80+kpart) = rotateWest(kpart)
        enddo

c       Locations 90 to 94 for the MAGF control line information
c       Map file choice code taken from gufld.f
        volume = cpvolu_opt(1,1)
        call cltou(volume)
        
        if(volume.eq.'OLD'.or.volume.eq.'VOLD') then
          iudst_pr_uhead(90) = 4
          
        elseif( volume.eq.'LIN' ) then
          iudst_pr_uhead(90) = 5
          
        elseif( volume.eq.'QUAD'.or.volume.eq.'2D97' ) then
          iudst_pr_uhead(90) = 0
          
        elseif( volume.eq.'2D01'.or.volume.eq.'2DIM' ) then
          iudst_pr_uhead(90) = 1
          
        elseif( volume.eq.'3D01'.or.volume.eq.'3DIM' ) then
          iudst_pr_uhead(90) = 2
          
        elseif( volume.eq.'3D03'.or.volume.eq.'3D+0' ) then
          iudst_pr_uhead(90) = 3
          
        elseif( volume.eq.'2D03' ) then
          iudst_pr_uhead(90) = 6
          
        elseif( volume.eq.'3D++' ) then
          iudst_pr_uhead(90) = 8
          
        elseif( volume.eq.'2D04' ) then
          iudst_pr_uhead(90) = 7
          
        elseif( volume.eq.'3D+-'.or.volume.eq.'MNAM' ) then
          iudst_pr_uhead(90) = 9
        else
	         
          write( 6,10)cpvolu_opt(1,1)
 10       format('e_put_dst - cpvolu_opt(1,1) = ',a4)
          stop ' MAGF in pisa.kumac has invalid first option field'
          
        endif       

        fudst_pr_uhead(91) = rpvolu_opt(2,1)  ! MAGF scale factor
        fudst_pr_uhead(92) = rpvolu_opt(3,1)  ! MAGF R cutoff in gustep
        fudst_pr_uhead(93) = rpvolu_opt(4,1)  ! MAGF Z cutoff in gustep
        fudst_pr_uhead(94) = rpvolu_opt(5,1)  ! MAGF momentum cutoff in gustep

c       location 95 for reaction plane angle, 96 for transvere angle
        fudst_pr_uhead(95) = reactionPlaneAngle

        iudst_pr_uhead(100) = 0
        if( ivolu_opt(1,13).ne.0.and.cvolu_opt(4,13).ne.'NONE') then
           iudst_pr_uhead(100) = 1  ! indicate that zdc was used
        endif

        if(zebra_output.eq.1) then
           call head_put_dst
        endif

        if(root_output.eq.1) then
           call headrootout(iudst_pr_uhead(1), fudst_pr_uhead(1))
        endif

c       new output call for the restricted kine information
c       all add primary event bank
        call fk_put_dst       ! all particles in a detector (must go first for DIO)
        call fx_put_dst       ! primary particles

c        alphabetical ordering of the detector sets according to key letter code
c       first check that detector set exits and then that DIGI was called

        if( ivolu_opt(1,14).ne.0.and.cvolu_opt(4,14).ne.'NONE') then
          call fa_put_dst    ! aer #14 in sequence
        endif
              
        if( ivolu_opt(1,2).ne.0.and.cvolu_opt(4,2).ne.'NONE') then
          call fb_put_dst    ! bbc #2 in sequence
        endif
              
        if( ivolu_opt(1,5).ne.0.and.cvolu_opt(4,5) .ne. 'NONE') then
          call FC_PUT_DST    ! CRK #5 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,3).ne.0.and.cvolu_opt(4,3) .ne. 'NONE') then
          call FD_PUT_DST    ! SVX (SILICON TRACKER) #3 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,8).ne.0.and.cvolu_opt(4,8) .ne. 'NONE') then
          call FE_PUT_DST    ! EMC (All EMCal types) #8 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,7).ne.0.and.cvolu_opt(4,7) .ne. 'NONE') then
          call FF_PUT_DST    ! TOF #7 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,4).ne.0.and.cvolu_opt(4,4) .ne. 'NONE') then
          call FI_PUT_DST    ! ITR (DC/PAD1) #4 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,10).ne.0.and.cvolu_opt(4,10) .ne. 'NONE') then
          call FM_PUT_DST    ! MUM (MUON TRACKERS) #10 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,11).ne.0.and.cvolu_opt(4,11) .ne. 'NONE') then
          call FN_PUT_DST    ! MUN (MUON IDENTIFIER) #11 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,22).ne.0.and.cvolu_opt(4,22) .ne. 'NONE') then
          call FNCC_PUT_DST    ! NCC #22 IN SEQUENCE
        endif

        if( ivolu_opt(1,25).ne.0.and.cvolu_opt(4,25) .ne. 'NONE') then
          call FNCC_PUT_DST    ! MXPS #25 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,23).ne.0.and.cvolu_opt(4,23) .ne. 'NONE') then
          call FMPC_PUT_DST  ! MPC #23 IN SEQUENCE
        endif

        if( ivolu_opt(1,24).ne.0.and.cvolu_opt(4,24) .ne. 'NONE') then
          call FMPC_PUT_DST  ! MPCX #24 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,9).ne.0.and.cvolu_opt(4,9) .ne. 'NONE') then
          call FP_PUT_DST    ! PAD (PAD2/PAD3) #9 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,20).ne.0.and.cvolu_opt(4,20) .ne. 'NONE') then
          call Fmupc_PUT_DST    ! MuPC #20 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,16).ne.0.and.cvolu_opt(4,16) .ne. 'NONE') then
          call FQ_PUT_DST    ! NTC (NEW TIMING COUNTER) #16 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,21).ne.0.and.cvolu_opt(4,21) .ne. 'NONE') then
          call FRLT_PUT_DST   ! RLT #21 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,18).ne.0.and.cvolu_opt(4,18) .ne. 'NONE') then
          call FS_PUT_DST    ! RXN (Reaction Plane) #18 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,15).ne.0.and.cvolu_opt(4,15) .ne. 'NONE') then
          call FH_PUT_DST    ! HBD (HBD Counter, WIS Version) #15 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,17).ne.0.and.cvolu_opt(4,17) .ne. 'NONE') then
          call FR_PUT_DST    ! TPC (HBD/TPC Counter) #17 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,6).ne.0.and.cvolu_opt(4,6) .ne. 'NONE') then
          call FT_PUT_DST    ! TRD (TRD/TEC) #6 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,12).ne.0.and.cvolu_opt(4,12) .ne. 'NONE') then
          call FW_PUT_DST    ! TFW (CENTRAL MUON IDENTIFIER) #12 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,19).ne.0.and.cvolu_opt(4,19) .ne. 'NONE') then
          call FY_PUT_DST    ! FCL (Forward calorimeter) #19 IN SEQUENCE
        endif
              
        if( ivolu_opt(1,13).ne.0.and.cvolu_opt(4,13) .ne. 'NONE') then
          call FZ_PUT_DST    ! ZDC #13 IN SEQUENCE
        endif
              
C       write out dst. specified in DOUT card, + an end of event (ENDE) d.st.
        if( zebra_output.eq.1) then
          call dout_put_dst
        endif
        
        if( root_output.eq.1) then
          if( end_evtflg) then
            call endrootout(2)
          else
            call endrootout(1)
          endif
        endif
              
      else if (cudst_otag_typ .eq. 'PARA') then
        
        iudst_pr_uhead(5) = idrun     ! run number
        iudst_pr_uhead(6) = 1         ! readout_type
        iudst_pr_nuh = 6
        
c       alphabetical ordering of the detector sets according to key letter code
c       first check that detector set exits and then that GEOM was called
        if( ivolu_opt(1,14).ne.0.and.cvolu_opt(1,14) .ne. 'NONE') then
          call FA_PUT_DST    ! AER #14 IN SEQUENCE
        endif
        if( ivolu_opt(1,2).ne.0.and.cvolu_opt(1,2) .ne. 'NONE') then
          call FB_PUT_DST    ! BBC #2 IN SEQUENCE
        endif
        if( ivolu_opt(1,5).ne.0.and.cvolu_opt(1,5) .ne. 'NONE') then
          call FC_PUT_DST    ! CRK #5 IN SEQUENCE
        endif
        if( ivolu_opt(1,3).ne.0.and.cvolu_opt(1,3) .ne. 'NONE') then
          call FD_PUT_DST    ! SVX (SILICON TRACKER) #3 IN SEQUENCE
        endif
        if( ivolu_opt(1,8).ne.0.and.cvolu_opt(1,8) .ne. 'NONE') then
          call FE_PUT_DST    ! EMC (All EMCal types) #8 IN SEQUENCE
        endif
        if( ivolu_opt(1,7).ne.0.and.cvolu_opt(1,7) .ne. 'NONE') then
          call FF_PUT_DST    ! TOF #7 IN SEQUENCE
        endif
        if( ivolu_opt(1,4).ne.0.and.cvolu_opt(1,4) .ne. 'NONE') then
          call FI_PUT_DST    ! ITR (DC/PAD1) #4 IN SEQUENCE
        endif
        if( ivolu_opt(1,10).ne.0.and.cvolu_opt(1,10) .ne. 'NONE') then
          call FM_PUT_DST    ! MUM (MUON TRACKERS) #10 IN SEQUENCE
        endif
        if( ivolu_opt(1,11).ne.0.and.cvolu_opt(1,11) .ne. 'NONE') then
          call FN_PUT_DST    ! MUN (MUON IDENTIFIER) #11 IN SEQUENCE
        endif
        if( ivolu_opt(1,22).ne.0.and.cvolu_opt(1,22) .ne. 'NONE') then
          call FNCC_PUT_DST     ! NCC (Ncc Calorimeter) #22 IN SEQUENCE
        endif
        if( ivolu_opt(1,25).ne.0.and.cvolu_opt(1,25) .ne. 'NONE') then
          call FNCC_PUT_DST     ! MPCEX_PS #25 IN SEQUENCE
        endif
        if( ivolu_opt(1,23).ne.0.and.cvolu_opt(1,23) .ne. 'NONE') then
          call FMPC_PUT_DST ! MPC #23 IN SEQUENCE
        endif
        if( ivolu_opt(1,24).ne.0.and.cvolu_opt(1,24) .ne. 'NONE') then
          call FMPC_PUT_DST ! MPCX #24 IN SEQUENCE
        endif
        if( ivolu_opt(1,9).ne.0.and.cvolu_opt(1,9) .ne. 'NONE') then
          call FP_PUT_DST    ! PAD (PAD2/PAD3) #9 IN SEQUENCE
        endif
        if( ivolu_opt(1,20).ne.0.and.cvolu_opt(1,20) .ne. 'NONE') then
          call Fmupc_PUT_DST    ! MuPC #20 IN SEQUENCE
        endif
        if( ivolu_opt(1,16).ne.0.and.cvolu_opt(1,16) .ne. 'NONE') then
          call FQ_PUT_DST    ! NTC (New Timing Counter) #16 IN SEQUENCE
        endif
        if( ivolu_opt(1,21).ne.0.and.cvolu_opt(1,21) .ne. 'NONE') then
          call FRLT_PUT_DST   ! RLT #21 IN SEQUdENCE
        endif
        if( ivolu_opt(1,18).ne.0.and.cvolu_opt(1,18) .ne. 'NONE') then
          call FS_PUT_DST    ! RXN (Reaction Plane) #18 IN SEQUENCE
        endif
        if( ivolu_opt(1,15).ne.0.and.cvolu_opt(1,15) .ne. 'NONE') then
          call FH_PUT_DST    ! HBD (WIS Version) #15 IN SEQUENCE
        endif
        if( ivolu_opt(1,17).ne.0.and.cvolu_opt(1,17) .ne. 'NONE') then
          call FR_PUT_DST    ! TPC (HBD/TPC Counter) #17 IN SEQUENCE
        endif
        if( ivolu_opt(1,6).ne.0.and.cvolu_opt(1,6) .ne. 'NONE') then
          call FT_PUT_DST    ! TRD (TRD) #6 IN SEQUENCE
        endif
        if( ivolu_opt(1,12).ne.0.and.cvolu_opt(1,12) .ne. 'NONE') then
          call FW_PUT_DST    ! TFW (CENTRAL MUON IDENTIFIER) #12 IN SEQUENCE
        endif
        if( ivolu_opt(1,19).ne.0.and.cvolu_opt(1,19) .ne. 'NONE') then
          call FY_PUT_DST     ! FCL (Forward Calorimeter) #19 IN SEQUENCE
        endif
      endif   ! check on EVENT or PARA type output
      return
      end
