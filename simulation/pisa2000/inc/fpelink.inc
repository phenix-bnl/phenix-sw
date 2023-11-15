C
C --- FPELINK.inc ---------------------------------------------------------
c     CFM proposition for PHNX  EMC   ZEBRA banks May 7, 1992
c       Revised 9/20/92 to be compatible with the PISA Release 2
c
c       Revised 05/18/93 to be compatible with new emc routines
c       G. David
c
c       scintillator parameters from GSI (possible future use)
c
       common       /FELINK/
     #        lFE_link,         ! start of structural links
     #        lFE_CAL(MAXSUB),  ! DIGI subroutine output
     #        lFE_PARA,         ! From the PAR file
     #        lFE_PARU,         ! User derived parameters
     #  lFE_SpareLinks(7),
     #  lFE_lref,         !                 start of reference links
     #  lFE_SpareRefs(5),
     #        lFE_last          !                 last link ptr.
C
      INTEGER*4 lFE_PARA, lFE_CAL, lFE_PARU
      INTEGER*4 lFE_link, lFE_lref, lFE_last
      INTEGER*4 lFE_SpareLinks, lFE_SpareRefs
C -----------------------------------------------------------------------------
C -------  Topology
C
      INTEGER*4  mFE_ALLDETS              ! maximum number to bank
       integer  emc_units              ! total number of calorimeter units
       integer*4 inv_occup              ! occupancy = 1.0/float(inv_occup)
       parameter (emc_units=32000)       ! assume 3 Sh-K, 1 PbGl + 4000 xtals
       parameter (inv_occup=3)       ! 33% occupancy (?)
      PARAMETER(mFE_ALLDETS=emc_units/inv_occup)
c
c       dimension parameters
c
C -------------------------------------------------------------------------
C --- structure of the parameter bank      bank ID: EPRA
C
c       This is set up for two arms and one longitudinal segment/arm
c
       integer   emc_para_nd
       parameter (emc_para_nd = 40)
c
       integer   ofea_emc_walls
       parameter (ofea_emc_walls = 0)
       integer   ofea_emc_opt
       parameter (ofea_emc_opt = 1)
       integer   ofea_emc_dum1
       parameter (ofea_emc_dum1 = 2)
       integer   ofea_emc_dum2
       parameter (ofea_emc_dum2 = 3)
       integer   ofea_emc_dum3
       parameter (ofea_emc_dum3 = 4)
       integer   ofea_emc_angle
       parameter (ofea_emc_angle = 5)
       integer   ofea_emc_rpos
       parameter (ofea_emc_rpos = 6)
c
c      The rest is right now unused (05/18/93 GD)
c
C -------------------------------------------------------------------------
C --- structure of the user bank      bank ID: EPRU
C
c      Reserve space for future use (05/18/93 GD)
c      However, right now this is not stored
c
       integer   emc_paru_nd
       parameter (emc_paru_nd = 40)
c
       integer   ofeu_emc_walls
       parameter (ofeu_emc_walls = 0)
       integer   ofeu_emc_opt
       parameter (ofeu_emc_opt = 1)
       integer   ofeu_emc_dum1
       parameter (ofeu_emc_dum1 = 2)
       integer   ofeu_emc_dum2
       parameter (ofeu_emc_dum2 = 3)
       integer   ofeu_emc_dum3
       parameter (ofeu_emc_dum3 = 4)
       integer   ofeu_emc_angle
       parameter (ofeu_emc_angle = 5)
       integer   ofeu_emc_rpos
       parameter (ofeu_emc_rpos = 6)
c
c      The rest is right now unused (05/18/93 GD)
c
C ---------------------------------------------------------------------------
C --- structure of a calibrated data bank       bank ID: FCAL
C
c       lFE_Cal
c       |
c       V
c       V
c       0 MUL, (islat, iarm, isegm, iunit, dele, tof, mult, fstr, fsid,
c                     nxtr, nxid, binthe, binphi), ...
c                     arm, segment, unit, energy loss, time-of-flight,
c                     multiplicity, first track, first parent, next track,
c                     next parent, binned theta, binned phi, ...
c
c
c	The same structure will be used to store calibrated
c	or mapped ADC data
c
      INTEGER*4 mFE_Cal
       parameter (mfe_cal = 25)
       integer ofec_hitid           ! Serial number (ID) of hit
       parameter (ofec_hitid = 0)
       integer ofec_ityp            ! 1 for calibrated data, 2 for mapped ADC
       parameter (ofec_ityp = 1)
       integer ofec_mcevt            ! Event number
       parameter (ofec_mcevt = 2)
       integer ofec_mcsub            ! Subevent number
       parameter (ofec_mcsub = 3)
       integer ofec_mcpid            ! Monte Carlo parent particle ID
       parameter (ofec_mcpid = 4)
       integer ofec_mcx              ! Vertex x
       parameter (ofec_mcx = 5)
       integer ofec_mcy              ! Vertex y
       parameter (ofec_mcy = 6)
       integer ofec_mcz              ! Vertex z
       parameter (ofec_mcz = 7)
       integer ofec_mce              ! Parent particle energy
       parameter (ofec_mce = 8)
       integer ofec_mcpx            !  Parent particle px
       parameter (ofec_mcpx = 9)
       integer ofec_mcpy            ! Parent particle py
       parameter (ofec_mcpy = 10)
       integer ofec_mcpz            ! Parent particle pz
       parameter (ofec_mcpz = 11)
       integer ofec_sdet             ! 1: PbSc, 2: PbGl 3: BaF2
       parameter (ofec_sdet = 12)
       integer ofec_wall             ! Wall (or sector, octant)
       parameter (ofec_wall = 13)
       integer ofec_zind            ! z index in corresponding array
       parameter (ofec_zind = 14)
       integer ofec_yind            ! y index in corresponding array
       parameter (ofec_yind = 15)
       integer ofec_eorig           ! Uncalibrated sum GEANT energy in tower
       parameter (ofec_eorig = 16)
       integer ofec_ecal           ! Calibrated/corrected sum energy
       parameter (ofec_ecal = 17)
       integer ofec_tfirst             ! Earliest hit
       parameter (ofec_tfirst = 18)
       integer ofec_tled             ! Simulated leading edge timing
       parameter (ofec_tled = 19)
       integer ofec_tledqual         ! Quality of calculated timing
       parameter (ofec_tledqual = 20)
       integer ofec_trk1            ! 
       parameter (ofec_trk1 = 21)
       integer ofec_trk2            ! 
       parameter (ofec_trk2 = 22)
       integer ofec_trk3            ! 
       parameter (ofec_trk3 = 23)
       integer ofec_trk4            ! 
       parameter (ofec_trk4 = 24)
c
c	Define two more guys which will not be used, but are a remnant
c	until emc_user and enc_user_spec merge
c
       integer ofec_zgea            ! z index in corresponding array
       parameter (ofec_zgea = 16)
       integer ofec_ygea            ! y index in corresponding array
       parameter (ofec_ygea = 17)
c
C --- end of FPELINK.inc --------------------------------------------------
C
