C --- PCLINK.inc ---------------------------------------------------------
c     CFM proposition for PHNX  CERENKOV   ZEBRA banks April 7, 1992
C     6 NOV 90  added ringwise calibration
C     8 nov 90 changed RHIT data structure: flag match to ring 6 included
C     9 jan 91 modified to include Z determination
c
c    24 Apr 92 YA Simplified for CERK of PHENIX
c                 No links to calibration, etc. There will be only
c                 CCAL bank (for a while)
c     3 MAY 92 YA links for CTRA bank, CHIT banks are added.
c                 Description of CCAL, CTRA, CHIT are revised
c     4 MAY 92 YA Add ipar in CHIT
c
c    12 MAY 92 YA Add (px,py,pz,vx,vy,vz) in CTRA
c
c    15 MAY 92 YA Add (px,py,pz) in CHIT. THis is to test angular cut
c                for CPHO detector
c                The contents of CHIT is now (x,y,z,tof,px,py,pz,pid,itra,ipar)
c    23 NOV 92 YA Major addition for PISA release 2.0
c                 Geometry parameters from PHNX.PAR are stored in 'PARA' bank
c                 A link variable lFC_PARA is added to support 'PARA' bank
c                 Offsets in 'PARA' bank are defined.
c                 Additional user parameters are stored in 'PARU' bank
c                 A link variable lFC_PARU is added to support 'PARU' bank
c                 Offsets in 'PARU' bank are defined.
c    27 AUG 96 KS Add (bi1, bp1, bi2, bp2) in CHIT to handle baffle in PISORP
c     4 SEP 96 KS Add nbf (number of hit baffles) in CHIT
c
      common   /FCLINK/
     #   lFC_link,     ! start of structural links
     #   lFC_Cal(MAXSUB),          ! Calibrated data
     #  lFC_Tra(MAXSUB),          ! bank for virtual tracker
     #  lFC_PARA,         ! bank for geometrical parameter
     #  lFC_PARU,         ! bank for user parameter
     #  lFC_SpareLinks(5),
     #  lFC_lref,         !                 start of reference links
     #  lFC_SpareRefs(5),
     #   lFC_last          !                 last link ptr.
C
      INTEGER*4 lFC_Cal         !pointer to the CCAL bank
      INTEGER*4 lFC_Tra         !pointer to the CTRA bank
      INTEGER*4 lFC_PARA        !pointer to the PARA bank
      INTEGER*4 lFC_PARU        !pointer to the PARU bank
      INTEGER*4 lFC_link, lFC_lref, lFC_last
      INTEGER*4 lFC_SpareLinks, lFC_SpareRefs
C ---------------------------------------------------------------------------
C --- structure of a calibrated data bank       bank ID: CCAL
C
c                   lFC_Cal
c                    |
c                    V
c   -MUL  ...  -2  -1  0 MUL, (CELL, NPE, TOF), ...,
c     |         |   |    number of hit CERENKOV detector
c     |         |   V
c     |         V   CHIT bank for the first hit cell
c     V         CHIT bank for the second hit cell
c     CHIT bank for the MUL-th hit cell
c
c The description of CHIT bank is in the next section
c
      INTEGER*4 mFC_Cal
      INTEGER*4 oFCc_CELL, oFCc_NPE, oFCc_TOF
C
C --- offsets
C
      PARAMETER ( mFC_CAL = 3 )  ! Size of 1 entry in calibrated data bank
      PARAMETER ( oFCc_CELL = 0) ! Offset cell number
      PARAMETER ( oFCc_NPE  = 1 )! Offset # of photo-electrons in the cell
      PARAMETER ( oFCc_TOF  = 2 )! Offset HIT TOF value
C ---------------------------------------------------------------------------
C --- structure of a "original hit" bank       bank ID: CHIT
c      JCHIT = lqf(lFC_Cal - ICHIT)
c     |
c     V
c     0 MUL, (x, y, z, tof, PID, ITRA, IPAR), ...,
      INTEGER*4 mFC_HIT
      INTEGER*4 oFCh_x,   oFCh_y,   oFCh_z,   oFCh_tof
      INTEGER*4 oFCh_px,  oFCh_py,  oFCh_pz,  oFCh_pid
      INTEGER*4 oFCh_tra, oFCh_par
      INTEGER*4 oFCh_nbf, oFCh_bi1, oFCh_bp1, oFCh_bi2, oFCh_bp2
C
C --- offsets
C
      PARAMETER ( mFC_HIT =15 )  ! Size of 1 entry in CHIT data bank
      PARAMETER ( oFCh_x  = 0 )  ! Offset x coordinate of the hit
      PARAMETER ( oFCh_y  = 1 )  ! Offset y coordinate of the hit
      PARAMETER ( oFCh_z  = 2 )  ! Offset z coordinate of the hit
      PARAMETER ( oFCh_tof= 3 )  ! Offset tof of the hit
      PARAMETER ( oFCh_px = 4 )  ! Offset px of the hit
      PARAMETER ( oFCh_py = 5 )  ! Offset py of the hit
      PARAMETER ( oFCh_pz = 6 )  ! Offset pz of the hit
      PARAMETER ( oFCh_pid= 7 )  ! Offset particle id  of hit
      PARAMETER ( oFCh_tra= 8 )  ! Offset track # (GEANT, in KINE) of the hit
      PARAMETER ( oFCh_par= 9 )  ! Offset parent track# (in KINE)
                                 ! (0 if the hit is not due to GEANTINO)
      PARAMETER ( oFCh_nbf=10 )  ! Offset number of hit baffles
      PARAMETER ( oFCh_bi1=11 )  ! Offset first baffle ID
      PARAMETER ( oFCh_bi2=12 )  ! Offset second baffle ID
      PARAMETER ( oFCh_bp1=13 )  ! Offset first baffle hit position
      PARAMETER ( oFCh_bp2=14 )  ! Offset second baffle hit position
C ---------------------------------------------------------------------------
C --- structure of "cerenkov virtual tracker" data bank       bank ID: CTRA
C
c     lFC_TRA
c     |
c     V
c     0 MUL, (x, y, z, PID, ITRA, PVx,PVy,PVz,Vx,Vy,Vz ), ...,
c
      INTEGER*4 mFC_TRA
      INTEGER*4 oFCt_x, oFCt_y, oFCt_z, oFCt_pid, oFCt_itra
      INTEGER*4 oFCt_pvx,oFCt_pvy,oFCt_pvz,oFCt_vx,oFCt_vy,oFCt_vz
C
C --- offsets
C
      PARAMETER ( mFC_TRA = 11)  ! Size of 1 entry in tracking bank
      PARAMETER ( oFCt_x =   0)  ! Offset x
      PARAMETER ( oFCt_y =   1)  ! Offset y
      PARAMETER ( oFCt_z =   2)  ! Offset z
      PARAMETER ( oFCt_pid=  3)  ! Offset pid
      PARAMETER ( oFCt_itra= 4)  ! Offset track# (in KINE)
      PARAMETER ( oFCt_pvx = 5)  ! Offset pvx (px at vertex)
      PARAMETER ( oFCt_pvy = 6)  ! Offset pvy (py at vertex)
      PARAMETER ( oFCt_pvz = 7)  ! Offset pvz (pz at vertex)
      PARAMETER ( oFCt_vx =  8)  ! Offset vx
      PARAMETER ( oFCt_vy =  9)  ! Offset vy
      PARAMETER ( oFCt_vz = 10)  ! Offset vz
C ---------------------------------------------------------------------------
C --- structure of "cerenkov geometry parameter" data bank       bank ID: PARA
C
c offsets in 'PARA' bank for CERK geometry parameters. These parameters are
c read at run time from phnx.par file
c
c offsets in 'PARA' bank for global geometry parameters
c
      integer    oC_PHI_CNTR    ! phi angle of the center of the two arms
      parameter (oC_PHI_CNTR     =   1)
      integer    oC_PHI_OPEN    ! phi opening (half angle) of the two arms
      parameter (oC_PHI_OPEN     =   2)
      integer    oC_DPHI_CARM   ! phi span (fill angle) of CARM outer envelope
      parameter (oC_DPHI_CARM    =   3)
      integer    oC_DPHI_CSHE   ! phi span (fill angle) of CSHE outer envelope
      parameter (oC_DPHI_CSHE    =   4)
      integer    oC_DPHI_CPHO   ! phi span (fill angle) of CPHO outer envelope
      parameter (oC_DPHI_CPHO    =   5)
      integer    oC_PHI_CPHO_OFF ! phi offset of CPHO center from CSHE center
      parameter (oC_PHI_CPHO_OFF =   6)
      integer    oC_N_SECT      ! number of sectors
      parameter (oC_N_SECT       =   7)
      integer    oC_N_SPM       ! number of supermodules in a sector
      parameter (oC_N_SPM        =   8)
      integer    oC_N_PMT       ! number of PMT's in a supermodule
      parameter (oC_N_PMT        =   9)
c
c offsets in 'PARA' bank for photon detector geometry parameters
c
      integer    oC_R_PMT_ENT   ! radius of PMT (cone) entrance
      parameter (oC_R_PMT_ENT    =  10)
      integer    oC_DX_PMT      ! sideward displacement of each PMT from the strongback
      parameter (oC_DX_PMT       =  11) ! 32 parameters:  11 -  42
      integer    oC_R_PMT       ! radial position of each PMT
      parameter (oC_R_PMT        =  43) ! 32 parameters:  43 -  74
      integer    oC_Z_PMT       ! z position of each PMT
      parameter (oC_Z_PMT        =  75) ! 32 parameters:  75 - 106
      integer    oC_THETA_PMT   ! theta (elevation) angle of each PMT
      parameter (oC_THETA_PMT    = 107) ! 32 parameters: 107 - 138
c
c offsets in 'PARA' bank for mirror geometry parameters
c
      integer    oC_MIR_RIN     ! radius of inner surface of the mirrors
      parameter (oC_MIR_RIN      = 139)
      integer    oC_MIR_THCK    ! mirror thickness
      parameter (oC_MIR_THCK     = 140)
      integer    oC_MIR_THETA1  ! theta angle of low |z| side edge of mirrors
      parameter (oC_MIR_THETA1   = 141)
      integer    oC_MIR_THETA2  ! theta angle of high |z| side edge of mirrors
      parameter (oC_MIR_THETA2   = 142)
      integer    oC_MIR_THETACUT ! theta angle of mirror cover
      parameter (oC_MIR_THETACUT = 143)
      integer    oC_MIR_PHI1    ! starting phi angle
      parameter (oC_MIR_PHI1     = 144)
      integer    oC_MIR_PHI2    ! ending phi angle
      parameter (oC_MIR_PHI2     = 145)
      integer    oC_MIR_DZ      ! displacement of mirror center along z
      parameter (oC_MIR_DZ       = 146)
c
c offsets in 'PARA' bank for windows geometry parameters
c
      integer    oC_WI1_RIN     ! inner radius of the entrance window
      parameter (oC_WI1_RIN      = 147)
      integer    oC_WI1_THCK    ! thickness of the entrance window
      parameter (oC_WI1_THCK     = 148)
      integer    oC_WI1_ZEND    ! z position of the entrance window edges
      parameter (oC_WI1_ZEND     = 149)
      integer    oC_WI2_RIN     ! inner radius of the exit window
      parameter (oC_WI2_RIN      = 150)
      integer    oC_WI2_THCK    ! thickness of the exit window
      parameter (oC_WI2_THCK     = 151)
      integer    oC_WI2_ZEND    ! z position of the exit window edges
      parameter (oC_WI2_ZEND     = 152)
c
c offsets in 'PARA' bank for virtual trackers geometry parameters
c
      integer    oC_TR1_RIN     ! inner radius of the first virtual tracker
      parameter (oC_TR1_RIN      = 153)
      integer    oC_TR1_THCK    ! thickness of the first virtual tracker
      parameter (oC_TR1_THCK     = 154)
      integer    oC_TR1_ZEND    ! z position of the first virtual tracker edges
      parameter (oC_TR1_ZEND     = 155)
      integer    oC_TR2_RIN     ! inner radius of the second virtual tracker
      parameter (oC_TR2_RIN      = 156)
      integer    oC_TR2_THCK    ! thickness of the second virtual tracker
      parameter (oC_TR2_THCK     = 157)
      integer    oC_TR2_ZEND    ! z position of the second virtual tracker edges
      parameter (oC_TR2_ZEND     = 158)
c
      integer    CRK_PARA_ND    ! number of data words in 'PARA'
      parameter (CRK_PARA_ND     = 158)
c
C ---------------------------------------------------------------------------
C --- structure of "cerenkov user parameter" data bank       bank ID: PARU
C
c offsets in 'PARU' bank for CERK parameters.
c Parameters stored in the 'PARU' bank are:
c     1) GAS radiator.....read from GFFGO.DAT file at run time
c     2) N0' value = N0 / (active region ratio); added 16.Oct.96 by K.Shigaki
c
      integer    oCRAD_GAS      ! Cerenkov radiator Gas.
      parameter (oCRAD_GAS       =   1)
      integer    oCRK_N0INI     ! N0 / (active area ratio) in PISA
      parameter (oCRK_N0INI      =   2)
c
      integer    CRK_PARU_ND    ! number of data words in 'PARU'
      parameter (CRK_PARU_ND     =   2)
c
c ---------------------------------------------------------------------------
c Since we use 'float' for all these parameter values, we must encode
c the value of CPHO_GAS, which is actually a character string, and takes
c value 'CH4 ' or 'C2H6'
c
c YA  'CO2 ' and 'FR13' are added  
c
      real CRAD_GAS_C2H6
      parameter (CRAD_GAS_C2H6 = 1.0)
      real CRAD_GAS_CH4
      parameter (CRAD_GAS_CH4  = 2.0)
      real CRAD_GAS_N2
      parameter (CRAD_GAS_N2   = 3.0)
      real CRAD_GAS_CO2
      parameter (CRAD_GAS_CO2  = 4.0)
      real CRAD_GAS_FR13
      parameter (CRAD_GAS_FR13 = 5.0)
c
c --- N0p = N0 / (active area ratio)
      real N0_INIT
      parameter (N0_INIT = 200.0)
c
c --- other constants to be passed to PISORP
      real    N_C2H6
      parameter (N_C2H6 = 1.00082) ! index of refraction in C2H6 gas (see memo)
      real    N_CH4
      parameter (N_CH4  = 1.000444) ! index of refraction in CH4 gas (see memo)
      real    N_N2
      parameter (N_N2   = 1.000297) ! index of refraction in N2 gas
      real    N_CO2
      parameter (N_CO2  = 1.000450) ! index of refraction in CO2 gas
      real    N_FR13
      parameter (N_FR13 = 1.00078)  ! index of refraction in Freon 13 gas
C --- end of PCLINK.inc --------------------------------------------------
