C
C --- FPVLINK.inc ---------------------------------------------------------
C     JH Kang LINK for the VERTEX (April 27, 1992)
c       Revised 9/28/92 to be compatible with the PISA Release 2
C     May 5, 1993 -- JPSullivan added 1 to all offset parameters in
C                    the para bank (the previous offsets were wrong).
C     July 27, 1994 -- JPSullivan changed structure of VMAP and VCAL banks
C                      to save "hits" information instead of dE in strips.
C                      New parameters have also been added to the PARA bank.
C     January 9, 1998 -- JPSullivan added new parameter OFVA_MED_DUMMY
C                      for use with MVD geometry parameter bank
C
C     In COMMON /FVLINK/ below (used for a Zebra bank):
C       LFV_LINK          : start of structural links
C       LFV_MAP           : Mapped (contiguous) raw data
C       LFV_CAL           : Calibrated data
C       LFV_PARA          : From the PAR file
C       LFV_PARU          : User derived parameters
C       LFV_LREF          : start of reference links
C       LFV_LAST          : last link ptr.
C
        COMMON  /FVLINK/
     #  LFV_LINK,
     #  LFV_MAP(MAXSUB),
     #  LFV_CAL(MAXSUB),
     #  LFV_PARA,
     #  LFV_PARU,
     #  LFV_SPARELINKS(7),
     #  LFV_LREF,
     #  LFV_SPAREREFS(5),
     #  LFV_LAST
C
      INTEGER*4 LFV_MAP, LFV_CAL
      INTEGER*4 LFV_PARA, LFV_PARU
      INTEGER*4 LFV_LINK, LFV_LREF, LFV_LAST
      INTEGER*4 LFV_SPARELINKS, LFV_SPAREREFS
C -----------------------------------------------------------------------------
C -------  Topology
C
      INTEGER*4  MFV_ALLDETS
      PARAMETER( MFV_ALLDETS = 5000) ! max # of VER channels hit in one event
C
C --- structure of a topological data bank entry
C
c       lFV_Top
c       |
c       V
c
C -------------------------------------------------------------------------
C --- structure of the vertex geometry parameter bank      bank ID: VPRA
C
c       This is set up for two arms and one longitudinal segment/arm
c
        INTEGER*4 VER_PARA_ND
        PARAMETER (VER_PARA_ND = 100)    !number of data words to save
        INTEGER OFVA_DVISL              ! dimension of inner silicon ladder
        PARAMETER (OFVA_DVISL = 1)
        INTEGER OFVA_DVOSL              ! dimension of outer silicon ladder
        PARAMETER (OFVA_DVOSL = 4)
        INTEGER OFVA_DVWR1              ! dimension of wafer type 1
        PARAMETER (OFVA_DVWR1 = 7)
        INTEGER OFVA_DVWR2              ! dimension of wafer type 2
        PARAMETER (OFVA_DVWR2 =10)
        INTEGER OFVA_NWHOLE             ! number of wafers missing in "hole"
        PARAMETER (OFVA_NWHOLE = 13)
        INTEGER OFVA_NWPERP             ! number of wafers (perpendicular)
        PARAMETER (OFVA_NWPERP = 14)
        INTEGER OFVA_PITCH              ! pitch size
        PARAMETER (OFVA_PITCH = 15)
        INTEGER OFVA_NSTRIP             ! number of strips in the wafer
        PARAMETER (OFVA_NSTRIP = 16)
        INTEGER OFVA_DVOEL              ! outer plate for electronics
        PARAMETER (OFVA_DVOEL = 17)
        INTEGER OFVA_DVROH              ! dimension for ROHACELL
        PARAMETER (OFVA_DVROH = 20)
        INTEGER OFVA_WDVROH             ! width of ribs in ROHACELL
        PARAMETER (OFVA_WDVROH = 24)
        INTEGER OFVA_NHVROH             ! number of holes in ROHACELL
        PARAMETER (OFVA_NHVROH = 25)
        INTEGER OFVA_VISLR              ! radius of inner silicon layer
        PARAMETER (OFVA_VISLR = 26)
        INTEGER OFVA_VOSLR              ! radius of outer silicon layer
        PARAMETER (OFVA_VOSLR = 27)
        INTEGER OFVA_SV1PH              ! angle to the first sector
        PARAMETER (OFVA_SV1PH = 28)
        INTEGER OFVA_NPHSEG
        PARAMETER (OFVA_NPHSEG = 29)
C
        INTEGER    OFVA_R1_ENC           !R of inside of inner enclosure cylinder
        PARAMETER (OFVA_R1_ENC=30)
        INTEGER    OFVA_R2_ENC           !R of inside of outer enclosure cylinder
        PARAMETER (OFVA_R2_ENC=31)
        INTEGER    OFVA_DR_ENC1         !enclosure skin thickness
        PARAMETER (OFVA_DR_ENC1=32)
        INTEGER    OFVA_DR_ENC2         !enclosure core thickness
        PARAMETER (OFVA_DR_ENC2=33)
        INTEGER    OFVA_DZ_ENC          !half-length of enclosure
        PARAMETER (OFVA_DZ_ENC=34)
        INTEGER    OFVA_R_STRUT         !inner radius of enclosure strut
        PARAMETER (OFVA_R_STRUT=35)
        INTEGER    OFVA_DR_STRUT        !thickness of enclosure strut
        PARAMETER (OFVA_DR_STRUT=36)
        INTEGER    OFVA_DZ_ENDPL        !thickness of enclosure endplate
        PARAMETER (OFVA_DZ_ENDPL=37)
        INTEGER    OFVA_DZ_PMB          !thickness of MVD pad motherboard
        PARAMETER (OFVA_DZ_PMB=38)
        INTEGER    OFVA_Z_PMB           !z of MVD pad motherboard
        PARAMETER (OFVA_Z_PMB=39)
        INTEGER    OFVA_R_PMB           !outer radius of MVD pad motherboard
        PARAMETER (OFVA_R_PMB=40)
        INTEGER OFVA_MED_SEN            !tracking medium#, sensitive Si
        PARAMETER (OFVA_MED_SEN=41)
        INTEGER OFVA_MED_INS            !tracking medium#, insensitive Si
        PARAMETER (OFVA_MED_INS=42)
        INTEGER OFVA_MED_ROH            !tracking medium#, rhoacell
        PARAMETER (OFVA_MED_ROH=43)
        INTEGER OFVA_MED_MCM            !tracking medium for MCM
        PARAMETER (OFVA_MED_MCM=44)
        INTEGER OFVA_MED_PMB            !tracking medium#, pad motherboard=G10
        PARAMETER (OFVA_MED_PMB=45)
        INTEGER OFVA_MED_ENC1           !tracking medium#, skin on enclosure
        PARAMETER (OFVA_MED_ENC1=46)
        INTEGER OFVA_MED_ENC2           !tracking medium#, core of enclosure
        PARAMETER (OFVA_MED_ENC2=47)
        INTEGER OFVA_MED_STRUT          !tracking medium#, enclosure struts
        PARAMETER (OFVA_MED_STRUT=48)
        INTEGER OFVA_MED_ENDPL          !tracking medium#, barrel endplates
        PARAMETER (OFVA_MED_ENDPL=49)
        INTEGER OFVA_MED_HE             !tracking medium#, empty spaces
        PARAMETER (OFVA_MED_HE=50)
        INTEGER OFVA_R1_PAD             !inner radius of pad detectors
        PARAMETER (OFVA_R1_PAD=51)
        INTEGER OFVA_R2_PAD             !outer radius of pad detectors
        PARAMETER (OFVA_R2_PAD=52)
        INTEGER OFVA_Z_PAD              !z position of pad detectors
        PARAMETER (OFVA_Z_PAD=53)
        INTEGER OFVA_R_MOPL             !outer radius of pad mounting plate
        PARAMETER (OFVA_R_MOPL=54)
        INTEGER OFVA_MED_MOPL           !tracking medium#, pad mounting plate
        PARAMETER (OFVA_MED_MOPL=55)
        INTEGER OFVA_DZ_MOPL            !1/2 thickness, pad mounting plate
        PARAMETER (OFVA_DZ_MOPL=56)
        INTEGER OFVA_Z_MOPL             !z of pad mounting plate
        PARAMETER (OFVA_Z_MOPL=57)
        INTEGER OFVA_Z_MVD              !z of MVD center
        PARAMETER (OFVA_Z_MVD=58)
C
        INTEGER OFVA_EL_SPACE           !MCM spacing
        PARAMETER (OFVA_EL_SPACE=59)
        INTEGER OFVA_TH_BUS             !thickness of bus cable (MCM to external world)
        PARAMETER (OFVA_TH_BUS=60)
        INTEGER OFVA_W_BUS              !width of bus cable
        PARAMETER (OFVA_W_BUS=61)
        INTEGER OFVA_PLNM_THWALL        !thickness of plenum walls
        PARAMETER (OFVA_PLNM_THWALL=62)
        INTEGER OFVA_R_PLENA            !distance of plenum from beamline
        PARAMETER (OFVA_R_PLENA=63)
        INTEGER OFVA_ANG_PLENA          !tilt angle of plenum
        PARAMETER (OFVA_ANG_PLENA=64)
        INTEGER OFVA_TH_CABL            !thickness of cable from Si to MCM
        PARAMETER (OFVA_TH_CABL=65)
        INTEGER OFVA_MED_BUS            !tracking medium for bus cable
        PARAMETER (OFVA_MED_BUS=66)
        INTEGER OFVA_MED_CABL           !tracking medium for cable from Si to MCM
        PARAMETER (OFVA_MED_CABL=67)
        INTEGER OFVA_MED_DUMMY          !tracking medium for dummy MCMs
        PARAMETER (OFVA_MED_DUMMY=68)
        INTEGER OFVA_N_CAGES            !number of cages
        PARAMETER (OFVA_N_CAGES=69)
        INTEGER OFVA_CAGE_TYPE          !first of 24 parameters specifying
        PARAMETER (OFVA_CAGE_TYPE=70)   !number of MCMs per cage, last element
                                        !is 70+24-1=93
        INTEGER OFVA_PAD_INSTALL_FLAG 
        PARAMETER (OFVA_PAD_INSTALL_FLAG=94)  !2 elements, 94 and 95
C
C -------------------------------------------------------------------------
C --- structure of a mapped raw data bank      bank ID: VMAP
C
c       LFV_MAP
c       |
c       V
c       0 MUL, (wafer, dev, xyz entrance, xyz exit), ...,
c                               wafer number
c                               energy loss in detector
c                               track number
C                               x,y,z of entrance hit
C                               x,y,z of exit hit
c
      INTEGER*4 MFV_MAP
      INTEGER*4 OFVM_WAFER,OFVM_DEV,  OFVM_NTRK
      INTEGER*4 OFVM_XIN,  OFVM_YIN,  OFVM_ZIN
      INTEGER*4 OFVM_XOUT, OFVM_YOUT, OFVM_ZOUT
C
C --- offsets
C
      PARAMETER ( MFV_MAP    = 9 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( OFVM_WAFER = 0 )  ! Offset overall wafer address
      PARAMETER ( OFVM_DEV   = 1 )  ! Offset energy loss
      PARAMETER ( OFVM_NTRK  = 2 )  ! Offset track number
      PARAMETER ( OFVM_XIN   = 3 )  ! x of entrance hit, local coordinates
      PARAMETER ( OFVM_YIN   = 4 )  ! y of entrance hit, local coordinates
      PARAMETER ( OFVM_ZIN   = 5 )  ! z of entrance hit, local coordinates
      PARAMETER ( OFVM_XOUT  = 6 )  ! x of exit hit, local coordinates
      PARAMETER ( OFVM_YOUT  = 7 )  ! y of exit hit, local coordinates
      PARAMETER ( OFVM_ZOUT  = 8 )  ! z of exit hit, local coordinates
C ---------------------------------------------------------------------------
C --- structure of a calibrated data bank       bank ID: VCAL
C
c       LFV_CAL
c       |
c       V
c       0 MUL, (wafer, dev, xyz entrance, xyz exit), ...,
c                               wafer number
c                               energy loss in vertex (calibrated in mip ?)
c                               track number
C                               x,y,z of entrance hit
C                               x,y,z of exit hit
c
      INTEGER*4 MFV_CAL
      INTEGER*4 OFVC_WAFER,OFVC_DEV,  OFVC_NTRK
      INTEGER*4 OFVC_XIN,  OFVC_YIN,  OFVC_ZIN
      INTEGER*4 OFVC_XOUT, OFVC_YOUT, OFVC_ZOUT
C
C --- offsets
C
      PARAMETER ( MFV_CAL    = 9 )  ! Size of 1 entry in calibrated data bank
      PARAMETER ( OFVC_WAFER = 0 )  ! Offset overall wafer address
      PARAMETER ( OFVC_DEV   = 1 )  ! Offset energy loss
      PARAMETER ( OFVC_NTRK  = 2 )  ! Offset track number
      PARAMETER ( OFVC_XIN   = 3 )  ! x of entrance hit, local coordinates
      PARAMETER ( OFVC_YIN   = 4 )  ! y of entrance hit, local coordinates
      PARAMETER ( OFVC_ZIN   = 5 )  ! z of entrance hit, local coordinates
      PARAMETER ( OFVC_XOUT  = 6 )  ! x of exit hit, local coordinates
      PARAMETER ( OFVC_YOUT  = 7 )  ! y of exit hit, local coordinates
      PARAMETER ( OFVC_ZOUT  = 8 )  ! z of exit hit, local coordinates
C
C --- end of FPVLINK.inc --------------------------------------------------
C
