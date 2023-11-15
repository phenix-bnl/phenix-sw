C
C --- FPHLINK.inc ---------------------------------------------------------
C
       common  /FHLINK/
     #  lFH_link,         ! start of structural links
*CAA Number of sensitive volumes:
*CAA 576 = 560 TPC padrows + 8 HBD sensitive gas volume sectors + 8 HBD CsI sectors
     #  lFH_HBD(576,1),
     #  lFH_PARA, LFH_PARU,     !support parameter bank
     #  lFH_lref,         !                 start of reference links
     #  lFH_last          !                 last link ptr.
      INTEGER*4 lFH_HBD, LFH_PARA, LFH_PARU, lFH_link, LFH_LREF,
     &   lFH_last
C -----------------------------------------------------------------------------
C -------  Topology
C
C
C --- structure of a mapped raw data bank      bank ID: HMAP
C
c       lFH_HBD
c       |
c       V
c       0 MUL, (x,y,z,deltaE,px,idpart,track,ilayer,py,pz,pathLength,TOF), ...
c
      INTEGER*4 mFH_HBD
      INTEGER*4 OFHM_Xin, OFHM_Yin, OFHM_Zin, OFHM_PX, OFHM_PY,
     &  	OFHM_PZ, OFHM_TOF, OFHM_P_ID, OFHM_TRCK, OFHM_Xout,
     &          OFHM_Yout, OFHM_Zout, OFHM_DELE, OFHM_DSTP,
     &          OFHM_NDET, OFHM_NSECT, OFHM_NPR, OFHM_DETFLAG

C
C --- offsets in the zebra bank for hit components in TPC 
C
      PARAMETER ( mFH_HBD = 18 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( ofhm_Xin = 0)    ! TPC/HBDgas/HBDCsI x entry position
      PARAMETER ( ofhm_Yin = 1)    ! TPC/HBDgas/HBDCsI y entry position
      PARAMETER ( ofhm_Zin = 2)    ! TPC/HBDgas/HBDCsI z entry position
      PARAMETER ( ofhm_PX = 3)    ! TPC/HBDgas/HBDCsI x momentum
      PARAMETER ( ofhm_PY = 4)    ! TPC/HBDgas/HBDCsI y momentum 
      PARAMETER ( ofhm_PZ = 5)    ! TPC/HBDgas/HBDCsI z momentum
      PARAMETER ( ofhm_TOF = 6)   ! TPC/HBDgas/HBDCsI Time-of-flight
      PARAMETER ( ofhm_P_ID = 7)     ! Particle ID
      PARAMETER ( ofhm_TRCK = 8)     ! Track number
      PARAMETER ( ofhm_Xout = 9)    ! TPC/HBDgas x exit position
      PARAMETER ( ofhm_Yout = 10)   ! TPC/HBDgas y exit position
      PARAMETER ( ofhm_Zout = 11)   ! TPC/HBDgas z exit position
      PARAMETER ( ofhm_DELE = 12) ! TPC Energy loss
      PARAMETER ( ofhm_DSTP = 13) ! TPC Path length
      PARAMETER ( ofhm_NDET = 14) ! TPC Detector # (1 or 2 for south or north)
      PARAMETER ( ofhm_NSECT = 15)! TPC/HBDgas/HBDCsI Sector # (1-8)
      PARAMETER ( ofhm_NPR = 16)  ! TPC Pad row # (1-35)
      PARAMETER ( ofhm_DETFLAG = 17)  ! Flag indicating TPC (1), HBDgas (2) , or HBDCsi (3)


C
C --- end of FPHLINK.inc --------------------------------------------------


