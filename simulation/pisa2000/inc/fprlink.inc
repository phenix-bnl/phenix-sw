C
C --- FPRLINK.inc ---------------------------------------------------------
C
       common  /FRLINK/
     #  lFR_link,         ! start of structural links
*CAA Number of sensitive volumes:
*CAA 576 = 560 TPC padrows + 8 HBD sensitive gas volume sectors + 8 HBD CsI sectors
     #  lFR_HBD(576,1),
     #  lFR_PARA, LFR_PARU,     !support parameter bank
     #  lFR_lref,         !                 start of reference links
     #  lFR_last          !                 last link ptr.
      INTEGER*4 lFR_HBD, LFR_PARA, LFR_PARU, lFR_link, LFR_LREF,
     &   lFR_last
C -----------------------------------------------------------------------------
C -------  Topology
C
C
C --- structure of a mapped raw data bank      bank ID: HMAP
C
c       lFR_HBD
c       |
c       V
c       0 MUL, (x,y,z,deltaE,px,idpart,track,ilayer,py,pz,pathLength,TOF), ...
c
      INTEGER*4 mFR_HBD
      INTEGER*4 OFRM_Xin, OFRM_Yin, OFRM_Zin, OFRM_PX, OFRM_PY,
     &  	OFRM_PZ, OFRM_TOF, OFRM_P_ID, OFRM_TRCK, OFRM_Xout,
     &          OFRM_Yout, OFRM_Zout, OFRM_DELE, OFRM_DSTP,
     &          OFRM_NDET, OFRM_NSECT, OFRM_NPR, OFRM_DETFLAG

C
C --- offsets in the zebra bank for hit components in TPC 
C
      PARAMETER ( mFR_HBD = 18 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( ofrm_Xin = 0)    ! TPC/HBDgas/HBDCsI x entry position
      PARAMETER ( ofrm_Yin = 1)    ! TPC/HBDgas/HBDCsI y entry position
      PARAMETER ( ofrm_Zin = 2)    ! TPC/HBDgas/HBDCsI z entry position
      PARAMETER ( ofrm_PX = 3)    ! TPC/HBDgas/HBDCsI x momentum
      PARAMETER ( ofrm_PY = 4)    ! TPC/HBDgas/HBDCsI y momentum 
      PARAMETER ( ofrm_PZ = 5)    ! TPC/HBDgas/HBDCsI z momentum
      PARAMETER ( ofrm_TOF = 6)   ! TPC/HBDgas/HBDCsI Time-of-flight
      PARAMETER ( ofrm_P_ID = 7)     ! Particle ID
      PARAMETER ( ofrm_TRCK = 8)     ! Track number
      PARAMETER ( ofrm_Xout = 9)    ! TPC/HBDgas x exit position
      PARAMETER ( ofrm_Yout = 10)   ! TPC/HBDgas y exit position
      PARAMETER ( ofrm_Zout = 11)   ! TPC/HBDgas z exit position
      PARAMETER ( ofrm_DELE = 12) ! TPC Energy loss
      PARAMETER ( ofrm_DSTP = 13) ! TPC Path length
      PARAMETER ( ofrm_NDET = 14) ! TPC Detector # (1 or 2 for south or north)
      PARAMETER ( ofrm_NSECT = 15)! TPC/HBDgas/HBDCsI Sector # (1-8)
      PARAMETER ( ofrm_NPR = 16)  ! TPC Pad row # (1-35)
      PARAMETER ( ofrm_DETFLAG = 17)  ! Flag indicating TPC (1), HBDgas (2) , or HBDCsi (3)


C
C --- end of FPHLINK.inc --------------------------------------------------


