*CMZ :          31/01/95  10.15.31  by  Ajit Mohanty
*CMZU:  2.02/02 28/07/93  08.42.29  by  S R Tonse
*-- Author :    Toru Sugitate   26/07/93
C---- FPBLINK.inc ------------------------------------------------------
C
c       Revised Jul 26, 1993 by Toru Sugitate for use with PISORP from
c       FBLINK.
c
c       Revised Mar. 23, 1998 by C.F. Maguire to add track-in-subevent
c                             number to PISA hits output.  Also changes
c                             in bbc_hits.f routine for this reason.
C
C-----------------------------------------------------------------------
C
C  Should optimize the size of IFP_Map and IFB_Cal.
C
C  Sequence MAXLIMS used here in order to have max # of sub-events
C  specified in only one place SRTonse
C----------------------------------------------------------------------
C  History
C       23Nov94 Toru  The geometry bank PARA modifiled.
C
C----------------------------------------------------------------------
      common   /PBLINK/
     #   lFB_link,              ! start of structural links
     #   lFB_Map(MAXSUB),          ! Mapped (contiguous) raw data
     #   lFB_Cal(MAXSUB),          ! Calibrated data
     #   lFB_PARA,
     #   lFB_PARU,
     #   lFB_SpareLinks(7),
     #   lFB_lref,              ! start of reference links
     #   lFB_SpareRefs(5),
     #   lFB_last               ! last link ptr.
C
      INTEGER*4 lFB_link
      INTEGER*4 lFB_Map, lFB_Cal
      INTEGER*4 lFB_PARA, lFB_PARU
      INTEGER*4 lFB_lref, lFB_last
      INTEGER*4 lFB_SpareLinks, lFB_SpareRefs
C -----------------------------------------------------------------------------
C -------  Topology
C
      INTEGER*4  mFB_ALLDETS
C     PARAMETER( mFB_ALLDETS = 132)  ! max # of BBC detectors
      PARAMETER( mFB_ALLDETS = 200)  ! max # of BBC detectors
C
C --- structure of a topological data bank entry
C
c     lFB_Top
c     |
c     V
C -------------------------------------------------------------------------
C --- structure of the Beam-Beam Counter geometry bank        bank ID: PARA
C
      INTEGER*4  BBC_PARA_ND
      PARAMETER (BBC_PARA_ND = 49)
      INTEGER*4  OFBA_COL
      PARAMETER (OFBA_COL =  0 )
      INTEGER*4  OFBA_SEE
      PARAMETER (OFBA_SEE =  1 )
      INTEGER*4  OFBA_MAB
      PARAMETER (OFBA_MAB =  2 )
      INTEGER*4  OFBA_MAT
      PARAMETER (OFBA_MAT =  3 )
      INTEGER*4  OFBA_MBA
      PARAMETER (OFBA_MBA =  4 )
      INTEGER*4  OFBA_MCO
      PARAMETER (OFBA_MCO =  5 )
      INTEGER*4  OFBA_MFR
      PARAMETER (OFBA_MFR =  6 )
      INTEGER*4  OFBA_MMO
      PARAMETER (OFBA_MMO =  7 )
      INTEGER*4  OFBA_MPM
      PARAMETER (OFBA_MPM =  8 )
      INTEGER*4  OFBA_MQU
      PARAMETER (OFBA_MQU =  9 )
      INTEGER*4  OFBA_MST
      PARAMETER (OFBA_MST = 10 )
 
      INTEGER*4  OFBA_ABS
      PARAMETER (OFBA_ABS = 11)
      INTEGER*4  OFBA_ATT
      PARAMETER (OFBA_ATT = 14)
      INTEGER*4  OFBA_BAC
      PARAMETER (OFBA_BAC = 24)
      INTEGER*4  OFBA_COV
      PARAMETER (OFBA_COV = 27)
      INTEGER*4  OFBA_FRO
      PARAMETER (OFBA_FRO = 28)
      INTEGER*4  OFBA_PMT
      PARAMETER (OFBA_PMT = 31)
      INTEGER*4  OFBA_QUA
      PARAMETER (OFBA_QUA = 34)
      INTEGER*4  OFBA_SPA
      PARAMETER (OFBA_SPA = 44)
      INTEGER*4  OFBA_STR
      PARAMETER (OFBA_STR = 45)
      INTEGER*4  OFBA_ZPO
      PARAMETER (OFBA_ZPO = 48)
c
C -------------------------------------------------------------------------
C --- structure of a mapped raw data bank                     bank ID: BMAP
C
c     lFB_Map
c     |
c     V
c     0 MUL, (PMT#,X,Y,Z,DELE,TOF,PID,PX,PY,PZ,PLEN),...
c                   PMT number in each detector.
c                   X,Y,Z position.
c                   Energy loss
c                   TOF
c                   Particle ID of hit
c                   Momentum of particle
c                   Path length
c
      INTEGER*4 mFB_Map
      INTEGER   oFBM_PMT
      INTEGER   oFBM_X
      INTEGER   oFBM_Y
      INTEGER   oFBM_Z
      INTEGER   oFBM_DEL
      INTEGER   oFBM_TOF
      INTEGER   oFBM_PID
      INTEGER   oFBM_PX
      INTEGER   oFBM_PY
      INTEGER   oFBM_PZ
      INTEGER   oFBM_LEN
      INTEGER   oFBM_TRAK
 
C
C --- offsets
C
      PARAMETER( mFB_Map   = 12)
      PARAMETER( oFBM_PMT  =  0)        ! Offset PMT #
      PARAMETER( oFBM_X    =  1)        ! Offset X position
      PARAMETER( oFBM_Y    =  2)        ! Offset Y position
      PARAMETER( oFBM_Z    =  3)        ! Offset Z Position
      PARAMETER( oFBM_DEL  =  4)        ! Offset energy loss
      PARAMETER( oFBM_TOF  =  5)        ! Offset TOF value
      PARAMETER( oFBM_PID  =  6)        ! Offset particle ID of hit
      PARAMETER( oFBM_PX   =  7)        ! Offset Px of particle
      PARAMETER( oFBM_PY   =  8)        ! Offset Py of particle
      PARAMETER( oFBM_PZ   =  9)        ! Offset Pz of particle
      PARAMETER( oFBM_LEN  = 10)        ! Offset path length of hit
      PARAMETER( oFBM_TRAK = 11)        ! Offset for track number of hit
 
C ---------------------------------------------------------------------------
C --- structure of a calibrated data bank                       bank ID: BCAL
C     Temporalily same as the MCAL bank for now. Toru
c
c     lFB_Cal
c     |
c     V
c     0 MUL, (PMT#,X,Y,Z,DELE,TOF,PID,PX,PY,PZ,PLEN),...
c                   PMT number in each detector.
c                   X,Y,Z position.
c                   Energy loss
c                   TOF
c                   Particle ID of hit
c                   Momentum of particle
c                   Path length
c
      INTEGER*4 mFB_Cal
      INTEGER   oFBC_PMT
      INTEGER   oFBC_X
      INTEGER   oFBC_Y
      INTEGER   oFBC_Z
      INTEGER   oFBC_DEL
      INTEGER   oFBC_TOF
      INTEGER   oFBC_PID
      INTEGER   oFBC_PX
      INTEGER   oFBC_PY
      INTEGER   oFBC_PZ
      INTEGER   oFBC_LEN
      INTEGER   oFBC_TRAK
 
C
C --- offsets
C
      PARAMETER( mFB_Cal   = 12)
      PARAMETER( oFBC_PMT  =  0)        ! Offset PMT #
      PARAMETER( oFBC_X    =  1)        ! Offset X position
      PARAMETER( oFBC_Y    =  2)        ! Offset Y position
      PARAMETER( oFBC_Z    =  3)        ! Offset Z Position
      PARAMETER( oFBC_DEL  =  4)        ! Offset energy loss
      PARAMETER( oFBC_TOF  =  5)        ! Offset TOF value
      PARAMETER( oFBC_PID  =  6)        ! Offset particle ID of hit
      PARAMETER( oFBC_PX   =  7)        ! Offset Px of particle
      PARAMETER( oFBC_PY   =  8)        ! Offset Py of particle
      PARAMETER( oFBC_PZ   =  9)        ! Offset Pz of particle
      PARAMETER( oFBC_LEN  = 10)        ! Offset path length of hit
      PARAMETER( oFBC_TRAK = 11)        ! Offset for track number of hit
C
C --- end of FPBLINK.inc --------------------------------------------------
C
