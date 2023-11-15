C --- FPDLINK.inc ---------------------------------------------------------
C     29-JUL-1992 changed this for a silicon vertex device SRTonse
C     16-Dec-1994 changed name of file to FPDLINK, CFM ("D" for Dalitz rejection study)
C     28-Jun-2001 changed lFD_SIL to have (100,1)  CFM: no more subevent business for PISORP)
C     03-Sep-2003 V. L. Rykov: Extended hit set
C     26-Jul-2006 Hubert van Hecke: extended hit set by 6
C
       common  /FDLINK/
     #  lFD_link,         ! start of structural links
     #  lFD_SIL(100,1),
     #  lFD_PARA, LFD_PARU,     !support parameter bank
     #  lFD_lref,         !                 start of reference links
     #  lFD_last          !                 last link ptr.


      INTEGER*4 lFD_SIL, LFD_PARA, LFD_PARU, lFD_link, LFD_LREF,
     &   lFD_last


C -----------------------------------------------------------------------------
C -------  Topology
C
      INTEGER*4  mFD_ALLDETS
C     PARAMETER( mFD_ALLDETS = 500)  ! max # of INNER detectors
C
C --- structure of a mapped raw data bank      bank ID: DMAP
C
c       lFD_SIL
c       |
c       V
c       0 MUL, (x,y,z,deltaE,px,idpart,track,ilayer,py,pz), ...
c
      INTEGER*4 mFD_SIL
      INTEGER*4 OFDM_x, OFDM_y, OFDM_z, OFDM_partl, OFDM_DEL
     &	, OFDM_ITRA, OFDM_LAYER, OFDM_PX, OFDM_PY, OFDM_PZ
Corrected by V. L. Rykov 09/03/2003
      Integer oFDM_tf, oFDM_xi, oFDM_yi, oFDM_zi
     &	, oFDM_xo, oFDM_yo, oFDM_zo, oFDM_vol,
     & oFDM_xgi, oFDM_ygi, oFDM_zgi, oFDM_xgo, oFDM_ygo, oFDM_zgo
C
C --- offsets
C
Corrected by V. L. Rykov 09/03/2003
*      PARAMETER ( mFD_SIL = 10 )     ! Size of 1 entry in mapped raw data bank
*      PARAMETER ( mFD_SIL    = 23 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( mFD_SIL    = 32 )  ! Size of 1 entry in mapped raw data bank (6 more) +3 vols
      PARAMETER ( oFDM_x     = 0)    ! Offset x position.
      PARAMETER ( oFDM_y     = 1)    ! Offset y position.
      PARAMETER ( oFDM_z     = 2)    ! Offset z position.
      PARAMETER ( oFDM_DEL   = 3)    ! ENERGY LOSS in si
      PARAMETER ( oFDM_PX    = 4)    ! Particle momentum X component
      PARAMETER ( oFDM_partl = 5)    ! Particle ID
      PARAMETER ( oFDM_ITRA  = 6)    ! track #
      PARAMETER ( oFDM_LAYER = 7)    ! which Silicon layer 
      PARAMETER ( oFDM_PY    = 8)    ! Particle momentum Y component
      PARAMETER ( oFDM_PZ    = 9)    ! Particle momentum Z component

Corrected by V. L. Rykov 09/03/2003
      PARAMETER ( oFDM_tf    = 10)   ! Time-of-flight
      PARAMETER ( oFDM_xi    = 11)   ! Local x-entry
      PARAMETER ( oFDM_yi    = 12)   ! Local y-entry
      PARAMETER ( oFDM_zi    = 13)   ! Local z-entry
      PARAMETER ( oFDM_xo    = 14)   ! Local x-exit
      PARAMETER ( oFDM_yo    = 15)   ! Local y-exit
      PARAMETER ( oFDM_zo    = 16)   ! Local z-exit

* Added global in/out coordinates July 2006 Hubert van Hecke:
      PARAMETER ( oFDM_xgi   = 17)   ! Global x-entry
      PARAMETER ( oFDM_ygi   = 18)   ! Global y-entry
      PARAMETER ( oFDM_zgi   = 19)   ! Global z-entry
      PARAMETER ( oFDM_xgo   = 20)   ! Global x-exit
      PARAMETER ( oFDM_ygo   = 21)   ! Global y-exit
      PARAMETER ( oFDM_zgo   = 22)   ! Global z-exit

      PARAMETER ( oFDM_vol   = 23)   ! (z-momentum at exit) vol copy number
C                                    ! 23-32
C --- end of FPDLINK.inc --------------------------------------------------
