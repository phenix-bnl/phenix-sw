C
C --- FPALINK.inc ---------------------------------------------------------
C
       common  /FALINK/
     #  lFA_link,         ! start of structural links
     #  lFA_AER(200,1),
     #  lFA_PARA, LFA_PARU,     !support parameter bank
     #  lFA_lref,         !                 start of reference links
     #  lFA_last          !                 last link ptr.
      INTEGER*4 lFA_AER, LFA_PARA, LFA_PARU, lFA_link, LFA_LREF,
     &   lFA_last
C -----------------------------------------------------------------------------
C -------  Topology
C
C
C --- structure of a mapped raw data bank      bank ID: AMAP
C
c       lFA_AER
c       |
c       V
c       0 MUL, (x,y,z,deltaE,px,idpart,track,quadrant,py,pz,pathLength,TOF), ...
c
      INTEGER*4 mFA_AER
      INTEGER*4 OFAM_x, OFAM_y, OFAM_z, OFAM_partl, OFAM_DEL,
     &   OFAM_ITRA, OFAM_LAYER, OFAM_PX, OFAM_PY, OFAM_PZ,
     &   OFAM_LENG, OFAM_TOF, OFAM_STEP, OFAM_ETOT, OFAM_CHARGE,
     &   OFAM_MOMENTUM, OFAM_VERTX, OFAM_VERTY, OFAM_VERTZ
C
C --- offsets
C
      PARAMETER ( mFA_AER = 19 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( oFAM_x = 0) ! Offset x position.
      PARAMETER ( oFAM_y = 1) ! Offset y position.
      PARAMETER ( oFAM_z = 2) ! Offset z position.
      PARAMETER ( oFAM_DEL  = 3) ! ENERGY LOSS in AER
      PARAMETER ( oFAM_PX = 4) ! Particle momentum X component
      PARAMETER ( oFAM_partl = 5) ! Particle ID
      PARAMETER ( oFAM_ITRA  = 6 )! track #
      PARAMETER ( oFAM_LAYER = 7) ! which layer
      PARAMETER ( oFAM_PY = 8) ! Particle momentum Y component
      PARAMETER ( oFAM_PZ = 9) ! Particle momentum Z component
      PARAMETER ( oFAM_LENG = 10) ! pathLength to the AER
      PARAMETER ( oFAM_TOF = 11) ! Time-of-Flight to the AER
      PARAMETER ( oFAM_STEP = 12) ! 1 step length
      PARAMETER ( oFAM_ETOT = 13) ! Total energy of particle
      PARAMETER ( oFAM_CHARGE = 14) ! charge of particle
      PARAMETER ( oFAM_MOMENTUM = 15) ! charge of particle
      PARAMETER ( oFAM_VERTX = 16) !
      PARAMETER ( oFAM_VERTY = 17) !
      PARAMETER ( oFAM_VERTZ = 18) !
C
C --- end of FPALINK.inc --------------------------------------------------


