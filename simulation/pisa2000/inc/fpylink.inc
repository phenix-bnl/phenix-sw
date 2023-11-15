C --- FPYLINK.inc ---------------------------------------------------------
C     January 25, 2003: initial version for FCL forward calorimeter

       common  /FYLINK/
     #  lFY_link,         ! start of structural links
     #  lFY_FCL(100,1),
     #  lFY_PARA, LFY_PARU,     !support parameter bank
     #  lFY_lref,         !                 start of reference links
     #  lFY_last          !                 last link ptr.
      INTEGER*4 lFY_FCL, LFY_PARA, LFY_PARU, lFY_link, LFY_LREF,
     &   lFY_last
C -----------------------------------------------------------------------------
C -------  Topology
C
      INTEGER*4  mFY_ALLDETS
      PARAMETER( mFY_ALLDETS = 500)  ! max # of INNER detectors
C
C --- structure of a mapped raw data bank      bank ID: DMAP
C
c       lFY_FCL
c       |
c       V
c       0 MUL, (x,y,z,deltaE,px,idpart,track,ilayer,py,pz), ...
c
      INTEGER*4 mFY_FCL
      INTEGER*4 OFYM_x, OFYM_y, OFYM_z, OFYM_partl, OFYM_DEL,
     &   OFYM_ITRA, OFYM_LAYER, OFYM_PX, OFYM_PY, OFYM_PZ
C
C --- offsets
C
      PARAMETER ( mFY_FCL = 10 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( oFYM_x = 0) ! Offset x position.
      PARAMETER ( oFYM_y = 1) ! Offset y position.
      PARAMETER ( oFYM_z = 2) ! Offset z position.
      PARAMETER ( oFYM_DEL  = 3) ! ENERGY LOSS in FCL
      PARAMETER ( oFYM_PX = 4) ! Particle momentum X component
      PARAMETER ( oFYM_partl = 5) ! Particle ID
      PARAMETER ( oFYM_ITRA  = 6 )! track #
      PARAMETER ( oFYM_LAYER = 7) ! which FCL layer 
      PARAMETER ( oFYM_PY = 8) ! Particle momentum Y component
      PARAMETER ( oFYM_PZ = 9) ! Particle momentum Z component
C
C --- end of FPYLINK.inc --------------------------------------------------
