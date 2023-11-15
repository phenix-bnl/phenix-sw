C
C --- FPQLINK.inc ---------------------------------------------------------
C
       common  /FQLINK/
     #  lFQ_link,         ! start of structural links
     #  lFQ_NTC(100,1),
     #  lFQ_PARA, LFQ_PARU,     !support parameter bank
     #  lFQ_lref,         !                 start of reference links
     #  lFQ_last          !                 last link ptr.
      INTEGER*4 lFQ_NTC, LFQ_PARA, LFQ_PARU, lFQ_link, LFQ_LREF,
     &   lFQ_last
C -----------------------------------------------------------------------------
C -------  Topology
C
C
C --- structure of a mapped raw data bank      bank ID: QMAP
C
c       lFQ_NTC
c       |
c       V
c       0 MUL, (x,y,z,deltaE,px,idpart,track,quadrant,py,pz,pathLength,TOF), ...
c
      INTEGER*4 mFQ_NTC
      INTEGER*4 OFQM_x, OFQM_y, OFQM_z, OFQM_partl, OFQM_DEL,
     &   OFQM_ITRA, OFQM_QUADRANT, OFQM_PX, OFQM_PY, OFQM_PZ,
     &   OFQM_LENG, OFQM_TOF
C
C --- offsets
C
      PARAMETER ( mFQ_NTC = 12 )  ! Size of 1 entry in mapped raw data bank
      PARAMETER ( oFQM_x = 0) ! Offset x position.
      PARAMETER ( oFQM_y = 1) ! Offset y position.
      PARAMETER ( oFQM_z = 2) ! Offset z position.
      PARAMETER ( oFQM_DEL  = 3) ! ENERGY LOSS in si
      PARAMETER ( oFQM_PX = 4) ! Particle momentum X component
      PARAMETER ( oFQM_partl = 5) ! Particle ID
      PARAMETER ( oFQM_ITRA  = 6 )! track #
      PARAMETER ( oFQM_QUADRANT = 7) ! which quadrant
      PARAMETER ( oFQM_PY = 8) ! Particle momentum Y component
      PARAMETER ( oFQM_PZ = 9) ! Particle momentum Z component
      PARAMETER ( oFQM_LENG = 10) ! pathLength to the NTC
      PARAMETER ( oFQM_TOF = 11) ! Time-of-Flight to the NTC
C
C --- end of FPQLINK.inc --------------------------------------------------
