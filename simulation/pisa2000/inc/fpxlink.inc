C
C --- FPXLINK.inc ---------------------------------------------------------
c     CFM proposition for PHNX  X ZEBRA banks  May 29, 1998
c
 
      integer lfx_link, lfx_kine(maxsub), lfx_last, lfx_lref
 
      common   /FXLINK/
     #   lfx_link,         ! start of structural links
     #   lfx_kine,         ! restricted kinematics/vertex information
     #   lfx_lref,         ! reference link
     #   lfx_last          ! last link ptr.
 
C
C --- offsets
C
      INTEGER  mfx_kine ! entry size
      integer OFSX_TR   ! track number in event input file
      integer OFSX_ID   ! particle id number
      integer OFSX_PX   ! X component of momentum
      integer OFSX_PY   ! Y component of momentum
      integer OFSX_PZ   ! Z component of momentum
      integer OFSX_TRSUB   ! track number in subevent
 
      parameter ( mfx_kine = 6 )  ! Size of 1 entry in the data bank
      parameter ( ofsx_tr = 1)
      parameter ( ofsx_id = 2)
      parameter ( ofsx_px = 3)
      parameter ( ofsx_py = 4)
      parameter ( ofsx_pz = 5)
      parameter ( ofsx_trsub = 6)
c
C
C --- end of FPXLINK.inc --------------------------------------------------
C
