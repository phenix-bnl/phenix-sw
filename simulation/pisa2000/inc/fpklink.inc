*-- Author :
C
C --- FPKLINK.inc ---------------------------------------------------------
c     CFM proposition for PHNX  K  ZEBRA banks August 1, 1994
c         modified to include the parent ID offset and parent ID position
c
 
      integer lfk_link, lfk_kine(maxsub), lfk_last, lfk_lref
 
      common   /FKLINK/
     #   lfk_link,         ! start of structural links
     #   lfk_kine,         ! restricted kinematics/vertex information
     #   lfk_lref,         ! reference link
     #   lfk_last          ! last link ptr.
 
C
C --- offsets
C
      INTEGER  mfk_kine ! entry size
      integer OFSK_TR   ! track number
      integer OFSK_ID   ! particle id number
      integer OFSK_MO   ! momentum at vertex
      integer OFSK_TH   ! theta direction at vertex (from momentum)
      integer OFSK_PH   ! phi directin at vertex (from momentum)
      integer OFSK_RV   ! R position value at vertex
      integer OFSK_ZV   ! Z position value at vertex
      integer OFSK_TV   ! Theta position value at vertex (0.0 for primaries)
      integer OFSK_PV   ! Phi position value at vertex (0.0 for primaries)
      integer OFSK_PN   ! Parent track number if not a primary (0 for primary)
      integer OFSK_PI   ! Parent ID (some event generators will make orphans)
 
      parameter ( mfk_kine = 11 )  ! Size of 1 entry in the data bank
      parameter ( ofsk_tr = 1)
      parameter ( ofsk_id = 2)
      parameter ( ofsk_mo = 3)
      parameter ( ofsk_th = 4)
      parameter ( ofsk_ph = 5)
      parameter ( ofsk_rv = 6)
      parameter ( ofsk_zv = 7)
      parameter ( ofsk_tv = 8)
      parameter ( ofsk_pv = 9)
      parameter ( ofsk_pn = 10)
      parameter ( ofsk_pi = 11)
c
C
C --- end of FPKLINK.inc --------------------------------------------------
C
