C
C --- FPELINK_SPEC.inc ------------------------------------------------------
C
c     CFM: new digitization scheme (unpacked)
c
      INTEGER*4 mFE_Cal_spec
       parameter (mfe_cal_spec = 15)
       integer ofec_spec_i1         ! counter number
       parameter (ofec_spec_i1 = 0)
       integer ofec_spec_iwall      ! wall number
       parameter (ofec_spec_iwall = 1)
       integer ofec_spec_itype      ! detector type
       parameter (ofec_spec_itype = 2)
       integer ofec_spec_imedium    ! detector medium
       parameter (ofec_spec_imedium = 3)
       integer ofec_spec_dele       ! energy loss
       parameter (ofec_spec_dele = 4)
       integer ofec_spec_pos_x     !  x position
       parameter (ofec_spec_pos_x = 5)
       integer ofec_spec_pos_y     !  y position
       parameter (ofec_spec_pos_y = 6)
       integer ofec_spec_pos_z     ! z position
       parameter (ofec_spec_pos_z = 7)
       integer ofec_spec_tof     !  tof
       parameter (ofec_spec_tof = 8)
       integer ofec_spec_ind1      ! supermodule index pointer
       parameter (ofec_spec_ind1 = 9)
       integer ofec_spec_ind2      ! cell in supermodule index pointer
       parameter (ofec_spec_ind2 = 10)
       integer ofec_spec_numed      ! medium value from GMEDIA call
       parameter (ofec_spec_numed = 11)
       integer ofec_spec_id         ! incident particle ID
       parameter (ofec_spec_id = 12)
       integer ofec_spec_nc         ! number of ancestry cycles
       parameter (ofec_spec_nc = 13)
       integer ofec_spec_tr         ! track number of incident particle
       parameter (ofec_spec_tr = 14)

C
c     CFM: new digitization scheme (packed into four words)
c
      iNTEGER*4 mFE_Cal_specp
      parameter (mfe_cal_specp = 4)

C --- end of FPELINK_SPEC.inc ----------------------------------------------
C



