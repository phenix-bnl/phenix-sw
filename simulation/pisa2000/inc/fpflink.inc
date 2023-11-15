*-- Author :
c     ======================
c     File Name : FFLINK.INC
c     ======================
c
c     Description :
c     =============
c
c     This subroutine contains the TOF include statements.
c
c     Author:-   Tapan Nayak
c     (based on CFM proposition for TOF ZEBRA banks May 7, 1992
c
c
c     Creation Date: 16-Sep-1992
c     ==========================
c
c     Revisions:-
c     =========
c     Date    Name            Description
c     ----    ----            -----------
c    12/4/96  K.Kurita	      Major overhaul to accomodate fixed TOF geometry
c			      design.  Currently 5 panles exist.
c    5/29/00  A.Kiyomichi     Modifiled the geometry bank PARA and PARU.
c
c
c     =========================
c     Start of the include file
c     =========================
      integer*4    lFF_Cal(maxsub)    ! Pointer to the FCAL bank
      integer*4    lFF_link
      integer*4    lFF_lref
      integer*4    lFF_last
      integer*4    lFF_PARA
      integer*4    lFF_PARU
      common       /FFLINK/
     #             lFF_link,           ! start of structural links
     #             lFF_PARA,           ! parameter bank's links
     #             lFF_PARU,
     #             lFF_Cal,            ! Calibrated data
     #             lFF_lref,           ! start of reference links
     #             lFF_last            ! last link ptr.
c
c   Slat information
c
      integer      mFF_pmts            ! number of PMTs per slat
      parameter    (mFF_pmts = 2)
c
c     scintillator parameters
c
      real         scint_vlight           ! light velocity in cm/ns
      parameter    (scint_vlight = 14.0)  ! GSI measured value
      real         scint_lambda           ! light attenuation factor (cm)
      parameter    (scint_lambda=0.0125)  ! GSI number
c
c----> Structure of the parameter back     bank ID : EPRA
c
      integer        ofea_TOFL_rpos
      integer        ofea_TFPN_dimen_x
      integer        ofea_TFPN_dimen_y
      integer        ofea_TFPN_dimen_z
      integer        ofea_TFPN_panel0_0x
      integer        ofea_TFPN_panel0_0y
      integer        ofea_TFPN_panel0_0z
      integer        ofea_TFPN_panel0_1x
      integer        ofea_TFPN_panel0_1y
      integer        ofea_TFPN_panel0_1z
      integer        ofea_TFPN_panel0_2x
      integer        ofea_TFPN_panel0_2y
      integer        ofea_TFPN_panel0_2z
      integer        ofea_TFPN_panel1_0x
      integer        ofea_TFPN_panel1_0y
      integer        ofea_TFPN_panel1_0z
      integer        ofea_TFPN_panel1_1x
      integer        ofea_TFPN_panel1_1y
      integer        ofea_TFPN_panel1_1z
      integer        ofea_TFPN_panel1_2x
      integer        ofea_TFPN_panel1_2y
      integer        ofea_TFPN_panel1_2z
      integer        ofea_TFPN_panel2_0x
      integer        ofea_TFPN_panel2_0y
      integer        ofea_TFPN_panel2_0z
      integer        ofea_TFPN_panel2_1x
      integer        ofea_TFPN_panel2_1y
      integer        ofea_TFPN_panel2_1z
      integer        ofea_TFPN_panel2_2x
      integer        ofea_TFPN_panel2_2y
      integer        ofea_TFPN_panel2_2z
      integer        ofea_TFPN_panel3_0x
      integer        ofea_TFPN_panel3_0y
      integer        ofea_TFPN_panel3_0z
      integer        ofea_TFPN_panel3_1x
      integer        ofea_TFPN_panel3_1y
      integer        ofea_TFPN_panel3_1z
      integer        ofea_TFPN_panel3_2x
      integer        ofea_TFPN_panel3_2y
      integer        ofea_TFPN_panel3_2z
      integer        ofea_TFPN_panel4_0x
      integer        ofea_TFPN_panel4_0y
      integer        ofea_TFPN_panel4_0z
      integer        ofea_TFPN_panel4_1x
      integer        ofea_TFPN_panel4_1y
      integer        ofea_TFPN_panel4_1z
      integer        ofea_TFPN_panel4_2x
      integer        ofea_TFPN_panel4_2y
      integer        ofea_TFPN_panel4_2z
      integer        ofea_TFPN_panel5_0x
      integer        ofea_TFPN_panel5_0y
      integer        ofea_TFPN_panel5_0z
      integer        ofea_TFPN_panel5_1x
      integer        ofea_TFPN_panel5_1y
      integer        ofea_TFPN_panel5_1z
      integer        ofea_TFPN_panel5_2x
      integer        ofea_TFPN_panel5_2y
      integer        ofea_TFPN_panel5_2z
      integer        ofea_TFPN_panel6_0x
      integer        ofea_TFPN_panel6_0y
      integer        ofea_TFPN_panel6_0z
      integer        ofea_TFPN_panel6_1x
      integer        ofea_TFPN_panel6_1y
      integer        ofea_TFPN_panel6_1z
      integer        ofea_TFPN_panel6_2x
      integer        ofea_TFPN_panel6_2y
      integer        ofea_TFPN_panel6_2z
      integer        ofea_TFPN_panel7_0x
      integer        ofea_TFPN_panel7_0y
      integer        ofea_TFPN_panel7_0z
      integer        ofea_TFPN_panel7_1x
      integer        ofea_TFPN_panel7_1y
      integer        ofea_TFPN_panel7_1z
      integer        ofea_TFPN_panel7_2x
      integer        ofea_TFPN_panel7_2y
      integer        ofea_TFPN_panel7_2z
      integer        ofea_TFPN_panel8_0x
      integer        ofea_TFPN_panel8_0y
      integer        ofea_TFPN_panel8_0z
      integer        ofea_TFPN_panel8_1x
      integer        ofea_TFPN_panel8_1y
      integer        ofea_TFPN_panel8_1z
      integer        ofea_TFPN_panel8_2x
      integer        ofea_TFPN_panel8_2y
      integer        ofea_TFPN_panel8_2z
      integer        ofea_TFPN_panel9_0x
      integer        ofea_TFPN_panel9_0y
      integer        ofea_TFPN_panel9_0z
      integer        ofea_TFPN_panel9_1x
      integer        ofea_TFPN_panel9_1y
      integer        ofea_TFPN_panel9_1z
      integer        ofea_TFPN_panel9_2x
      integer        ofea_TFPN_panel9_2y
      integer        ofea_TFPN_panel9_2z
      integer        ofea_TFPN_nslat
      integer        ofea_npnl_sector0
      integer        ofea_npnl_sector1
      integer        ofea_npnl_sector2
      integer        ofea_color_tof
      integer        ofea_med_tof
c
      parameter     (ofea_TOFL_rpos      =  0)
      parameter     (ofea_TFPN_dimen_x   =  1)
      parameter     (ofea_TFPN_dimen_y   =  2)
      parameter     (ofea_TFPN_dimen_z   =  3)
      parameter     (ofea_TFPN_panel0_0x =  4)
      parameter     (ofea_TFPN_panel0_0y =  5)
      parameter     (ofea_TFPN_panel0_0z =  6)
      parameter     (ofea_TFPN_panel0_1x =  7)
      parameter     (ofea_TFPN_panel0_1y =  8)
      parameter     (ofea_TFPN_panel0_1z =  9)
      parameter     (ofea_TFPN_panel0_2x = 10)
      parameter     (ofea_TFPN_panel0_2y = 11)
      parameter     (ofea_TFPN_panel0_2z = 12)
      parameter     (ofea_TFPN_panel1_0x = 13)
      parameter     (ofea_TFPN_panel1_0y = 14)
      parameter     (ofea_TFPN_panel1_0z = 15)
      parameter     (ofea_TFPN_panel1_1x = 16)
      parameter     (ofea_TFPN_panel1_1y = 17)
      parameter     (ofea_TFPN_panel1_1z = 18)
      parameter     (ofea_TFPN_panel1_2x = 19)
      parameter     (ofea_TFPN_panel1_2y = 20)
      parameter     (ofea_TFPN_panel1_2z = 21)
      parameter     (ofea_TFPN_panel2_0x = 22)
      parameter     (ofea_TFPN_panel2_0y = 23)
      parameter     (ofea_TFPN_panel2_0z = 24)
      parameter     (ofea_TFPN_panel2_1x = 25)
      parameter     (ofea_TFPN_panel2_1y = 26)
      parameter     (ofea_TFPN_panel2_1z = 27)
      parameter     (ofea_TFPN_panel2_2x = 28)
      parameter     (ofea_TFPN_panel2_2y = 29)
      parameter     (ofea_TFPN_panel2_2z = 30)
      parameter     (ofea_TFPN_panel3_0x = 31)
      parameter     (ofea_TFPN_panel3_0y = 32)
      parameter     (ofea_TFPN_panel3_0z = 33)
      parameter     (ofea_TFPN_panel3_1x = 34)
      parameter     (ofea_TFPN_panel3_1y = 35)
      parameter     (ofea_TFPN_panel3_1z = 36)
      parameter     (ofea_TFPN_panel3_2x = 37)
      parameter     (ofea_TFPN_panel3_2y = 38)
      parameter     (ofea_TFPN_panel3_2z = 39)
      parameter     (ofea_TFPN_panel4_0x = 40)
      parameter     (ofea_TFPN_panel4_0y = 41)
      parameter     (ofea_TFPN_panel4_0z = 42)
      parameter     (ofea_TFPN_panel4_1x = 43)
      parameter     (ofea_TFPN_panel4_1y = 44)
      parameter     (ofea_TFPN_panel4_1z = 45)
      parameter     (ofea_TFPN_panel4_2x = 46)
      parameter     (ofea_TFPN_panel4_2y = 47)
      parameter     (ofea_TFPN_panel4_2z = 48)
      parameter     (ofea_TFPN_panel5_0x = 49)
      parameter     (ofea_TFPN_panel5_0y = 50)
      parameter     (ofea_TFPN_panel5_0z = 51)
      parameter     (ofea_TFPN_panel5_1x = 52)
      parameter     (ofea_TFPN_panel5_1y = 53)
      parameter     (ofea_TFPN_panel5_1z = 54)
      parameter     (ofea_TFPN_panel5_2x = 55)
      parameter     (ofea_TFPN_panel5_2y = 56)
      parameter     (ofea_TFPN_panel5_2z = 57)
      parameter     (ofea_TFPN_panel6_0x = 58)
      parameter     (ofea_TFPN_panel6_0y = 59)
      parameter     (ofea_TFPN_panel6_0z = 60)
      parameter     (ofea_TFPN_panel6_1x = 61)
      parameter     (ofea_TFPN_panel6_1y = 62)
      parameter     (ofea_TFPN_panel6_1z = 63)
      parameter     (ofea_TFPN_panel6_2x = 64)
      parameter     (ofea_TFPN_panel6_2y = 65)
      parameter     (ofea_TFPN_panel6_2z = 66)
      parameter     (ofea_TFPN_panel7_0x = 67)
      parameter     (ofea_TFPN_panel7_0y = 68)
      parameter     (ofea_TFPN_panel7_0z = 69)
      parameter     (ofea_TFPN_panel7_1x = 70)
      parameter     (ofea_TFPN_panel7_1y = 71)
      parameter     (ofea_TFPN_panel7_1z = 72)
      parameter     (ofea_TFPN_panel7_2x = 73)
      parameter     (ofea_TFPN_panel7_2y = 74)
      parameter     (ofea_TFPN_panel7_2z = 75)
      parameter     (ofea_TFPN_panel8_0x = 76)
      parameter     (ofea_TFPN_panel8_0y = 77)
      parameter     (ofea_TFPN_panel8_0z = 78)
      parameter     (ofea_TFPN_panel8_1x = 79)
      parameter     (ofea_TFPN_panel8_1y = 80)
      parameter     (ofea_TFPN_panel8_1z = 81)
      parameter     (ofea_TFPN_panel8_2x = 82)
      parameter     (ofea_TFPN_panel8_2y = 83)
      parameter     (ofea_TFPN_panel8_2z = 84)
      parameter     (ofea_TFPN_panel9_0x = 85)
      parameter     (ofea_TFPN_panel9_0y = 86)
      parameter     (ofea_TFPN_panel9_0z = 87)
      parameter     (ofea_TFPN_panel9_1x = 88)
      parameter     (ofea_TFPN_panel9_1y = 89)
      parameter     (ofea_TFPN_panel9_1z = 90)
      parameter     (ofea_TFPN_panel9_2x = 91)
      parameter     (ofea_TFPN_panel9_2y = 92)
      parameter     (ofea_TFPN_panel9_2z = 93)
      parameter     (ofea_TFPN_nslat     = 94)
      parameter     (ofea_npnl_sector0   = 95)
      parameter     (ofea_npnl_sector1   = 96)
      parameter     (ofea_npnl_sector2   = 97)
      parameter     (ofea_color_tof      = 98)
      parameter     (ofea_med_tof        = 99)
c
c----> Structure of the user parameter back     bank ID : EPRU
c
      integer       ofea_TFPN_rpos_0
      integer       ofea_TFPN_rpos_1
      integer       ofea_TFPN_rpos_2
      integer       ofea_TFPN_rpos_3
      integer       ofea_TFPN_rpos_4
      integer       ofea_TFPN_rpos_5
      integer       ofea_TFPN_rpos_6
      integer       ofea_TFPN_rpos_7
      integer       ofea_TFPN_rpos_8
      integer       ofea_TFPN_rpos_9
      integer       ofea_TFPN_phi_0
      integer       ofea_TFPN_phi_1
      integer       ofea_TFPN_phi_2
      integer       ofea_TFPN_phi_3
      integer       ofea_TFPN_phi_4
      integer       ofea_TFPN_phi_5
      integer       ofea_TFPN_phi_6
      integer       ofea_TFPN_phi_7
      integer       ofea_TFPN_phi_8
      integer       ofea_TFPN_phi_9
      integer       ofea_TFPN_zpos_0
      integer       ofea_TFPN_zpos_1
      integer       ofea_TFPN_zpos_2
      integer       ofea_TFPN_zpos_3
      integer       ofea_TFPN_zpos_4
      integer       ofea_TFPN_zpos_5
      integer       ofea_TFPN_zpos_6
      integer       ofea_TFPN_zpos_7
      integer       ofea_TFPN_zpos_8
      integer       ofea_TFPN_zpos_9
      integer       ofea_TFSS_dimen_x
      integer       ofea_TFSS_dimen_y
      integer       ofea_TFSS_dimen_z
      integer       ofea_TFSS_length
      integer       ofea_TFLS_dimen_x
      integer       ofea_TFLS_dimen_y
      integer       ofea_TFLS_dimen_z
      integer       ofea_TFLS_length
      integer       ofea_scint_vlight
      integer       ofea_scint_lambda
      integer       ofea_mFF_HADdets
      integer       ofea_mFF_ELEdets
      integer       ofea_mFF_ALLdets
c
      parameter    (ofea_TFPN_rpos_0 = 0)
      parameter    (ofea_TFPN_rpos_1 = 1)
      parameter    (ofea_TFPN_rpos_2 = 2)
      parameter    (ofea_TFPN_rpos_3 = 3)
      parameter    (ofea_TFPN_rpos_4 = 4)
      parameter    (ofea_TFPN_rpos_5 = 5)
      parameter    (ofea_TFPN_rpos_6 = 6)
      parameter    (ofea_TFPN_rpos_7 = 7)
      parameter    (ofea_TFPN_rpos_8 = 8)
      parameter    (ofea_TFPN_rpos_9 = 9)
      parameter    (ofea_TFPN_phi_0  =10)
      parameter    (ofea_TFPN_phi_1  =11)
      parameter    (ofea_TFPN_phi_2  =12)
      parameter    (ofea_TFPN_phi_3  =13)
      parameter    (ofea_TFPN_phi_4  =14)
      parameter    (ofea_TFPN_phi_5  =15)
      parameter    (ofea_TFPN_phi_6  =16)
      parameter    (ofea_TFPN_phi_7  =17)
      parameter    (ofea_TFPN_phi_8  =18)
      parameter    (ofea_TFPN_phi_9  =19)
      parameter    (ofea_TFPN_zpos_0 =20)
      parameter    (ofea_TFPN_zpos_1 =21)
      parameter    (ofea_TFPN_zpos_2 =22)
      parameter    (ofea_TFPN_zpos_3 =23)
      parameter    (ofea_TFPN_zpos_4 =24)
      parameter    (ofea_TFPN_zpos_5 =25)
      parameter    (ofea_TFPN_zpos_6 =26)
      parameter    (ofea_TFPN_zpos_7 =27)
      parameter    (ofea_TFPN_zpos_8 =28)
      parameter    (ofea_TFPN_zpos_9 =29)
      parameter    (ofea_TFSS_dimen_x=30)
      parameter    (ofea_TFSS_dimen_y=31)
      parameter    (ofea_TFSS_dimen_z=32)
      parameter    (ofea_TFSS_length =33)
      parameter    (ofea_TFLS_dimen_x=34)
      parameter    (ofea_TFLS_dimen_y=35)
      parameter    (ofea_TFLS_dimen_z=36)
      parameter    (ofea_TFLS_length =37)
      parameter    (ofea_scint_vlight=38)
      parameter    (ofea_scint_lambda=39)
      parameter    (ofea_mFF_HADdets =40)
      parameter    (ofea_mFF_ELEdets =41)
      parameter    (ofea_mFF_ALLdets =42)
c
c ---------------------------------------------------------------------------
c --- structure of the calibrated data bank       bank ID: FCAL
c
      integer*4   mFF_CAL
      integer     oFFc_slatype
      integer     oFFc_ipanel
      integer     oFFc_icolumn
      integer     oFFc_ipslat
      integer     oFFc_islat
      integer     oFFc_partid
      integer     oFFc_xhall
      integer     oFFc_yhall
      integer     oFFc_zhall
      integer     oFFc_hitpos
      integer     oFFc_pxhall
      integer     oFFc_pyhall
      integer     oFFc_pzhall
      integer     oFFc_tof
      integer     oFFc_dedx
      integer     oFFc_itrack
c
c --- oFFsets
c
      PARAMETER ( mFF_Cal      =16)  ! Size of 1 entry in calibrated data bank
      PARAMETER ( oFFc_slatype = 0)  ! slat type  0:short 1:long
      PARAMETER ( oFFc_ipanel  = 1)  ! Panel number
      PARAMETER ( oFFc_icolumn = 2)  ! Column number in the panel
      PARAMETER ( oFFc_ipslat  = 3)  ! Slat number in the column
      PARAMETER ( oFFc_islat   = 4)  ! Slat number
      PARAMETER ( oFFc_partid  = 5)  ! Offset particle ID of hit
      PARAMETER ( oFFc_xhall   = 6)  ! Offset x pos in hall coordinate
      PARAMETER ( oFFc_yhall   = 7)  ! Offset y pos in hall coordinate
      PARAMETER ( oFFc_zhall   = 8)  ! Offset z pos in hall coordinate
      PARAMETER ( oFFc_hitpos  = 9)  ! Offset hit position in slat coordinate
      PARAMETER ( oFFc_pxhall  =10)  ! Offset px in hall
      PARAMETER ( oFFc_pyhall  =11)  ! Offset py in hall 
      PARAMETER ( oFFc_pzhall  =12)  ! Offset pz in hall
      PARAMETER ( oFFc_tof     =13)  ! Offset tof value
      PARAMETER ( oFFc_dedx    =14)  ! Offset calculated DELE
      PARAMETER ( oFFc_itrack  =15)  ! Offset track number
C --- end of FFLINK.inc --------------------------------------------------
C
