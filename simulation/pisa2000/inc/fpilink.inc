c-- Author : Charles F. Maguire
c   New Version July 8, 1995:  Removes tracking output data structures
c                              Retains only PISA data structures
C
C --- FPILINK.inc --------------------------------------------------------
C     28-DEC-1992 N.Sm. Modified for Intermediate Trackers 16*Padchambers
C     + 16*Drift Chamber#1 + 16*Drift Chamber#2
C
c    New version July 19, 1994 is back to 18 sectors
c    Change to MAXSUB limit
c
c    C.F. Maguire   Feb. 14, 1998   Path length and global coordinates in output
      INTEGER*4 lFI_link, lFI_PAD, lfi_dcae, lfi_dcaw, lfI_lref,
     &   lFI_DIG1, lFI_DIG2, lFI_para, lFI_paru, lfi_dcgp, lFI_last

      common   /FILINK/
     #   lFI_link,     	        ! start of structural links
     &   lFI_PAD(18,MAXSUB),    ! to the 18 Pad banks
     &   lfi_dcaw(MAXSUB),      ! for 1996 DC geometry west arm
     &   lfi_dcae(MAXSUB),      ! for 1996 DC geometry east arm
     &   LFI_PARA, LFI_PARU,    ! parameter banks
     &   lfi_dcgp,              ! IT96b parameter bank
     &   LFI_lREF,              ! reference links, unused at present
     #   lFI_last               ! last link ptr.

C
C -----------------------------------------------------------------------
c
c   cfm:  PAD 1 output changed (April 1, 1994) to match PAD 2 and PAD 3
c         Changed to add vertex theta information
c
c   cfm:  New version here for PISA 2.04 July 19, 1994
c         Drop the vertex pointers
c
c   cfm:  PAD1 output changed to have global and path length (Feb. 15, 1998)
c         Same for DC output
c
      INTEGER MFI_PD1, OFIPD1_X1, OFIPD1_Y1, OFIPD1_Z1, OFIPD1_X2,
     &   OFIPD1_Y2, OFIPD1_Z2, OFIPD1_TF, OFIPD1_ID, OFIPD1_DE,
     &   OFIPD1_TR, OFIPD1_XG1, OFIPD1_YG1, OFIPD1_ZG1, OFIPD1_PL
 
      PARAMETER(MFI_PD1=14, OFIPD1_TR=1, OFIPD1_X1=2, OFIPD1_Y1=3,
     &   OFIPD1_Z1=4, OFIPD1_X2=5, OFIPD1_Y2=6, OFIPD1_Z2=7,
     &   OFIPD1_TF=8, OFIPD1_ID=9, OFIPD1_DE=10,
     &   OFIPD1_XG1=11, OFIPD1_YG1=12, OFIPD1_ZG1=13,OFIPD1_PL=14)
 
c
c     cfm additions for global tracking
c
      integer mfi_dch   ! number of words per hit in output bank
      integer ofidc_x1  ! offset for entrance X position (daughter coordinate)
      integer ofidc_y1  ! offset for entrance Y position (daughter coordinate)
      integer ofidc_z1  ! offset for entrance Z position (daughter coordinate)
      integer ofidc_f1  ! offset for entrance time
      integer ofidc_x2  ! offset for entrance X position (daughter coordinate)
      integer ofidc_y2  ! offset for entrance Y position (daughter coordinate)
      integer ofidc_z2  ! offset for entrance Z position (daughter coordinate)
      integer ofidc_f2  ! offset for exit time (new)
      integer ofidc_tr  ! offset for track number
      integer ofidc_nw  ! offset for wire number
      integer ofidc_xg1 ! offset for x global coordinate at entrance
      integer ofidc_yg1 ! offset for y global coordinate at entrance
      integer ofidc_zg1 ! offset for z global coordinate at entrance
      integer ofidc_pl ! offset for path length value 
      integer ofidc_id  ! offset for particle ID number
 
      parameter(mfi_dch  = 15) ! number of words per hit in output bank
      parameter(ofidc_x1 =  1) ! offset for entrance X position daughter
      parameter(ofidc_y1 =  2) ! offset for entrance Y position daughter
      parameter(ofidc_z1 =  3) ! offset for entrance Z position daughter
      parameter(ofidc_f1 =  4) ! offset for entrance time
      parameter(ofidc_x2 =  5) ! offset for entrance X position daughter
      parameter(ofidc_y2 =  6) ! offset for entrance Y position daughter
      parameter(ofidc_z2 =  7) ! offset for entrance Z position daughter
      parameter(ofidc_f2 =  8) ! offset for exit time (new)
      parameter(ofidc_tr =  9) ! offset for track number
      parameter(ofidc_nw = 10) ! offset for wire number
      parameter(ofidc_id = 11) ! offset for particle ID number 
      parameter(ofidc_xg1 = 12) ! offset for x global coordinate at entrance
      parameter(ofidc_yg1 = 13) ! offset for y global coordinate at entrance
      parameter(ofidc_zg1 = 14) ! offset for z global coordinate at entrance
      parameter(ofidc_pl = 15) ! offset for path length value
c
c --- end of fpilink.inc -----------------------------------------------
c
