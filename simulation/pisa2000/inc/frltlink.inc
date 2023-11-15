c
c     frltlink.inc file for RLT systems 
c
c     Original Author:  L. A. Linden Levy
c     
       INTEGER LFrlt_LINK, LFrltRPC1, LFrltRPC2, LFrltRPC3, 
     *          LFrlt_PARA, LFrlt_LREF, LFrlt_LAST
       INTEGER LFrlt_PARU
       COMMON /FLLINK/ LFrlt_LINK, 
     >	   LFrltRPC1(2,MAXSUB), LFrltRPC2(2,MAXSUB), LFrltRPC3(2,MAXSUB), 
     >     LFrlt_PARA, LFrlt_PARU,
     >     LFrlt_LREF, LFrlt_LAST
 
       INTEGER OFIrltRPC1_TR, OFIrltRPC1_X1, OFIrltRPC1_Y1, 
     *         OFIrltRPC1_Z1,
     >    OFIrltRPC1_X2, OFIrltRPC1_Y2, OFIrltRPC1_Z2, OFIrltRPC1_TF, 
     >    OFIrltRPC1_ID, OFIrltRPC1_DE, MFI_rltRPC1,
     >    OFIrltRPC1_XG1, OFIrltRPC1_YG1, OFIrltRPC1_ZG1, OFIrltRPC1_PL
 
       PARAMETER ( OFIrltRPC1_TR=1, OFIrltRPC1_X1=2,
     >             OFIrltRPC1_Y1=3, OFIrltRPC1_Z1=4,
     >             OFIrltRPC1_X2=5, OFIrltRPC1_Y2=6,
     >             OFIrltRPC1_Z2=7, OFIrltRPC1_TF=8,
     >             OFIrltRPC1_ID=9, OFIrltRPC1_DE=10,
     >             OFIrltRPC1_XG1=11, OFIrltRPC1_YG1=12, 
     *             OFIrltRPC1_ZG1=13,
     >             OFIrltRPC1_PL=14, MFI_rltRPC1=14 )
 
 
       INTEGER OFIrltRPC2_TR, OFIrltRPC2_X1, OFIrltRPC2_Y1, 
     *      OFIrltRPC2_Z1,
     >    OFIrltRPC2_X2, OFIrltRPC2_Y2, OFIrltRPC2_Z2, OFIrltRPC2_TF, 
     >    OFIrltRPC2_ID, OFIrltRPC2_DE, MFI_rltRPC2,
     >    OFIrltRPC2_XG1, OFIrltRPC2_YG1, OFIrltRPC2_ZG1, OFIrltRPC2_PL
 
       PARAMETER ( OFIrltRPC2_TR=1, OFIrltRPC2_X1=2,
     >             OFIrltRPC2_Y1=3, OFIrltRPC2_Z1=4,
     >             OFIrltRPC2_X2=5, OFIrltRPC2_Y2=6,
     >             OFIrltRPC2_Z2=7, OFIrltRPC2_TF=8,
     >             OFIrltRPC2_ID=9, OFIrltRPC2_DE=10,
     >             OFIrltRPC2_XG1=11, OFIrltRPC2_YG1=12, 
     *             OFIrltRPC2_ZG1=13,
     >             OFIrltRPC2_PL=14, MFI_rltRPC2=14 )


       INTEGER OFIrltRPC3_TR, OFIrltRPC3_X1, OFIrltRPC3_Y1, 
     *    OFIrltRPC3_Z1,
     >    OFIrltRPC3_X2, OFIrltRPC3_Y2, OFIrltRPC3_Z2, OFIrltRPC3_TF, 
     >    OFIrltRPC3_ID, OFIrltRPC3_DE, MFI_rltRPC3,
     >    OFIrltRPC3_XG1, OFIrltRPC3_YG1, OFIrltRPC3_ZG1, OFIrltRPC3_PL
 
       PARAMETER ( OFIrltRPC3_TR=1, OFIrltRPC3_X1=2,
     >             OFIrltRPC3_Y1=3, OFIrltRPC3_Z1=4,
     >             OFIrltRPC3_X2=5, OFIrltRPC3_Y2=6,
     >             OFIrltRPC3_Z2=7, OFIrltRPC3_TF=8,
     >             OFIrltRPC3_ID=9, OFIrltRPC3_DE=10,
     >             OFIrltRPC3_XG1=11, OFIrltRPC3_YG1=12,
     *             OFIrltRPC3_ZG1=13,
     >             OFIrltRPC3_PL=14, MFI_rltRPC3=14 )
