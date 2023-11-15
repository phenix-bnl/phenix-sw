c
c     fmupclink.INC file for MuPC systems 
c
c     Original Author:  Wei Xie
c     
       INTEGER LFmupc_LINK, LFmupc1, LFmupc2, LFmupc3,
     *          LFmupc_PARA, LFmupc_LREF, LFmupc_LAST
       INTEGER LFmupc_PARU

       COMMON /FZLINK/ LFmupc_LINK, 
     >     LFmupc1(2,MAXSUB), LFmupc2(2,MAXSUB), LFmupc3(2,MAXSUB),
     >     LFmupc_PARA, LFmupc_PARU,
     >     LFmupc_LREF, LFmupc_LAST
 
       INTEGER OFImupc1_TR, OFImupc1_X1, OFImupc1_Y1, OFImupc1_Z1,
     >    OFImupc1_X2, OFImupc1_Y2, OFImupc1_Z2, OFImupc1_TF, 
     >    OFImupc1_ID, OFImupc1_DE, MFI_mupc1,
     >    OFImupc1_XG1, OFImupc1_YG1, OFImupc1_ZG1, OFImupc1_PL
 
       PARAMETER ( OFImupc1_TR=1, OFImupc1_X1=2,
     >             OFImupc1_Y1=3, OFImupc1_Z1=4,
     >             OFImupc1_X2=5, OFImupc1_Y2=6,
     >             OFImupc1_Z2=7, OFImupc1_TF=8,
     >             OFImupc1_ID=9, OFImupc1_DE=10,
     >             OFImupc1_XG1=11, OFImupc1_YG1=12, OFImupc1_ZG1=13,
     >             OFImupc1_PL=14, MFI_mupc1=14 )
 
 
       INTEGER OFImupc2_TR, OFImupc2_X1, OFImupc2_Y1, OFImupc2_Z1,
     >    OFImupc2_X2, OFImupc2_Y2, OFImupc2_Z2, OFImupc2_TF, 
     >    OFImupc2_ID, OFImupc2_DE, MFI_mupc2,
     >    OFImupc2_XG1, OFImupc2_YG1, OFImupc2_ZG1, OFImupc2_PL
 
       PARAMETER ( OFImupc2_TR=1, OFImupc2_X1=2,
     >             OFImupc2_Y1=3, OFImupc2_Z1=4,
     >             OFImupc2_X2=5, OFImupc2_Y2=6,
     >             OFImupc2_Z2=7, OFImupc2_TF=8,
     >             OFImupc2_ID=9, OFImupc2_DE=10,
     >             OFImupc2_XG1=11, OFImupc2_YG1=12, OFImupc2_ZG1=13,
     >             OFImupc2_PL=14, MFI_mupc2=14 )

       INTEGER OFImupc3_TR, OFImupc3_X1, OFImupc3_Y1, OFImupc3_Z1,
     >    OFImupc3_X2, OFImupc3_Y2, OFImupc3_Z2, OFImupc3_TF, 
     >    OFImupc3_ID, OFImupc3_DE, MFI_mupc3,
     >    OFImupc3_XG1, OFImupc3_YG1, OFImupc3_ZG1, OFImupc3_PL
 
       PARAMETER ( OFImupc3_TR=1, OFImupc3_X1=2,
     >             OFImupc3_Y1=3, OFImupc3_Z1=4,
     >             OFImupc3_X2=5, OFImupc3_Y2=6,
     >             OFImupc3_Z2=7, OFImupc3_TF=8,
     >             OFImupc3_ID=9, OFImupc3_DE=10,
     >             OFImupc3_XG1=11, OFImupc3_YG1=12, OFImupc3_ZG1=13,
     >             OFImupc3_PL=14, MFI_mupc3=14 )
