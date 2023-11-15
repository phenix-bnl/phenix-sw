c
c     FPPLINK.INC file for PC2/PC3 systems (split from TRD/TEC system)
c
c     Original Author: Charles F. Maguire, March 29, 1996
c                      taken from FPTLINK.INC file
c     
c     Revision History
c
c
c
c    Name           Date            Comment
c    C.F. Maguire   Nov. 1993       8 PC chambers  (change for UCDR version)
c
c    C.F. Maguire   Feb. 14, 1998   Path length and global coordinates in output
c
       INTEGER LFP_LINK, LFP_PD2, LFP_PD3, LFP_PARA, LFP_LREF, LFP_LAST
       INTEGER LFP_PARU
       COMMON/FPLINK/ LFP_LINK, 
     >     LFP_PD2(8,MAXSUB), LFP_PD3(8,MAXSUB), LFP_PARA, LFP_PARU,
     >     LFP_LREF, LFP_LAST
 
       INTEGER OFIPD2_TR, OFIPD2_X1, OFIPD2_Y1, OFIPD2_Z1,
     >    OFIPD2_X2, OFIPD2_Y2, OFIPD2_Z2, OFIPD2_TF, OFIPD2_ID,
     >    OFIPD2_DE, MFI_PD2,
     >    OFIPD2_XG1, OFIPD2_YG1, OFIPD2_ZG1, OFIPD2_PL
 
       PARAMETER ( OFIPD2_TR=1, OFIPD2_X1=2,
     >             OFIPD2_Y1=3, OFIPD2_Z1=4,
     >             OFIPD2_X2=5, OFIPD2_Y2=6,
     >             OFIPD2_Z2=7, OFIPD2_TF=8,
     >             OFIPD2_ID=9, OFIPD2_DE=10,
     >             OFIPD2_XG1=11, OFIPD2_YG1=12, OFIPD2_ZG1=13,
     >             OFIPD2_PL=14, MFI_PD2=14 )
 
       INTEGER OFIPD3_TR, OFIPD3_X1, OFIPD3_Y1, OFIPD3_Z1,
     >    OFIPD3_X2, OFIPD3_Y2, OFIPD3_Z2, OFIPD3_TF, OFIPD3_ID,
     >    OFIPD3_DE, MFI_PD3,
     >    OFIPD3_XG1, OFIPD3_YG1, OFIPD3_ZG1, OFIPD3_PL
 
       PARAMETER ( OFIPD3_TR=1, OFIPD3_X1=2,
     >             OFIPD3_Y1=3, OFIPD3_Z1=4,
     >             OFIPD3_X2=5, OFIPD3_Y2=6,
     >             OFIPD3_Z2=7, OFIPD3_TF=8,
     >             OFIPD3_ID=9, OFIPD3_DE=10,
     >             OFIPD3_XG1=11, OFIPD3_YG1=12, OFIPD3_ZG1=13,
     >             OFIPD3_PL=14, MFI_PD3=14 )
 
       integer npd23_para
       parameter (npd23_para = 132)

