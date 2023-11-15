c
c   New version July 19, 1994 (drop the vertex variables)
c   November 23, 1994: Go to MAXSUB limit on the pointers
c   March 29, 1996: Split the PC2/PC3 system from TRD/TEC
c
c   Feb. 18, 1998: Add global coordinates to output
c
*
*    6  Maximum TEC planes (UPGRADE limit)
*
       INTEGER LFT_LINK, LFT_PARA, LFT_LREF, LFT_LAST
       INTEGER LFT_PARU, LFT_TEC
       COMMON/FTLINK/ LFT_LINK, LFT_TEC(6,MAXSUB),
     >     LFT_PARA, LFT_PARU, LFT_LREF, LFT_LAST
 

       INTEGER OFITEC_TR, OFITEC_X1, OFITEC_Y1, OFITEC_Z1,
     >    OFITEC_X2, OFITEC_Y2, OFITEC_Z2, OFITEC_F1, OFITEC_ID,
     >    OFITEC_F2, OFITEC_IC, OFITEC_DE, MFI_TEC,
     >    OFITEC_XG1, OFITEC_YG1, OFITEC_ZG1

 
       PARAMETER ( OFITEC_TR=1, OFITEC_X1=2,
     >             OFITEC_Y1=3, OFITEC_Z1=4,
     >             OFITEC_X2=5, OFITEC_Y2=6,
     >             OFITEC_Z2=7, OFITEC_F1=8,
     >             OFITEC_ID=9, OFITEC_F2=10,
     >             OFITEC_IC=11, OFITEC_DE=12,
     >             OFITEC_XG1=13, OFITEC_YG1=14, OFITEC_ZG1=15,
     >             MFI_TEC=15 )
