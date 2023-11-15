c
c     fNCClink.inc file for NCC systems 
c
c     Original Author:  V. Dzhordzhadze
c     MOdified by: Sky Rolnick, Ondrej Chvala
c     
       INTEGER LFNCC_LINK, LFNCC1, LFNCC2, LFNCC3, 
     >         LFNCC_PARA, LFNCC_LREF, LFNCC_LAST
       INTEGER LFNCC_PARU
       COMMON /FNCLINK/ LFNCC_LINK, 
     >     LFNCC1(1,MAXSUB),LFNCC2(9,MAXSUB),
     >     LFNCC3(1,MAXSUB),
     >     LFNCC_PARA, LFNCC_PARU,
     >     LFNCC_LREF, LFNCC_LAST


       INTEGER OFINCC1_IEVT,
     >         OFINCC1_INCC,  OFINCC1_TWRID,
     >         OFINCC1_SENID, OFINCC1_TOFIN,
     >         OFINCC1_DEDX1, OFINCC1_TR,
     >         MFI_NCC1


       PARAMETER (OFINCC1_TR=1,OFINCC1_IEVT=2,
     >         OFINCC1_INCC=3, OFINCC1_TWRID=4, OFINCC1_SENID=5,
     >         OFINCC1_TOFIN=6, OFINCC1_DEDX1=7, MFI_NCC1=7)


       INTEGER OFINCC2_IEVT,
     >         OFINCC2_INCC, 
     >         OFINCC2_DEDX1, OFINCC2_TR,
     >         MFI_NCC2


       PARAMETER (OFINCC2_TR=1,OFINCC2_IEVT=2,
     >         OFINCC2_INCC=3, OFINCC2_DEDX1=4, MFI_NCC2=4)

       INTEGER OFINCC3_TR,
     >	       OFINCC3_VX,
     >         OFINCC3_VY, 
     >         OFINCC3_VZ, 
     >         OFINCC3_PX,
     >         OFINCC3_PY,
     >         OFINCC3_PZ,
     >         MFI_NCC3

       PARAMETER (OFINCC3_TR=1,
     >         OFINCC3_VX=2,OFINCC3_VY=3, OFINCC3_VZ=4,
     >         OFINCC3_PX=5,OFINCC3_PY=6, OFINCC3_PZ=7,   
     >         MFI_NCC3=7)


