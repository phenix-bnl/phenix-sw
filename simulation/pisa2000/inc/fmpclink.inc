c
c     fmpclink.inc file for MPC systems 
c
c     Original Author:  V. Dzhordzhadze
c     

       INTEGER LFMPC_LINK
       INTEGER LFMPC
       INTEGER LFMPC_PARA, LFMPC_PARU
       INTEGER LFMPC_LREF, LFMPC_LAST

       COMMON /FMPCLINK/
     >     LFMPC_LINK, 			! start of structural links
     >	   LFMPC(2,MAXSUB),
     >     LFMPC_PARA,
     >     LFMPC_PARU,
     >     LFMPC_LREF,			! start pf reference links
     >     LFMPC_LAST			! last link ptr
 
C
C --- offsets
C
       INTEGER OFIMPC_TR, OFIMPC_X,    OFIMPC_Y,    OFIMPC_Z,
     >         OFIMPC_DEDX, OFIMPC_EX,   OFIMPC_EY,
     >         OFIMPC_EMOM, OFIMPC_EPID, OFIMPC_ENUM,
     >         OFIMPC_TOFG, OFIMPC_TOWR,    MFI_MPC
 
       PARAMETER (OFIMPC_TR=1)
       PARAMETER (OFIMPC_X=2)
       PARAMETER (OFIMPC_Y=3)
       PARAMETER (OFIMPC_Z=4)
       PARAMETER (OFIMPC_TOFG=5)
       PARAMETER (OFIMPC_EX=6)
       PARAMETER (OFIMPC_EY=7)
       PARAMETER (OFIMPC_EMOM=8)
       PARAMETER (OFIMPC_EPID=9)
       PARAMETER (OFIMPC_ENUM=10)
       PARAMETER (OFIMPC_TOWR=11)
       PARAMETER (OFIMPC_DEDX=12)
       PARAMETER (MFI_MPC=12)

C
C -- end of fmpclink.inc
C
