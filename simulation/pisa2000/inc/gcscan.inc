c $Id: gcscan.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

      INTEGER MSLIST,NPHI,IPHIMI,IPHIMA,IPHI1,IPHIL,NTETA,MODTET,NSLMAX,
     +        MAXMDT,NSLIST,ISLIST,IPHI,ITETA,ISCUR
      REAL    PHIMIN,PHIMAX,TETMIN,TETMAX,VSCAN,FACTX0,FACTL,
     +        FACTR,SX0,SABS,TETMID,TETMAD
     +       ,SX0S,SX0T,SABSS,SABST,FACTSF
     +       ,DLTPHI,DLTETA,DPHIM1,DTETM1
     +       ,FCX0M1,FCLLM1,FCRRM1
c       PARAMETER (MSLIST=32,MAXMDT=3)
c       COMMON/GCSCAN/SCANFL,NPHI,PHIMIN,PHIMAX,NTETA,TETMIN,TETMAX,
c      +              MODTET,IPHIMI,IPHIMA,IPHI1,IPHIL,NSLMAX,
c      +              NSLIST,ISLIST(MSLIST),VSCAN(3),FACTX0,FACTL,
c      +              FACTR,IPHI,ITETA,ISCUR,SX0,SABS,TETMID(MAXMDT),
c      +              TETMAD(MAXMDT)
c      +             ,SX0S,SX0T,SABSS,SABST,FACTSF
c      +             ,DLTPHI,DLTETA,DPHIM1,DTETM1
c      +             ,FCX0M1,FCLLM1,FCRRM1
      LOGICAL SCANFL
c       COMMON/GCSCAC/SFIN,SFOUT
      CHARACTER*80 SFIN,SFOUT


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcscan.inc"
