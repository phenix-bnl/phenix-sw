c $Id: gcparm.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

c     COMMON/GCPARM/IPARAM,PCUTGA,PCUTEL,PCUTNE,PCUTHA,PCUTMU
c      +             ,NSPARA,MPSTAK,NPGENE
      REAL PACUTS(5)
c       EQUIVALENCE (PACUTS(1),PCUTGA)
      INTEGER IPARAM,MPSTAK,NSPARA,NPGENE
      REAL    PCUTGA,PCUTEL,PCUTNE,PCUTHA,PCUTMU


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcparm.inc"
