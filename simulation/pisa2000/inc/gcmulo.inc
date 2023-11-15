c $Id: gcmulo.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

c     COMMON/GCMULO/SINMUL(101),COSMUL(101),SQRMUL(101),OMCMOL,CHCMOL
c      +  ,EKMIN,EKMAX,NEKBIN,NEK1,EKINV,GEKA,GEKB,EKBIN(200),ELOW(200)

c       REAL SINMUL,COSMUL,SQRMUL,OMCMOL,CHCMOL,EKMIN,EKMAX,ELOW,EKINV
c      REAL GEKA,GEKB,EKBIN
c      INTEGER NEKBIN,NEK1


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcmulo.inc"
