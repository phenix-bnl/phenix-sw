c $Id: gconst.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

      REAL          PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
      REAL          EMMU,PMASS,AVO

c       COMMON/GCONST/PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS
c       COMMON/GCONSX/EMMU,PMASS,AVO


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gconst.inc"
