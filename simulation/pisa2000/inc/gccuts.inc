c $Id: gccuts.inc,v 1.2 2008/05/21 08:15:53 hpereira Exp $


c     COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
c      +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)

      REAL          CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gccuts.inc"
