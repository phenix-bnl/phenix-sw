c $Id: gcsets.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

c     COMMON/GCSETS/IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)

      INTEGER       IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcsets.inc"
