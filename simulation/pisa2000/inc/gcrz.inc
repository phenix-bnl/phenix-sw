c $Id: gcrz.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

c     COMMON/GCRZ1/NRECRZ,NRGET,NRSAVE,LRGET(20),LRSAVE(20)
c       INTEGER      NRECRZ,NRGET,NRSAVE,LRGET    ,LRSAVE
c       COMMON/GCRZ2/RZTAGS
c       CHARACTER*8 RZTAGS(4)

c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcrz.inc"
