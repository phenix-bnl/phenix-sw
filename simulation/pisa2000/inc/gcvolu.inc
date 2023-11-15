c $Id: gcvolu.inc,v 1.2 2008/05/21 08:15:56 hpereira Exp $

c     COMMON/GCVOLU/NLEVEL,NAMES(15),NUMBER(15),
c      +LVOLUM(15),LINDEX(15),INFROM,NLEVMX,NLDEV(15),LINMX(15),
c      +GTRAN(3,15),GRMAT(10,15),GONLY(15),GLX(3)

      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,
     +        NLDEV,LINMX
      REAL GTRAN,GRMAT,GONLY,GLX

c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcvolu.inc"
