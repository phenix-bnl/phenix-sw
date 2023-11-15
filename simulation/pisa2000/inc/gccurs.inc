c $Id: gccurs.inc,v 1.2 2008/05/21 08:15:53 hpereira Exp $

      INTEGER INTFLA
      REAL    SIZD2,FACHV,HALF,SAVPLX,SAVPLY,YPLT,XPLT

c       COMMON/GCCURS/INTFLA,SIZD2,FACHV,HALF,SAVPLX,SAVPLY,YPLT,XPLT


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gccurs.inc"
