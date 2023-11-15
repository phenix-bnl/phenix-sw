c $Id: gcursb.inc,v 1.2 2008/05/21 08:15:56 hpereira Exp $

c     COMMON/GCURSB/NUMNDS,IADDI,NUMND2,NNPAR,IISELT
c       COMMON/GCURSC/MOMO
c       CHARACTER*4 MOMO

      INTEGER NUMNDS,IADDI,NUMND2,NNPAR,IISELT



c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcursb.inc"
