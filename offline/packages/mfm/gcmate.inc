c $Id: gcmate.inc,v 1.2 2008/05/21 08:25:49 hpereira Exp $

c     COMMON/GCMATE/NMAT,NAMATE(5),A,Z,DENS,RADL,ABSL

c      INTEGER NMAT,NAMATE
c      REAL A,Z,DENS,RADL,ABSL

c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcmate.inc"
