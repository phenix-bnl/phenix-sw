c $Id: gcomis.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

c     COMMON/GCOMIS/ICOMIS,JUINIT,JUGEOM,JUKINE,JUSTEP,JUOUT,JULAST
      INTEGER ICOMIS,JUINIT,JUGEOM,JUKINE,JUSTEP,JUOUT,JULAST
      INTEGER JPCOMS

c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcomis.inc"
