c $Id: gctime.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

c     COMMON/GCTIME/TIMINT,TIMEND,ITIME,IGDATE,IGTIME
c       INTEGER ITIME,IGDATE,IGTIME
c       REAL TIMINT,TIMEND


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gctime.inc"
