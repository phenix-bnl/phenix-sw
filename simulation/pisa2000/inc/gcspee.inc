c $Id: gcspee.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

      REAL S1,S2,S3,SS1,SS2,SS3,SRAGMX,SRAGMN,
     +     RAINT1,RAINT2,RMIN1,RMIN2,RMAX1,RMAX2
      INTEGER ISCOP,NTIM,NTFLAG,IOLDCU,ITSTCU,ISUBLI,IPORLI
      INTEGER LPASS,JPORJJ,LEP,JSC

c       COMMON/GCSPEE/S1,S2,S3,SS1,SS2,SS3,LEP,IPORLI,ISUBLI,
c      +              SRAGMX,SRAGMN,RAINT1,RAINT2,RMIN1,RMIN2,
c      +              RMAX1,RMAX2,JPORJJ,ITSTCU,IOLDCU,ISCOP,
c      +              NTIM,NTFLAG,LPASS,JSC


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcspee.inc"
