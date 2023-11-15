c $Id: gckine.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

c     COMMON/GCKINE/IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP
c      +      ,NAPART(5),AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD

      INTEGER       IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD
      REAL          PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gckine.inc"
