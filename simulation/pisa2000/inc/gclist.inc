c       COMMON/GCLIST/NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
c      +       ,NSTAT,LHSTA(20),LGET (20),LSAVE(20),LSETS(20),LPRIN(20)
c      +             ,LGEOM(20),LVIEW(20),LPLOT(20),LSTAT(20)

      INTEGER       NHSTA,NGET ,NSAVE,NSETS,NPRIN,NGEOM,NVIEW,NPLOT
     + ,NSTAT,LHSTA,LGET ,LSAVE,LSETS,LPRIN,LGEOM,LVIEW,LPLOT,LSTAT


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gclist.inc"
