c $Id: gcscal.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

      INTEGER MXSLNK, ISLINK, LSLAST, LSCAN, LSTEMP, LSPARA, LSERAY

c       PARAMETER(MXSLNK=100)
c       COMMON/GCSCAL/ ISLINK(MXSLNK)
c       EQUIVALENCE (LSLAST,ISLINK(MXSLNK))
c       EQUIVALENCE (LSCAN ,ISLINK(1)),(LSTEMP,ISLINK(2))
c       EQUIVALENCE (LSPARA,ISLINK(3)),(LSERAY,ISLINK(4))


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcscal.inc"
