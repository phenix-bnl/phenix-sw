c $Id: gchil2.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

      INTEGER LARETT,JTICK,JMYLL,JFIMOT,JFISCA,JFINAM,
     +        JAASS1,JAASS2,JAASS3,JAASS4,
     +        JTICKS,JMYLLS,JMYMOT
c       COMMON/GCHIL2/LARETT(2),JTICK,JMYLL,JFIMOT,JFISCA,JFINAM,
c      +              JAASS1,JAASS2,
c      +              JAASS3,JAASS4,JTICKS,JMYLLS,JMYMOT


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gchil2.inc"
