c $Id: gcunit.inc,v 1.2 2008/05/21 08:15:56 hpereira Exp $

c     COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
c       INTEGER LIN,LOUT,NUNITS,LUNITS
c       COMMON/GCMAIL/CHMAIL
c       CHARACTER*132 CHMAIL


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcunit.inc"
