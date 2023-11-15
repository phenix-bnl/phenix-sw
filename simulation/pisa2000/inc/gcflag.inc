c $Id: gcflag.inc,v 1.2 2008/05/21 08:15:53 hpereira Exp $

c     COMMON/GCFLAG/IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
c      +        ,IEOTRI,IEVENT,ISWIT(10),IFINIT(20),NEVENT,NRNDM(2)
c       COMMON/GCFLAX/BATCH, NOLOG
c       LOGICAL BATCH, NOLOG

      INTEGER 
     +  IDEBUG,IDEMIN,IDEMAX,ITEST,IDRUN,IDEVT,IEORUN
     +  ,IEOTRI,IEVENT,ISWIT,IFINIT,NEVENT,NRNDM


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcflag.inc"
