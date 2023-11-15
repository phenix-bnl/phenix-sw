c $Id: gcnum.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

c     COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
c      +            ,NSTMAX,NVERTX,NHEAD,NBIT
c       COMMON /GCNUMX/ NALIVE,NTMSTO

      INTEGER      NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT ,NALIVE,NTMSTO


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcnum.inc"
