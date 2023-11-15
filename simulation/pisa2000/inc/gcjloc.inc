c $Id: gcjloc.inc,v 1.2 2008/05/21 08:15:54 hpereira Exp $

c     COMMON/GCJLOC/NJLOC(2),JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
c      +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
c      +                  ,JMULOF,JCOEF,JRANG

c       INTEGER       NJLOC   ,JTM,JMA,JLOSS,JPROB,JMIXT,JPHOT,JANNI
c      +                  ,JCOMP,JBREM,JPAIR,JDRAY,JPFIS,JMUNU,JRAYL
c      +                  ,JMULOF,JCOEF,JRANG

c       COMMON/GCJLCK/NJLCK(2),JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
c      +                      ,JPOLAR,JTSTRA,JTSTCO,JTSTEN,JTASHO

c       EQUIVALENCE (JLASTV,JTSTEN)

c       INTEGER       NJLCK,JTCKOV,JABSCO,JEFFIC,JINDEX,JCURIN
c      +                   ,JPOLAR,JLASTV,JTSTRA,JTSTCO,JTSTEN
c      +                   ,JTASHO


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcjloc.inc"
