c $Id: gcstak.inc,v 1.2 2008/05/21 08:15:55 hpereira Exp $

c     INTEGER  NJTMAX,NJTMIN,NTSTKP,NTSTKS,NDBOOK,NDPUSH,NJFREE,NJGARB,
c      +         NJINVO,LINSAV,LMXSAV,NWSTAK,NWINT,NWREAL,NWTRAC
c       PARAMETER (NWSTAK=12,NWINT=11,NWREAL=12,NWTRAC=NWINT+NWREAL+5)
c       COMMON /GCSTAK/ NJTMAX, NJTMIN, NTSTKP, NTSTKS, NDBOOK, NDPUSH,
c      +                NJFREE, NJGARB, NJINVO, LINSAV(15), LMXSAV(15)


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcstak.inc"
