c $Id: gcgobj.inc,v 1.2 2008/05/21 08:15:53 hpereira Exp $

c     PARAMETER (NTRCG=1)
c       PARAMETER (NWB=207,NWREV=100,NWS=1500)
c       PARAMETER (C2TOC1=7.7, C3TOC1=2.,TVLIM=1296.)
c       COMMON /GCGOBJ/IST,IFCG,ILCG,NTCUR,NFILT,NTNEX,KCGST
c      +             ,NCGVOL,IVFUN,IVCLOS,IFACST,NCLAS1,NCLAS2,NCLAS3
c       COMMON /CGBLIM/IHOLE,CGXMIN,CGXMAX,CGYMIN,CGYMAX,CGZMIN,CGZMAX


c using official geant321 headers in place of local hacks
c to avoid conflicts when changing geant version
#include "geant321/gcgobj.inc"
