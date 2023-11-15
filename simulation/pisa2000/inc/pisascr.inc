*-- Author :    Charles F. Maguire   30/09/92
C  This is for the DIGI routines to work with
C  Pisa scratch common block. SRTonse 14-SEP-1992
C
      INTEGER SCRSIZ,ITEMP
      PARAMETER(SCRSIZ=200000)
      REAL TEMP(SCRSIZ)
      COMMON/PISASCR/ITEMP(SCRSIZ)
      EQUIVALENCE (ITEMP,TEMP)
