*CMZ :          29/10/94  13.53.50  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   29/10/94
C
C     GLOBAL ACTION SWTCIH OPTIONS
C
      INTEGER NG_OPT
      PARAMETER (NG_OPT = 2)
      CHARACTER*4 GLBSWIT, GLOBAL_OPT
      COMMON /GLOBAL/GLBSWIT, GLOBAL_OPT(NG_OPT)
