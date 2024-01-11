*CMZ :  2.04/00 07/10/94  15.42.36  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   07/10/94
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                                                                          c
c	SUBROUTINES FROM "Numerical Recipes"                               c
c                                                                          c
c       Authors:     W.H. Press, B.P. Flannery, S.A. Teukolsky             c
c                    and W.T. Vetterling                                   c
c                                                                          c
c       Publisher:   Cambridge Unversity Press                             c
c                                                                          c
c       Description:                                                       c
c                                                                          c
c       See Chapter 3, especially section 3.6 "Interpolation in Two        c
c       or More Dimensions"                                                c
c                                                                          c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      SUBROUTINE HUNT(XX,N,X,JLO)
      DIMENSION XX(N)
      LOGICAL ASCND
      ASCND=XX(N).GT.XX(1)
      IF(JLO.LE.0.OR.JLO.GT.N)THEN
        JLO=0
        JHI=N+1
        GO TO 3
      ENDIF
      INC=1
      IF(X.GE.XX(JLO).EQV.ASCND)THEN
1       JHI=JLO+INC
        IF(JHI.GT.N)THEN
          JHI=N+1
        ELSE IF(X.GE.XX(JHI).EQV.ASCND)THEN
          JLO=JHI
          INC=INC+INC
          GO TO 1
        ENDIF
      ELSE
        JHI=JLO
2       JLO=JHI-INC
        IF(JLO.LT.1)THEN
          JLO=0
        ELSE IF(X.LT.XX(JLO).EQV.ASCND)THEN
          JHI=JLO
          INC=INC+INC
          GO TO 2
        ENDIF
      ENDIF
3     IF(JHI-JLO.EQ.1)RETURN
      JM=(JHI+JLO)/2
      IF(X.GT.XX(JM).EQV.ASCND)THEN
        JLO=JM
      ELSE
        JHI=JM
      ENDIF
      GO TO 3
      END
