*CMZ :  2.04/00 16/04/93  19.56.30  by  Hyon-Joo Kehayias
*-- Author :
*-- Author :
      SUBROUTINE GULOREN(SBETA,SPA,SPB)
C     IMPLICIT NONE
C.
C.    ******************************************************************
C.    *                                                                *
C     *       Routine to transform momentum and energy from the        *
C     *       Lorentz frame A to the Lorentz frame B                   *
C     *                                                                *
C     *       SPA(1)                                                    *
C     *       SPA(2)     Momentum components in frame A                 *
C     *       SPA(3)                                                    *
C     *       SPA(4)     Energy                                         *
C     *       SPB(..)   same quantities in frame B                      *
C     *                                                                *
C     *       SBETA(1)    Components of velocity of frame B             *
C     *       SBETA(2)        as seen from frame A                      *
C     *       SBETA(3)                                                  *
C     *       SBETA(4)    1./SQRT(1.-BETA**2)                           *
C.    *                                                                *
C.    *    ==>Called by : GDECAY,GDECA3                                *
C.    *       Author    M.Hansroul  *********                          *
C     *    ==>added double precision. S.R.Tonse 1st April 1993.        *
C.    *                                                                *
C.    ******************************************************************
C.
C  variables passed in/out (single precision)
      REAL SBETA(4),SPA(4),SPB(4)
C  Local variables, double precision
      DOUBLE PRECISION BETA(4), PA(4), PB(4), BETPA, BPGAM
      INTEGER I
C.
C.    ------------------------------------------------------------------
C.
      DO I = 1,4
        PA(I) = DBLE(SPA(I))
        BETA(I) = DBLE(SBETA(I))
      END DO
 
      BETPA  = BETA(1)*PA(1) + BETA(2)*PA(2) + BETA(3)*PA(3)
      BPGAM  = (BETPA * BETA(4)/(BETA(4) + 1.) - PA(4)) * BETA(4)
      PB(1) = PA(1) + BPGAM  * BETA(1)
      PB(2) = PA(2) + BPGAM  * BETA(2)
      PB(3) = PA(3) + BPGAM  * BETA(3)
      PB(4) =(PA(4) - BETPA) * BETA(4)
 
      DO I = 1,4
        SPB(I) = SNGL(PB(I))
      END DO
      RETURN
      END
