*CMZU:  2.04/00 23/10/92  17.48.13  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   23/10/92
 
*CMZ :          23/10/92  17.15.36  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   23/10/92
      SUBROUTINE SCALING(IPART,A,X,DN_DPT)
C
C     For more information, look at PHENIX memo "A parameterization
C     of p_t dependences for RHIC" (by J. Kang and R. Seto)
C     for the PHENIX R&D meeting on Feb. 14, 1992
C
      IMPLICIT  NONE
      LOGICAL   FIRST /.TRUE./
      INTEGER   IPART
      REAL      A, X, DN_DPT
      REAL      H_H, H_S
      REAL      FIT0
      SAVE      FIRST
C
C     Use scaling laws for hard and soft processes.
C     For p-p data, PYTHIA was used.
C
C     INPUT:
C          IPART - 1 : Pi0
C          IPART - 2 : Pi0  using UA1 parameters
C          IPART - 3 : Rho
C          IPART - 4 : Omega
C          IPART - 5 : Phi
C          IPART - 6 : Phi  using p*=3.0 GeV/c instead of
C                      p*=2.5 GeV/c
C          A         : Atomic number
C          X         : P_t
C
C     OUTPUT:
C          DN_DPT    : dN/2P_t*dP_t
C
      if (FIRST) then
         H_H = A**1.05
         H_S = A**(4./3.)
         FIRST = .FALSE.
      endif
C
      IF ( IPART.eq.1 ) Then
         FIT0 = H_H*17853000*EXP(-SQRT(X**2+0.135**2)/0.14246) +
     &       EXP(-2.5/x)*H_S*2617400*((3.3830/(3.3830+X))**16.814)
      ELSE IF ( IPART.eq.2 ) Then
         FIT0 = H_H*17853000*EXP(-SQRT(X**2+0.135**2)/0.14246) +
     &       EXP(-2.5/x)*H_S*9011400*((1.8000/(1.8000+X))**12.140)
      ELSE IF ( IPART.eq.3 ) Then
         FIT0 = H_H*54738000*EXP(-SQRT(X**2+0.770**2)/0.15156) +
     &       EXP(-2.5/x)*H_S*599580*((5.1707/(5.1707+X))**19.804)
      ELSE IF ( IPART.eq.4 ) Then
         FIT0 = H_H*46370000*EXP(-SQRT(X**2+0.783**2)/0.15422) +
     &       EXP(-2.5/x)*H_S*674770*((5.6709/(5.6709+X))**21.837)
      ELSE IF ( IPART.eq.5 ) Then
         FIT0 = H_H*12854000*EXP(-SQRT(X**2+1.020**2)/0.15438) +
     &       EXP(-2.5/x)*H_S*742370*((3.6681/(3.6681+X))**19.351)
      ELSE IF ( IPART.eq.6 ) Then
         FIT0 = H_H*13131000*EXP(-SQRT(X**2+1.020**2)/0.15393) +
     &       EXP(-3.0/x)*H_S*690420*((3.9796/(3.9796+X))**20.161)
      END IF
C
C   Used 600K events and changes from dN/p_t*dp_t to dN/2p_t*dp_t.
C   (Bin size in x-axis(P_t) was 0.1 GeV/c.)
C
      DN_DPT = FIT0/1200000.
C
      RETURN
      END
