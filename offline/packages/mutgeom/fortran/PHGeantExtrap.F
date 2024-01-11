* $Id: PHGeantExtrap.F,v 1.11 2017/09/16 06:14:12 shlim Exp $
*********************************************************************
      SUBROUTINE PHGEANTEXTRAP
     *           (ARM, IPA, ZIN, PIN, CIN, ZOUT, POUT, COUT, DPOUTDPIN,
     *            FAILURE)
*********************************************************************
*********************************************************************
      IMPLICIT NONE
* Arguments
*     Input
      INTEGER Arm ! muon tracker arm
      INTEGER IPA ! particle ID
      DOUBLE PRECISION Zin, Pin(5), Cin(5, 5)
      !REAL Zin, Pin(5), Cin(5, 5)
*     Output
*     Failure: 0 if success, 9 if final plane not reached
      INTEGER Failure
      DOUBLE PRECISION Zout, Pout(5), Cout(5, 5), DPoutDPin(5, 5)
      !REAL Zout, Pout(5), Cout(5, 5), DPoutDPin(5, 5)

* COMMON blocks
#include "geant321/ertrio.inc"
#include "geant321/eropts.inc"
      
* Internal variables
      CHARACTER*4 CHOPT
      INTEGER I, IERR, J, K
*      REAL CHARGE, COV1(25), DUMMYC(3, 3), DUMMYD(3, 3), FACT, H(3)
      REAL CHARGE, COV1(25), DUMMYC(15), DUMMYD(15), FACT, H(3)
      REAL P0, P1(3), P1C(3), P1D(3), P2(3)
      REAL XIN(3), X2(3)
      REAL SPU, UNITVECT(3), UNIT1(3, 3), UNIT2(3, 3)

**************************************************
* Similar to the interface coded in "mum_gfitkf.F",
* except that the local system is taken as equal to the global system,
* i.e. no need for rotation matrices PLANI;
* will need some simplification????
**************************************************
      SAVE H
      DATA H / 0.0, 0.0, 20.0 / ! not really needed
      Failure = 0

* initialise matrices     
      CALL VZERO(DUMMYD, 9)
      CALL VZERO(UNIT1, 9)
      UNIT1(1, 1) = 1.0
      UNIT1(2, 2) = 1.0
      UNIT1(3, 3) = Zin
      CALL UCOPY(UNIT1, UNIT2, 9)
      UNIT2(3, 3) = Zout

* Input momentum in unsigned 1/P, X', Y'
      !P1D(1) = ABS(Pin(1))
      P1D(1) = SNGL(ABS(Pin(1)))
      P1D(2) = Pin(2)
      P1D(3) = Pin(3)

* Charge and GEANT particle type
      CHARGE = SIGN(1.0, SNGL(Pin(1)))
      !CHARGE = SIGN(1.0, Pin(1))

* Momentum direction along the Z axis:
* particles are coming from a vertex at around Z=0
* and are assumed to stay in the same arm
* - for South arm (Arm = 0), + for North arm (Arm = 1)
      IF ( Arm .EQ. 0 ) THEN 
        SPU = -1
      ELSE
        SPU = 1
      ENDIF

* Input momentum in spherical coordinates
      CALL TRSDSC(P1D, DUMMYD, P1C, DUMMYC, H, CHARGE, IERR, SPU,
     +            UNIT1(1, 1), UNIT1(1, 2))
                     
* Input momentum in Cartesian coordinates
      CALL DIRCOS(P1C(2), UNITVECT)
      P0 = 1.0 / P1D(1)
      DO I = 1, 3
         P1(I) = P0 * UNITVECT(I)
      ENDDO

* Input covariance matrix in unsigned 1/P, X', Y':
* triangular matrix COV1 from argument Cin
      K = 0
      DO I = 1, 5
         DO J = I, 5
            K = K + 1
            IF ((K .NE. 1) .AND. (K .LE. 5)) THEN
               FACT = CHARGE
            ELSE
               FACT = 1.0
            ENDIF
            COV1(K) = FACT * SNGL(Cin(I, J))
            !COV1(K) = FACT * Cin(I, J)
         ENDDO
      ENDDO

* Preparation for track extrapolation
      CALL EUFILP(1, COV1, UNIT1, UNIT2)

* check backward/forward extrapolation
      IF( ( Arm.EQ.1 .AND. ( Zin .LE. Zout ) ) .OR. 
     +    ( Arm.EQ.0 .AND. ( Zin .GE. Zout ) ) ) THEN     
*       forward extrapolation
        CHOPT = 'EP'
      ELSE 
*        backward extrapolation
         CHOPT = 'BEP'
      ENDIF

            
* Input position: no rotation
      XIN(1) = Pin(4)
      XIN(2) = Pin(5)
      XIN(3) = Zin

      CALL ERTRAK(XIN, P1, X2, P2, IPA, CHOPT)
      IF (IEPRED(1).NE.1) THEN
         Failure = 9
         RETURN
      END IF  

* Output momentum (signed 1/P, Px/Pz, Py/Pz)
* from ERPOUT in COMMON ERTRIO
      Pout(1) = CHARGE * ERPOUT(1, 1)
      Pout(2) = ERPOUT(2, 1)
      Pout(3) = ERPOUT(3, 1)

* Output position (Z already in Zout)
* from X2 result of ERTRAK
      Pout(4) = X2(1)
      Pout(5) = X2(2)

* Output covariance matrix (signed 1/P, X', Y', X, Y)
* symmetric Cout from ERROUT in COMMON ERTRIO
      K = 0
      DO I = 1, 5
         DO J = I, 5
            K = K + 1
            IF ((K .NE. 1) .AND. (K .LE. 5)) THEN
               FACT = CHARGE
            ELSE
               FACT = 1.0
            ENDIF
            Cout(I, J) = FACT * ERROUT(K, 1)
            Cout(J, I) = Cout(I, J)
         ENDDO
      ENDDO
      
* Partial derivatives from ERDTRP in COMMON ERTRIO:
* charge correction factor to go from unsigned 1/P to signed 1/P;
* no inversion of rows and columns is needed between ERDTRP and DPoutDPin
      DO I = 1, 5
         DO J = 1, 5
            IF (((I .EQ. 1) .AND. (J .NE. 1)) .OR.
     +          ((J .EQ. 1) .AND. (I .NE. 1))) THEN
               FACT = CHARGE
            ELSE
               FACT = 1.0
            ENDIF
            DPoutDPin(I, J) = FACT * ERDTRP(I, J, 1)
         ENDDO
      ENDDO
      
      RETURN
      END
