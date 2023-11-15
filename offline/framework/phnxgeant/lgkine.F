***************************************************************************
      FUNCTION LgKINE (J,I)
* Description: return Link to the I-th track in LgKINE and Link           *
*              to its mother bank (in JKINE linear chain) in J            *
***************************************************************************

#include "geant321/gcbank.inc"
#include "geant321/gclink.inc"

      INTEGER  LgKINE,I,J

      LgKINE = 0 
      J = JKINE 
      DO WHILE ((J.GT.0).AND.(J.LT.NZEBRA).AND.(I.GT.0))
         
        IF ((I.GT.IQ(J+2)).AND.(LQ(J).EQ.0)) THEN
c         Larger than floor, no higher link, this is it!
c         We go through this when adding a new track
          LgKINE = LQ(J-I+IQ(J+2))
          RETURN
        ELSE IF ((I.GT.IQ(J+2)).AND.(I.LE.IQ(J+1))
     *           .AND.(LQ(J).NE.0)) THEN
c         Larger than floor, lower than celing, with a higher link - OK
c         We go through this when seeking an existing track when 
c         multiple banks are booked
          LgKINE = LQ(J-I+IQ(J+2))
          RETURN
        ELSE IF (I.GT.IQ(J+1)) THEN
c         Larger than ceiling, next link up and try again
          J = LQ(J)
        ELSE IF (I.LE.IQ(J+2)) THEN
c         Smaller than floor, go down one link
          J = LQ(J+2)
        END IF

      END DO
      PRINT *,' LgKINE error - track ',I,' not found'
      END
