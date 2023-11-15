***************************************************************************
      FUNCTION LgVERT (J,I)
* Description: return Link to the I-th track in LgVERT and Link           *
*              to its mother bank (in JVERT linear chain) in J            *
***************************************************************************

#include "geant321/gcbank.inc"
#include "geant321/gclink.inc"

      INTEGER  LgVERT,I,J

      LgVERT = 0 
      J = JVERTX 
      DO WHILE ((J.GT.0).AND.(J.LT.NZEBRA).AND.(I.GT.0))
         
        IF ((I.GT.IQ(J+2)).AND.(LQ(J).EQ.0)) THEN
c         Larger than floor, no higher link, this is it!
c         We go through this when adding a new vertex
          LgVERT = LQ(J-I+IQ(J+2))
          RETURN
        ELSE IF ((I.GT.IQ(J+2)).AND.(I.LE.IQ(J+1))
     *           .AND.(LQ(J).NE.0)) THEN
c         Larger than floor, lower than celing, with a higher link - OK
c         We go through this when seeking an existing vertex when 
c         multiple banks are booked
          LgVERT = LQ(J-I+IQ(J+2))
          RETURN
        ELSE IF (I.GT.IQ(J+1)) THEN
c         Larger than ceiling, next link up and try again
          J = LQ(J)
        ELSE IF (I.LE.IQ(J+2)) THEN
c         Smaller than floor, go down one link
          J = LQ(J+2)
        END IF

      END DO
      PRINT *,' LgVERT error - vertex ',I,' not found'
      END
