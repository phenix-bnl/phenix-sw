      SUBROUTINE FY_INI_ZEB

C     Setup link area for the FCL forward calorimeter
C     CFM 1/25/2003

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpylink.inc"


C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FYLINK/',
     #             lFY_link,
     #             lFY_lref,
     #             lFY_last)
      RETURN
      END
