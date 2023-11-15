      SUBROUTINE FH_INI_ZEB

C     Setup link area for the HBD counter

C     Original Author: Charles F. Maguire
C     Release Date: September 30, 2002

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fphlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FHLINK/',
     #             lFH_link,
     #             lFH_lref,
     #             lFH_last)
      RETURN
      END
