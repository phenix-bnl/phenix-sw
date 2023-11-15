      SUBROUTINE FR_INI_ZEB

C     Setup link area for the TPC counter (was previously named HBD)

C     Original Author: Charles F. Maguire
C     Release Date: March 7, 2004

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fprlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FRLINK/',
     #             lFR_link,
     #             lFR_lref,
     #             lFR_last)
      RETURN
      END
