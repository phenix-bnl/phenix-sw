      SUBROUTINE FA_INI_ZEB

C     Setup link area for the AER counter

C     Original Author: Charles F. Maguire
C     Release Date: June 5, 2003

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpalink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FALINK/',
     #             lFA_link,
     #             lFA_lref,
     #             lFA_last)
      RETURN
      END
