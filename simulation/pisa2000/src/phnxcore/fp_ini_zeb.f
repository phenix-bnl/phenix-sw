      SUBROUTINE FP_INI_ZEB

C     Setup link area for PC2/PC3

c     Original author: Charles F. Maguire  March 29, 1996   Split off PC2/PC3

      IMPLICIT NONE

#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpplink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FPLINK/',
     #             lFP_link,
     #             lFP_lref,
     #             lFP_last)
      RETURN
      END
