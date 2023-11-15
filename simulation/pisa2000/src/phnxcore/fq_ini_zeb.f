      SUBROUTINE FQ_INI_ZEB

C     Setup link area for the NTC counter

C     Original Author: Charles F. Maguire
C     Release Date: July 2, 2002

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpqlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FQLINK/',
     #             lFQ_link,
     #             lFQ_lref,
     #             lFQ_last)
      RETURN
      END
