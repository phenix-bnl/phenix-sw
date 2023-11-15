      SUBROUTINE FW_INI_ZEB

C     Setup link area for the TOF-West detector

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpwlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,           ! central TOF-West detector
     #             '/FWLINK/',
     #             lFW_link,
     #             lFW_lref,
     #             lFW_last)
      RETURN
      END
