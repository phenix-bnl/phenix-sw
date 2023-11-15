      SUBROUTINE FX_INI_ZEB

C     Setup link area for the FPRI bank
C     CFM 5/29/98

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpxlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FXLINK/',
     #             lFX_link,
     #             lFX_lref,
     #             lFX_last)
      RETURN
      END
