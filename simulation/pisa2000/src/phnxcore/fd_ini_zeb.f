      SUBROUTINE FD_INI_ZEB

C     Setup link area for the (possible) inner Dalitz rejector
C     CFM 4/7/92

C     Revision  March 8, 2004: This ZEBRA area is for the SVX detector

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpdlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FDLINK/',
     #             lFD_link,
     #             lFD_lref,
     #             lFD_last)
      RETURN
      END
