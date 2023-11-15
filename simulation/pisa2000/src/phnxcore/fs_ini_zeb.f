      SUBROUTINE FS_INI_ZEB

C     Setup link area for the Reaction Plane Detector

      IMPLICIT NONE
#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpslink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,           ! central Reaction Plane detector
     #             '/FSLINK/',
     #             lFS_link,
     #             lFS_lref,
     #             lFS_last)
      RETURN
      END
