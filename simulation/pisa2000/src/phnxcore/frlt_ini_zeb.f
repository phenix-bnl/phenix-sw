      SUBROUTINE FRLT_INI_ZEB

C     Setup link area for RLT

c     Original author:  L. A. Linden Levy and M. C. McCain

      IMPLICIT NONE

#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "frltlink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FLLINK/',
     #             LFrlt_LINK,
     #             LFrlt_LREF,
     #             LFrlt_LAST)
      RETURN
      END
