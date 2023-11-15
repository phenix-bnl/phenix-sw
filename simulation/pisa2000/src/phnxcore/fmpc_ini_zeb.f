      SUBROUTINE FMPC_INI_ZEB

C     Setup link area for MPC

c     Original author: V.Dzhordzhadze

      IMPLICIT NONE

#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fmpclink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FMPCLINK/',
     #             LFMPC_LINK,
     #             LFMPC_LREF,
     #             LFMPC_LAST)
      RETURN
      END
