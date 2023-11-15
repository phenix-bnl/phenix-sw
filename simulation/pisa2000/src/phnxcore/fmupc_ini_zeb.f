      SUBROUTINE Fmupc_INI_ZEB

C     Setup link area for MuPC

c     Original author:  Wei Xie

      IMPLICIT NONE

#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fmupclink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FZLINK/',
     #             LFmupc_LINK,
     #             LFmupc_LREF,
     #             LFmupc_LAST)
      RETURN
      END
