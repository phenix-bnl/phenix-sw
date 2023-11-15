      SUBROUTINE FNCC_INI_ZEB

C     Setup link area for NCC

c     Original author: V.Dzhordzhadze

      IMPLICIT NONE

#include "stosto.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fncclink.inc"

C *** setup link area(s)

      CALL MZLINK( ixstor_F,
     #             '/FNCLINK/',
     #             LFNCC_LINK,
     #             LFNCC_LREF,
     #             LFNCC_LAST)
      RETURN
      END
