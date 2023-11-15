*CMZ :  2.04/00 08/10/92  21.37.07  by  Charles F. Maguire
*-- Author :    Charles F. Maguire   08/10/92
        Integer function egzend ()

c        UNIX Version of code

c        Routine to clean up ZEBRA

        implicit none

c        Global Declarations
c        ===================

*KEEP,EVTZEBRA.
#include "evtzebra.inc"
*KEND.

c        Executable code
c        ===============

        egzend = -1        ! assume success

        call fzendo(z_lun_out,'T')
        close (unit=z_lun_out)

        return
        end
