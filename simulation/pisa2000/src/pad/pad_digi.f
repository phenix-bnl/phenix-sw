*-- Author: Kirill Filimonov  13/09/95, an option of new PC2/PC3 geometry
*           is added

      subroutine pad_digi
      implicit none

#include "guphnx.inc"

c     begin execution


      
      IF(CVOLU_OPT(1,9) .EQ. 'FULL' .OR. CVOLU_OPT(1,9) .EQ. 'VOLS')
     &  THEN

        call pd23out

      ELSEIF(CVOLU_OPT(1,9) .EQ. 'PC96' .OR. 
     +       CVOLU_OPT(1,9) .EQ. 'PC98') THEN

        call pc23out

      ENDIF

      return
      end
