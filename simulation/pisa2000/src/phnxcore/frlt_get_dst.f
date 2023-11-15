C...  
C...  Author: L. A. Linden Levy and M. C. McCain 17.02.2004

      SUBROUTINE FRLT_GET_DST(READNXT,NUH,IUHEAD)
C  Cerenkov data strucure reading routine
      IMPLICIT NONE
      LOGICAL READNXT
      INTEGER IUHEAD(*),NUH, U_CTOH4
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,QUEST.
#include "quest.inc"
*KEEP,SUBLINK.
#include "sublink.inc"
*KEEP,FPCLINK.
#include "frltlink.inc"
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEND.
      IF(IUHEAD(4) .EQ. U_CTOH4('PARA'))THEN
C  accept the bank ('A')
        CALL FZIN(LUN_DINP,IXDIV_FE,LFrlt_PARA,1,'A',NUH,IUHEAD)
C  check that FZIN read went OK. Return READNXT false if not OK
        CALL IQUEST_CHK(READNXT)
        WRITE(6,60)
     &     '# log. rec. read  ',IQUEST(2),
     &     '# phys. rec. read ',IQUEST(3),
     &     'Entry address     ',IQUEST(13),
     &     '# words in d.st.  ',IQUEST(14),
     &     'Top bank name     ',LQF(LFrlt_PARA + 4)
      ELSE IF(IUHEAD(4) .EQ. U_CTOH4('PARU'))THEN
        CALL FZIN(LUN_DINP,IXDIV_FE,LFrlt_PARU,1,'A',NUH,IUHEAD)
C  check that FZIN read went OK. Return READNXT false if not OK
        CALL IQUEST_CHK(READNXT)
        WRITE(6,60)
     &     '# log. rec. read  ',IQUEST(2),
     &     '# phys. rec. read ',IQUEST(3),
     &     'Entry address     ',IQUEST(13),
     &     '# words in d.st.  ',IQUEST(14),
     &     'Top bank name     ',LQF(LFrlt_PARU + 4)
      END IF
60    FORMAT(4(1H ,A,I10,/),1H ,A,A4)
      RETURN
      END
