C...  
C...  Author: V.Dzhordzhade 07.19.2004

      SUBROUTINE FNCC_GET_DST(READNXT,NUH,IUHEAD)
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
*KEEP,FNCCLINK.
#include "fncclink.inc"
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEND.
      IF(IUHEAD(4) .EQ. U_CTOH4('PARA'))THEN
C  accept the bank ('A')
        CALL FZIN(LUN_DINP,IXDIV_FE,LFNCC_PARA,1,'A',NUH,IUHEAD)
C  check that FZIN read went OK. Return READNXT false if not OK
        CALL IQUEST_CHK(READNXT)
        WRITE(6,60)
     &     '# log. rec. read  ',IQUEST(2),
     &     '# phys. rec. read ',IQUEST(3),
     &     'Entry address     ',IQUEST(13),
     &     '# words in d.st.  ',IQUEST(14),
     &     'Top bank name     ',LQF(LFNCC_PARA + 4)
      ELSE IF(IUHEAD(4) .EQ. U_CTOH4('PARU'))THEN
        CALL FZIN(LUN_DINP,IXDIV_FE,LFNCC_PARU,1,'A',NUH,IUHEAD)
C  check that FZIN read went OK. Return READNXT false if not OK
        CALL IQUEST_CHK(READNXT)
        WRITE(6,60)
     &     '# log. rec. read  ',IQUEST(2),
     &     '# phys. rec. read ',IQUEST(3),
     &     'Entry address     ',IQUEST(13),
     &     '# words in d.st.  ',IQUEST(14),
     &     'Top bank name     ',LQF(LFNCC_PARU + 4)
      END IF
60    FORMAT(4(1H ,A,I10,/),1H ,A,A4)
      RETURN
      END
