*CMZ :  2.04/00 01/06/93  16.00.03  by  Charles F. Maguire
*CMZ :  2.01/00 05/10/92  11.19.41  by  Charles F. Maguire
*-- Author :
*-- Author :
      SUBROUTINE E_GET_DST
C  SRTonse  1-SEP-1992. Read the FINP file and pull in the parameter banks
C  until the first 'EVNT' bank is reached
C  read the input Zebra file, & print out what is there.
      IMPLICIT NONE
*KEEP,FSTORE.
#include "fstore.inc"
*KEEP,GUPHNX.
#include "guphnx.inc"
*KEEP,QUEST.
#include "quest.inc"
*KEND.
      INTEGER NUHPERM
      PARAMETER (NUHPERM=10)
      INTEGER NUH,IUHEAD(NUHPERM),I,J, U_CTOH4
      LOGICAL READNXT, FIRST
      DATA FIRST/.TRUE./, READNXT/.TRUE./
C  read our own banks
      DO WHILE(READNXT)
      DO I = 1,NUHPERM
        IUHEAD(I) = 0
      END DO
C  let NUH enter FZIN with value NUHPERM
      NUH = NUHPERM
C   read header only ('S') to see if we want this bank
      CALL FZIN(LUN_DINP,IXDIV_FE,lqf(1),1,'S',NUH,IUHEAD)
C  check that FZIN read went OK. Return READNXT false if not OK
      CALL IQUEST_CHK(READNXT)
      WRITE(6,50)(IUHEAD(J),J=1,NUH)
50    FORMAT(1H0,'Header Array:',4(A4,3X),7I8)
      WRITE(6,60)
     &   '# log. rec. read  ',IQUEST(2),
     &   '# phys. rec. read ',IQUEST(3),
     &   'Entry address     ',IQUEST(13),
     &   '# words in d.st.  ',IQUEST(14)
60    FORMAT(4(1H ,A,I10,/))
C  if correct bank type (PARA) call getpars to read rest of bank. Use U_CTOH4
C  because of Unix version
      IF(IUHEAD(1) .EQ. U_CTOH4('PISA') .AND. IUHEAD(2) .EQ.
     &   U_CTOH4('PARA')   .AND. READNXT)THEN
        CALL GETPARS(READNXT,NUH,IUHEAD)
      ELSE IF(IUHEAD(2) .EQ. U_CTOH4('EVNT'))THEN
        READNXT = .FALSE.     !no more PARA banks
      END IF
      END DO            !while readnxt=true
      RETURN
      END
