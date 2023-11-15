      SUBROUTINE INR_USER

      IMPLICIT NONE
C  to pull out entry from Silicon INNER detector digitization bank
c  SRTonse 31-JUL-1992
#include "guphnx.inc"
#include "fstore.inc"
#include "sublink.inc"
#include "fpdlink.inc"
      INTEGER IBANK,I,IPOINT,IPART,ITRA,MULT
      REAL X,Y,Z,DELE, PX, PY, PZ
        INTEGER N_SUBVOLUMES,ISUB
        PARAMETER (N_SUBVOLUMES=2)
        CHARACTER*4 NAME_SUBVOLUMES(N_SUBVOLUMES)
     +             /'SIL1','SIL2'/
      if(cvolu_opt(4,3).eq.'ELEM') then
        DO IBANK = 1,2
C  pull out info in bank SIL1 & SIL2
        MULT = iqf(lFD_SIL(IBANK,1)+1)   ! retrieve stored number of hits
        if(MULT .gt. 0)then
          WRITE(6,50)'Data for bank ',IBANK
50        FORMAT(1H0,A,I3)
          IPOINT = lFD_SIL(IBANK,1) + 2     ! offset into mother bank
          DO I = 1,MULT
              X = qf(IPOINT + OFDM_x)
              Y = qf(IPOINT + OFDM_y)
              Z = qf(IPOINT + OFDM_z)
              DELE = qf(IPOINT + OFDM_del)
              PX = qf(IPOINT + OFDM_PX)
              IPART = iqf(IPOINT + OFDM_partl)
              ITRA = iqf(IPOINT + OFDM_itra)
            IPOINT = IPOINT + mfd_sil
            IF(I .LT. 11)WRITE(6,100)
     &         'X,Y,Z,Deposited energy, part.type,',
     &         ' part.#, mom', X,Y,Z,DELE,IPART,ITRA,PX
100         FORMAT(2H  ,2A,/,3F7.2,F10.4,2I6,F10.3)
          ENDDO
        ELSE
          write(6,*)' G_USER (INNR silicon: no hits in data bank for',
     &       NAME_SUBVOLUMES(IBANK)
        ENDIF
        END DO
      ENDIF       ! check for ELEM
      RETURN
      END
