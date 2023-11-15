      FUNCTION ISA_TO_GEANT(ISA)
C
c Translates ISA (ISAJET particle code) to ID (GEANT code) SRTonse 10-AUG-1992
c
      implicit none
c
      integer isa,id,is,i, ISA_TO_GEANT

      real gran

c
      INTEGER ISACOD(44)
      DATA ISACOD/10,-12,12,11,-14,14,110,120,-120,-20,130,-130,1220,
     *1120,-1120,20,220,2130,1130,1230,2230,1330,2330,3331,-1220,
     *-2130,-1130,-1230,-2230,-1330,-2330,-3331,-16,16,-240,
     *240,-140,140,-340,340,2140,80,-80,90/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      IS=IABS(ISA)
      IF(IS.GE.11 .AND. IS.LE.16 .AND.
     &   IS.NE.12 .AND. IS.NE.14)ISA = 11    !e neutrino
        DO I=1,44
        IF(ISA.EQ.ISACOD(I))THEN
          ID = I
          GO TO 5       !jump out of loop
        END IF
        END DO
        if (isa.eq.-110) then
          id = 7     ! anti pi 0
        else if (abs(isa).eq.230) then   ! K0 and AK0
          call grndm(gran,1)
          if (gran .lt. 0.5) then
            id = 10             ! change to KL
          else
            id = 16             ! change to KS
          end if
        ELSE
          WRITE(6,*)' Particle type not found. ISA = ',ISA,
     &       ' Setting to e neutrino'
          ID = 4
        END IF
5       CONTINUE
      ISA_TO_GEANT = ID
      RETURN
      END
