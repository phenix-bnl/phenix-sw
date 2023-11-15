C===============================================================================
C    File name: hbd_gustep.f
C===============================================================================
      SUBROUTINE HBD_GUSTEP
      Implicit none
#include "gctrak.inc"
#include "gckine.inc"
#include "gcsets.inc"
C===============================================================================
C    Add gcking.inc and gctmed.inc for Cerenkov radiation
C===============================================================================
#include "geant321/gcking.inc"
#include "gctmed.inc"
      INTEGER K
      INTEGER IERROR
      REAL SINGLE_HIT(13)  ! Max possible # of hit components is 13, in the TPC pad rows
      SAVE SINGLE_HIT
C===============================================================================
C    Set itckov to 1 here, allowing production of Cerenkov radiation.
C===============================================================================
      itckov = 1
      if (FLOAT(IPART).EQ.50) then
         if (DESTEP.NE.0 .AND. IDET.GE.9) then        ! Ckov photon was detected in CsI
            DO K = 1,3
               SINGLE_HIT( K) = VECT( K)
            END DO
            SINGLE_HIT(4) = VECT(7)*VECT(4)*1.e9  ! Px in eV
            SINGLE_HIT(5) = VECT(7)*VECT(5)*1.e9  ! Py in eV
            SINGLE_HIT(6) = VECT(7)*VECT(6)*1.e9  ! Pz in eV
            SINGLE_HIT(7) = TOFG*1.e9             ! time of flight, converted to ns
            SINGLE_HIT(8) = FLOAT(IPART)          ! Particle ID
            SINGLE_HIT(9)  = -999.                ! Xout (not applicable to Cerenkov photon)
            SINGLE_HIT(10) = -999.                ! Yout
            SINGLE_HIT(11) = -999.                ! Zout
            SINGLE_HIT(12) = DESTEP               ! Energy loss.  If !=0, then photon detected
            SINGLE_HIT(13) = SLENG                ! Path length
            CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, SINGLE_HIT, IERROR )
            IF(IERROR.EQ.0)THEN
               write (6,*) "Failure calling GSAHIT (hbd_gustep)"
            ENDIF
         endif
      else                      ! It's not a Cerenkov photon--proceed normally
         IF (INWVOL .EQ. 0 .AND. ISTOP .EQ. 0) THEN
C===============================================================================
C    Particle already in volume, not stopped, so ONLY look at incremented energy loss, since
C    other values were stored on the particle's entry into this sensitive volume.  This 
C    will only be meaningful later for hits in the TPC padrows, and not for the HBD.
C    Accumulate energy loss.
C===============================================================================
            SINGLE_HIT(12) = SINGLE_HIT(12) + DESTEP
         ELSE IF( INWVOL .EQ. 1 ) THEN
C===============================================================================
C    Record entry position.  This will be meaningful for both TPC and HBD.
C===============================================================================
            DO K = 1,3
               SINGLE_HIT( K) = VECT( K)
            END DO
C===============================================================================
C    Initialise energy loss.  This will be meaningful only for TPC.
C===============================================================================
            SINGLE_HIT(12) = 0.0
C===============================================================================
C    Record entry Px, Py, Pz.  This will be meaningful for both TPC and HBD.
C===============================================================================
            SINGLE_HIT(4) = VECT(7)*VECT(4)
            SINGLE_HIT(5) = VECT(7)*VECT(5)
            SINGLE_HIT(6) = VECT(7)*VECT(6)
         ELSE IF (INWVOL .EQ. 2 .OR. ISTOP .NE. 0) THEN
C===============================================================================
C    Particle is exiting the volume, or has stopped
C===============================================================================
C    Record TOF and particle ID, meaningful for both TPC and HBD.
C===============================================================================
            SINGLE_HIT(7) = TOFG*1.e9       ! time of flight, converted to ns
            SINGLE_HIT(8) = FLOAT(IPART)    ! Particle ID
C===============================================================================
C    Record exit position.  This will be kept for TPC pad rows and HBD radiator gas volumes.
C===============================================================================
            DO K = 1,3
               SINGLE_HIT(K+8) = VECT( K)
            END DO
C===============================================================================
C    Record total energy loss and path length.  These will be meaningful for TPC only.
C===============================================================================
            SINGLE_HIT(12) = SINGLE_HIT(12) + DESTEP  ! Total accumulated energy loss
            SINGLE_HIT(13) = SLENG                    ! current path length (cm)
C===============================================================================
C    Now store values for current hit in structure JHITS
C===============================================================================
            CALL GSAHIT ( ISET, IDET, ITRA, NUMBV, SINGLE_HIT, IERROR )
            IF(IERROR.EQ.0)THEN
               WRITE(6,999)IERROR
 999           FORMAT(/,' HBD_GUSTEP <E>: failure in call to GSAHIT ',
     +              /,' return value was IERROR = ', i10,/)
               STOP  ' PISA is stopping in HBD_GUSTEP'
            ENDIF               !  check for successful store
         END IF                 ! Entering, exiting, stopping in the volume, etc.
C===============================================================================
*         if (NGPHOT.gt.0) then  ! If Cerenkov photons were produced, put then on the stack
*            call GSKPHO(0)
*         endif                  ! Cerenkov photons were produced
C===============================================================================
      endif                     ! Check on FLOAT(IPART) = 50 (Cerenkov photon)
      RETURN
      END
