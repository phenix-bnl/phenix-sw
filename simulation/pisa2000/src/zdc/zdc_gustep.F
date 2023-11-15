c $Id: zdc_gustep.F,v 1.1 2008/05/21 08:22:22 hpereira Exp $
******************************************************************
* RHIC-PHENIX ZDC detector simulation.
* GEANT tracking user routines.
* Author: Andras Ster, KFKI/Hungary
* Creation date: 20.April.1999
******************************************************************

      !****************************
      SUBROUTINE zdc_gustep

***   Routine called at each step of tracking

      IMPLICIT NONE

#include "gctrak.inc"
#include "gctmed.inc"
#include "gcmate.inc"
#include "gckine.inc"
#include "geant321/gcking.inc"
#include "subevt.inc"
#include "event.inc"
#include "gcvolu.inc"
#include "gcsets.inc"
 

      COMMON /spectator/ spectator_particles(maxptl), I_spectator,
     +n_participant_part
      INTEGER spectator_particles, I_spectator, n_participant_part

      COMMON/XYZ_MOTHER/XYZ_M(3),TILTE_ANG
      REAL XYZ_M,TILTE_ANG
 
      COMMON/ZDC_DEF/
     +      N_MOD,N_LAY,N_FIB,
     +      N_FIB_FST,N_LAY_FST,N_ABS_FST,N_PLA_FST,
     +      T_PLA,H_PLA,W_PLA,M_PLA,
     +      T_ABS,H_ABS,W_ABS,M_ABS,
     +      D_FIB,H_FIB,W_FIB,M_FIB,
     +      AGAP, T_GAP,H_GAP,W_GAP,M_GAP,
     +      FIB_LEN_LAY(27)
      INTEGER N_MOD,N_LAY,N_FIB,
     +      N_FIB_FST,N_LAY_FST,N_ABS_FST,N_PLA_FST,
     +      M_PLA,M_ABS,M_FIB,M_GAP
      REAL  T_PLA,H_PLA,W_PLA,
     +      T_ABS,H_ABS,W_ABS,
     +      D_FIB,H_FIB,W_FIB,
     +      AGAP, T_GAP,H_GAP,W_GAP,
     +      FIB_LEN_LAY

      INTEGER N_ADC
      PARAMETER (N_ADC=5)
      COMMON/L_VAR/
     + PH_TOT03(N_ADC),PH_TOT04(N_ADC),PH_TOT05(N_ADC),
     + PH_ELE03(N_ADC),PH_ELE04(N_ADC),PH_ELE05(N_ADC),
     + PH_HAD03(N_ADC),PH_HAD04(N_ADC),PH_HAD05(N_ADC),
     + HITS_TOT
      REAL
     + PH_TOT03,PH_TOT04,PH_TOT05,
     + PH_ELE03,PH_ELE04,PH_ELE05,
     + PH_HAD03,PH_HAD04,PH_HAD05,
     + HITS_TOT

      INTEGER    NUM_AP
      PARAMETER (NUM_AP=3)
      INTEGER N_PHOT(NUM_AP)
      REAL    AV_PHOT(NUM_AP)

      REAL part_imp, beta, aaa, fx, fy, fz, angle

      COMMON /zdchit/ ihit   ! this common block is not used ??
      INTEGER ihit

      INTEGER north_south, imod, it

      integer ih
      REAL    HITSD(11)    ! CFM change to 11 from 15

      integer kdebug /0/


*     PhotoElectrons generation for different fiber's NA:

*     Check in of FIBER's hit:

      IF(charge .EQ. 0.0) RETURN
c
c     CFM change; was originally  IF(inwvol.NE.1)RETURN
c
c???  IF(inwvol .NE. 0 .and. inwvol .ne. 2) RETURN
      IF(inwvol .NE. 1) RETURN
      IF(nmat .NE. m_fib) RETURN

      part_imp=vect(7)
      beta=part_imp/getot
      aaa=tilte_ang/57.29577951
      fx=0.0
      fy=SIN(AAA)
      fz=COS(AAA)

      IF(p4vec(3, iabs(I_spectator)) .LT. 0.0) fz = -fz

      angle=ACOS(fx*vect(4)+fy*vect(5)+fz*vect(6))
      angle=angle*57.29577951

*     Calculate the number of photoelectrons

      CALL uphotg(beta, angle, ipart, n_phot(1), av_phot(1))

      ph_tot05(n_adc)    = ph_tot05(n_adc)  +n_phot(3)
      IF(I_spectator .GT. 0)
     + ph_tot05(n_adc-1) = ph_tot05(n_adc-1)+n_phot(3)
      IF(gtype(iabs(I_spectator)) .EQ. 13)
     + ph_tot05(n_adc-2) = ph_tot05(n_adc-2)+n_phot(3)

      IF(n_phot(3) .EQ. 0) RETURN

      IF(fz .LT. 0.0) north_south = -1
      IF(fz .GE. 0.0) north_south =  1
      imod  = 1
      ihit  = ihit + 1

      HITSD( 1) = VECT(1)
      HITSD( 2) = VECT(2)
      HITSD( 3) = VECT(3)
      HITSD( 4) = float(n_phot(3))
      HITSD( 5) = VECT(4) * VECT(7)
      HITSD( 6) = VECT(5) * VECT(7)
      HITSD( 7) = VECT(6) * VECT(7)
      HITSD( 8) = TOFG
      HITSD( 9) = FLOAT(IPART)
      HITSD(10) = FLOAT(north_south)
      HITSD(11) = FLOAT(imod)
c     HITSD(12) = FLOAT(ihit)
c     HITSD(13) = FLOAT(ITRA)
c     HITSD(14) = FLOAT(ifile)
c     HITSD(15) = FLOAT(NSUB_EVT)

      if(kdebug.eq.1)then
         write(6,12347)numed, vect(1), vect(2), vect(3), itra,
     +         57.293*acos(vect(3)/sqrt(vect(1)*vect(1) +
     +         vect(2)*vect(2) + vect(3)*vect(3)))
12347 format(' ZDC numed = ',i4,2x,'x,y,z = ',2g10.2,g13.5,
     +                   ' it =',i6,'  theta = ', g13.5)
         write(6,12348)(hitsd(ih),ih=4,11)
12348 format(/, 'HITSD 4 to 11 ',2(/,2x,4e14.5))
      endif  ! debug printing

      CALL gsahit(ISET,IDET,ITRA,NUMBV,hitsd,it)    ! store hit in

      if(it.le.0)then
         write(6,123)IT,ISET,IDET,ITRA
123      format(/,' ZDC_GUSTEP <W>: error flag from GSAHIT = ',i5,
     +          /,' ISET,IDET,ITRA = ',3i10)
      endif  ! check on successful store

      END
