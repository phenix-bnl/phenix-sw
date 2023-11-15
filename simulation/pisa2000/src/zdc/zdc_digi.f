      SUBROUTINE zdc_digi

****************************************************************
*  GEANT digitization for ZDC simulations (created from an earlier
*  version of Edmundo Garcia's description file.)
*  Author: Andras Ster
*  Creation date: April 20, 1999
****************************************************************

c    C.H. Pinkenburg Feb. 09, 2001  check on # of hits returned by gfhits
c                                   indicating a too small user hit buffer 
c                                   (and set nhits to size of user buffer,
c                                   gfhits: GEANT manual page156)

      IMPLICIT NONE


#include "event.inc"
#include "subevt.inc"
#include "gckine.inc"

      COMMON/ZDCINP/ dxfield, fragment_model, zdc_hbook
      REAL dxfield
      INTEGER fragment_model, zdc_hbook

      COMMON /spectator/ spectator_particles(maxptl), I_spectator,
     +n_participant_part
      INTEGER spectator_particles, I_spectator, n_participant_part

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

      COMMON/EMCZDC/ r_emczdc
      REAL           r_emczdc

      REAL    PHOTO_CONVERSION
      REAL    E_zdc, E_emc, E_zdc_spectator, E_zdc_neutron
      INTEGER i_evt, first
      DATA    i_evt/0/,first/0/
      SAVE    i_evt, first, PHOTO_CONVERSION

      INTEGER NHITSMAX
      PARAMETER (NHITSMAX=10000)
      INTEGER NHDIM
      PARAMETER (NHDIM=11)
      INTEGER numvs(5), itr(NHITSMAX), numbv(5,NHITSMAX), nhits
      REAL    hits(NHDIM, NHITSMAX)
      INTEGER ihit


      IF (first .eq. 0) THEN

*       Relative light yield (# photoelectrons/TeV)
*       N_ph = 4.2 * Ebeam(GeV) + 4760 (see: arXiv:nucl-ex0008005 8 Aug 2000)

        PHOTO_CONVERSION = 4.2 * sqrt_s / 2.0 + 4760
        first = 1
      ENDIF

      CALL gfhits('ZDC ','MAI1',5,NHDIM,NHITSMAX,
     +            0,numvs,itr,numbv,hits,nhits)

C /chp/ if user array hits exceeded, nhits is returned as nhitsmax+1
      if (nhits .gt. nhitsmax) then
        write(6,*) '<W> ZDC (zdc_digi.f): number of hits exceeds',
     #  nhitsmax,' nhits truncated to ',nhitsmax,' for MAI1'
        nhits = nhitsmax
      end if

      if(nhits.gt.0)then
         do ihit = 1,nhits
            call trkstack(itr(ihit))
         enddo
      endif

      IF(zdc_hbook .NE. 0) THEN

         PRINT *,'  ZDC overflow hits / subevent:', nhits

         IF(end_evtflg .AND. nptls .GT. 0) THEN
	    E_zdc           = ph_tot05(n_adc)     / PHOTO_CONVERSION
	    E_zdc_spectator = ph_tot05(n_adc - 1) / PHOTO_CONVERSION
            E_emc           = r_emczdc

            i_evt = i_evt + 1

            CALL hf1(13102, E_zdc, 1.0)
            CALL hf1(13103, E_zdc_spectator, 1.0)
            CALL hf2(13200, bimevt, E_zdc, 1.0)
            CALL hf2(13210, bimevt, E_zdc_spectator, 1.0)
            CALL hf2(13220, float(n_participant_part), E_zdc, 1.0)
            CALL hf2(13300, E_zdc, E_emc, 1.0)

            CALL vzero(ph_tot05(1), n_adc)
         ENDIF
      ENDIF

      CALL gfhits('ZDC ','MAI2',5,NHDIM,NHITSMAX,
     +            0,numvs,itr,numbv,hits,nhits)

C /chp/ if user array hits exceeded, nhits is returned as nhitsmax+1
      if (nhits .gt. nhitsmax) then
        write(6,*) '<W> ZDC (zdc_digi.f): number of hits exceeds',
     #  nhitsmax,' nhits truncated to ',nhitsmax,' for MAI2'
        nhits = nhitsmax
      end if

      if(nhits.gt.0)then
         do ihit = 1,nhits
            call trkstack(itr(ihit))
         enddo
      endif

      RETURN
      END
