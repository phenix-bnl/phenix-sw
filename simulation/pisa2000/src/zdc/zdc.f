      SUBROUTINE zdc(full, nh)

******************************************************************
* RHIC-PHENIX ZDC detector simulation main routine in PISA framework
* Author: Andras Ster
* Creation date: 10.Dec.1999
******************************************************************

      IMPLICIT NONE

#include "guphnx.inc"

      CHARACTER*4  full         
      INTEGER*4    nh   
      
      COMMON/ZDCINP/ dxfield, fragment_model, zdc_hbook
      REAL dxfield
      INTEGER fragment_model, zdc_hbook

      INTEGER N_ADC
      PARAMETER (N_ADC=5)
      COMMON/L_VAR/
     + PH_TOT03(N_ADC),PH_TOT04(N_ADC),PH_TOT05(N_ADC),
     + PH_ELE03(N_ADC),PH_ELE04(N_ADC),PH_ELE05(N_ADC),
     + PH_HAD03(N_ADC),PH_HAD04(N_ADC),PH_HAD05(N_ADC),
     + HITS_TOT
       REAL PH_TOT03,PH_TOT04,PH_TOT05,
     + PH_ELE03,PH_ELE04,PH_ELE05,
     + PH_HAD03,PH_HAD04,PH_HAD05,
     + HITS_TOT

      CHARACTER*4  opt

*     ZDC inputs

      dxfield = rvolu_opt(7, 13)   ! pick up DX field from ZDC line 
      opt = cvolu_opt(5, 13)
      fragment_model = 1
      IF(opt .EQ. 'NONE') fragment_model = 0
      opt = cvolu_opt(6, 13)
      zdc_hbook = 0
      IF(opt .EQ. 'HBOO') zdc_hbook = 1

*     ZDC geometry and materials definitions 

      CALL zdc_geom_beam_pipes
      CALL zdc_geom

*     hbooking initialisation  

      IF (zdc_hbook .NE. 0) THEN
      CALL hbook1(13102,'E_zdc',           60,0.0,8.0, 0.0)
      CALL hbook1(13103,'E_zdc_nspec',     60,0.0,8.0, 0.0)
      CALL hbook2(13200,'b vs E_zdc',      60,0.0,20.0, 60,0.0,8., 0.)
      CALL hbook2(13210,'b vs E_zdc_nspec',60,0.0,20.0, 60,0.0,8., 0.)
      CALL hbook2(13220,'N_part vs E_zdc', 60,0.0,500., 60,0.0,8., 0.)
      CALL hbook2(13300,'E_zdc vs E_emc'  ,60,0.0,8.0,  60,0.0,300.,0.)

      CALL vzero(ph_tot05(1), n_adc)
      ENDIF

      RETURN
      END
