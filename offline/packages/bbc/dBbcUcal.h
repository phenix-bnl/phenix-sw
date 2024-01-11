#ifndef __DBBCUCAL_H__
#define __DBBCUCAL_H__


typedef struct {
   short Pmt;
   float AdcChGain;
   float TdcChGain0;
   float TdcChGain1;
   float TdcOffset0;
   float TdcOffset1;
   float TdcOver0_mean;
   float TdcOver0_sigma;
   float TdcOver1_mean;
   float TdcOver1_sigma;
   float TdcThreshold0;
   float TdcThreshold1;
   float PulseHeightReso;
   float PMTGain;
   float Pedestal;
   float AdcGainFac;
   float SlewParA;
   float SlewParA0;
   float SlewParB0;
   float SlewParC0;
   float SlewParA1;
   float SlewParB1;
   float SlewParC1;
   float Z0overC_off;     
   float dif_off;     
   float MeanTDC_off;     
   float ThresholdFactor;     
   float NoiseHeight;
   float NoiseHitProb;
   float TimeReso;
   float FakePede_mean;
   float FakePede_sigma;
} DBBCUCAL_ST;
#endif /*__DBBCUCAL_H__*/