#ifndef __DBBCCAL_H__
#define __DBBCCAL_H__


typedef struct {
   short Pmt;
   float AdcChGain;
   float TdcChGain0;
   float TdcChGain1;
   float Pedestal;
   float AdcGainFac;
   float SlewParA;
   float TdcOffset0;
   float TdcOffset1;
   float MeanTransitTime;
} DBBCCAL_ST;
#endif /*__DBBCCAL_H__*/
