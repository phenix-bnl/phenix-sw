#ifndef __DCRKCAL_H__
#define __DCRKCAL_H__


typedef struct {
   short pmt;
   float adc_gain;
   float adc_ped;
   float tdc_clock;
   float tdc_t0;
   float slew;
} DCRKCAL_ST;
#endif /*__DCRKCAL_H__*/
