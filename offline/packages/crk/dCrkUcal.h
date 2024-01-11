#ifndef __DCRKUCAL_H__
#define __DCRKUCAL_H__


typedef struct {
   short pmt;
   float gain;
   float ped;
   float clock;
   float t0;
   float slew;
   float N0p;
   float P_noise;
   float mean_noise;
   float sigma_pe;
   float sigma_t;
} DCRKUCAL_ST;
#endif /*__DCRKUCAL_H__*/
