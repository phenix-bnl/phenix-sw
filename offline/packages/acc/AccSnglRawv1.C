
#include "AccSnglRawv1.h"

ClassImp(AccSnglRawv1)

AccSnglRawv1::AccSnglRawv1()
{
  boxid = -9999;
  for(int i=0;i<2;i++){
    adc[i] = -9999;
    tdc[i] = -9999;
    adc_post[i] = -9999;
    adc_pre[i] = -9999;
  }

  return;
}

AccSnglRawv1::AccSnglRawv1(AccSnglRawv1* track)
{

  if(!track) return;

  boxid = track->get_boxid();
  for(int i=0;i<2;i++){
    adc[i] = track->get_adc(i);
    tdc[i] = track->get_tdc(i);
    adc_post[i] = track->get_adcpost(i);
    adc_pre[i] = track->get_adcpre(i);
  }

  return;
}

