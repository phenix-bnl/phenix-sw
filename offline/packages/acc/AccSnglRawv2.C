
#include "AccSnglRawv2.h"

ClassImp(AccSnglRawv2)

using namespace std;

AccSnglRawv2::AccSnglRawv2()
{
  boxid = -9999;
  for(int i=0;i<2;i++){
    adc[i] = -9999;
    tdc[i] = -9999;
    adc_post[i] = -9999;
  }

  return;
}

AccSnglRawv2::AccSnglRawv2(AccSnglRawv2* track)
{

  if(!track) return;

  boxid = track->get_boxid();
  for(int i=0;i<2;i++){
    adc[i] = track->get_adc(i);
    tdc[i] = track->get_tdc(i);
    adc_post[i] = track->get_adcpost(i);
  }

  return;
}

void
AccSnglRawv2::identify(ostream& os) const
{
  os << "AccSnglRawv2 Object:" << endl;
  AccSnglRaw::identify(os);
  return;
}
