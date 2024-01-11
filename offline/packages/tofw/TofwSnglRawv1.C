#include "TofwSnglRawv1.h"

ClassImp(TofwSnglRawv1)

TofwSnglRawv1::TofwSnglRawv1()
{
  stripid = -9999;
  chamberid = -9999;
  boxid = -9999;
  for(int i=0;i<2;i++)
    {
      t3[i] = -9999;
      t4[i] = -9999;
      q1[i] = -9999;
      q3[i] = -9999;
      tvc[i] = -9999;
      qvc[i] = -9999;
    }
  
  return;
}

TofwSnglRawv1::TofwSnglRawv1(TofwSnglRawv1* track)
{
  
  if(!track) return;
  
  stripid = track->get_stripid();
  chamberid = track->get_chamberid();
  boxid = track->get_boxid();
  for(int i=0;i<2;i++)
    {
      t3[i] = track->get_t3(i);
      t4[i] = track->get_t4(i);
      q1[i] = track->get_q1(i);
      q3[i] = track->get_q3(i);
      tvc[i] = track->get_tvc(i);
      qvc[i] = track->get_qvc(i);
    }
  
  return;
}
