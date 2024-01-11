#include "TofwSnglHitv1.h"

ClassImp(TofwSnglHitv1)

TofwSnglHitv1::TofwSnglHitv1()
{
  boxid = -9999;
  chamberid = -9999;
  nstrip = -9999;
  max_strip = -9999;
  for(int k=0; k<4; k++){
    stripid[k]   = -9999;
    time[k]     = -9999.;
    charge[k]   = -9999.;
    
    for(int i=0; i<2; i++){
      rawadc[k][i]= -9999.;
      rawtdc[k][i]= -9999.;
    }
    
  for(int i=0;i<3;i++)
    {
      xyz[k][i] = -9999.;
    }
  }
}

TofwSnglHitv1::TofwSnglHitv1(TofwSnglHitv1* hit)
{
  if(!hit) return;
  boxid = hit->get_boxid();
  chamberid = hit->get_chamberid();
  nstrip    = hit->get_nstrip();
  max_strip = hit->get_max();

  for(int istrip=0; istrip<nstrip; istrip++)
    {
      stripid[istrip]   = hit->get_stripid(istrip);
      time[istrip]      = hit->get_time(istrip);
      charge[istrip]    = hit->get_charge(istrip);
      
      for(int i=0;i<2;i++){
	rawadc[istrip][i] = hit->get_rawadc(istrip,i);
	rawtdc[istrip][i] = hit->get_rawtdc(istrip,i);
      }
      
      for(int i=0;i<3;i++)
	{
	  xyz[istrip][i] = hit->get_xyz(istrip,i);
	}
    }
}
