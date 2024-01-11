
#include "AccSnglHitv1.h"

ClassImp(AccSnglHitv1)

AccSnglHitv1::AccSnglHitv1()
{

  boxid = -9999;
  npe   = -9999.;
  tof   = -9999.;
  tdiff = -9999.;
  for(int i=0;i<3;i++){
    xyz[i] = -9999.;
  }

}

AccSnglHitv1::AccSnglHitv1(AccSnglHitv1* track)
{

  if(!track) return;

  boxid = track->get_boxid();
  npe   = track->get_npe();
  tof   = track->get_tof();
  tdiff = track->get_tdiff();
  for(int i=0;i<3;i++){
    xyz[i] = track->get_xyz(i);
  }

}

