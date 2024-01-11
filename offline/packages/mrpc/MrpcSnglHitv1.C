#include <MrpcSnglHitv1.h>

ClassImp(MrpcSnglHitv1)

MrpcSnglHitv1::MrpcSnglHitv1()
{
  slatid   = -9999;
  time     = -9999.;
  time_dig = -9999.;
  charge   = -9999.;
  for(int i=0;i<3;i++)
    {
      xyz[i] = -9999.;
    }
}

MrpcSnglHitv1::MrpcSnglHitv1(MrpcSnglHitv1* track)
{
  if(!track) return;

  slatid    = track->get_slatid();
  time      = track->get_time();
  time_dig  = track->get_time_dig();
  charge    = track->get_charge();
  for(int i=0;i<3;i++)
    {
      xyz[i] = track->get_xyz(i);
    }
}
