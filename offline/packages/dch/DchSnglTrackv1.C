#include "DchSnglTrackv1.h"

ClassImp(DchSnglTrackv1)

void DchSnglTrackv1::Init()
{
  for (int i=0; i<40; i++) 
    {
      hits[i] = -1;
    }

  x = 0.;
  y = 0.;
  z = 0.;
  dirx = 0.;
  diry = 0.;
  dirz = 0.;

  trackid = -1;
  arm = -1;
  side = -1;
  quality = 0;
  
  phi = -999.9;
  alpha = -999.9;
  beta = -999.9;
  betaNoVertex = -999.9;
  zed = -999.9;
  phi0 = -999.9;
  theta0 = -999.9;
  momentum = -999.9;
  //
  status = 0;
  alpha1 = -100.;
  alpha2 = -100.;
  chi21  = -1.;
  chi22  = -1.;
  dist1  = 100.;
  dist2  = 100.;
}

DchSnglTrackv1::DchSnglTrackv1()
{
  Init();
}

DchSnglTrackv1::DchSnglTrackv1(DchSnglTrackv1*track)
{
  Init();
  if(!track) return;

  for (int i=0; i<40; i++) 
    {
      hits[i] = track->get_hits(i);
    }
  PHPoint p = track->get_point();
  x  = p.getX(); 
  y = p.getY();  
  z = p.getZ();
  PHVector  v = track->get_direction();
  dirx  = v.getX();  
  diry  = v.getY(); 
  dirz = v.getZ();
  trackid = track->get_trackid();
  arm     = track->get_arm();
  side    = track->get_side();
  quality = track->get_quality();
  
  phi     = track->get_phi();
  alpha   = track->get_alpha();
  beta    = track->get_beta();
  betaNoVertex = track->get_betaNoVertex();
  zed     = track->get_zed();
  phi0    = track->get_phi0();
  theta0  = track->get_theta0();
  momentum= track->get_momentum(); 
  status  = track->get_status();
  alpha1  = track->get_alpha1();
  alpha2  = track->get_alpha2();
  chi21   = track->get_chi21();
  chi22   = track->get_chi22();
  dist1   = track->get_dist1();
  dist2   = track->get_dist2();
}

short  DchSnglTrackv1::get_nx1hits()
{
  short ret =0;
  for (int i=0; i<12; i++) 
    {
      if(hits[i]>-1) ret++;
    }
  return ret;
}

short  DchSnglTrackv1::get_nx2hits()
{
  /*call this function assume hit is in right place.
  */
  short ret =0;
  for (int i=20; i<32; i++) 
    {
      if(hits[i]>-1) ret++;
    }
  return ret;
}
