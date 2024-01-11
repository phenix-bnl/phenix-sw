#include "Tools.h"
#include <cmath>

//===================== Compute opening angle between two particles 
Float_t Tools::angle(Float_t p0[3], Float_t p1[3])
{
  Float_t P0 = sqrt(p0[0]*p0[0] + p0[1]*p0[1] + p0[2]*p0[2]); 
  Float_t P1 = sqrt(p1[0]*p1[0] + p1[1]*p1[1] + p1[2]*p1[2]);
  Float_t cost = (p0[0]*p1[0] + p0[1]*p1[1] + p0[2]*p1[2])/(P0*P1);
  Float_t Angle = (180/3.14159)*acos(cost);
  return Angle;
}

//==================================================================
Float_t Tools::angle(Float_t px0, Float_t py0, Float_t pz0,
                     Float_t px1, Float_t py1, Float_t pz1)
{
  Float_t P0 = sqrt(px0*px0 + py0*py0 + pz0*pz0); 
  Float_t P1 = sqrt(px1*px1 + py1*py1 + pz1*pz1);
  Float_t cost = (px0*px1 + py0*py1 + pz0*pz1)/(P0*P1);
  Float_t Angle = (180/3.14159)*acos(cost);
  return Angle;
}
