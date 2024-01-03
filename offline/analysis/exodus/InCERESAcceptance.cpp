//-----------------------------------------------------------------------------
//
//  Check if lepton pair is in the CERES acceptance
//
//-----------------------------------------------------------------------------

#include <cmath>
#include "InCERESAcceptance.h"
#include "Momentum.h"

bool InCERESAcceptance(Mom4 mom1, Mom4 mom2)
{
  double m1  = mom1.Mom4::Abs();
  double E1  = mom1.GetE();
  double p1  = E1>=m1 ? std::sqrt(E1*E1-m1*m1) : 0;
  double px1 = mom1.Getpx();
  double py1 = mom1.Getpy();
  double pz1 = mom1.Getpz();
  double pt1 = std::sqrt(px1*px1+py1*py1);
  double y1  = std::log((E1+pz1)/(E1-pz1))/2.0;

  double m2  = mom2.Mom4::Abs();
  double E2  = mom2.GetE();
  double p2  = E2>=m2 ? std::sqrt(E2*E2-m2*m2) : 0;
  double px2 = mom2.Getpx();
  double py2 = mom2.Getpy();
  double pz2 = mom2.Getpz();
  double pt2 = std::sqrt(px2*px2+py2*py2);
  double y2  = std::log((E2+pz2)/(E2-pz2))/2.0;

  double angle_12 = std::acos((Mom3(mom1)*mom2)/(p1*p2));

  bool result = true;
  if ( pt1<0.05 )           result=false;
  if ( pt2<0.05 )           result=false;
  if ( y1<2.10 || y1>2.65 ) result=false;
  if ( y2<2.10 || y2>2.65 ) result=false;
  if ( angle_12<0.035 )     result=false;

  return result;
}
