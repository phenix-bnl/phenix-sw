//-----------------------------------------------------------------------------
//
//  Implementation of the classes Mom3 and Mom4
//
//-----------------------------------------------------------------------------

#include <TMath.h>
#include <cmath>
#include "Momentum.h"

Mom3::Mom3():
  itsPx(0), itsPy(0), itsPz(0)
{ ; }

Mom3::Mom3(const double px, const double py, const double pz):
  itsPx(px), itsPy(py), itsPz(pz)
{ ; }

void Mom3::Set(const double px, const double py, const double pz)
{
  itsPx = px;
  itsPy = py;
  itsPz = pz;
}

double Mom3::Theta() const {
  return (Abs()!=0) ? std::acos(Getpz()/Abs()) : 0;
}

double Mom3::Phi() const {
  return Getpy()>=0 ?
    std::atan2(Getpy(),Getpx()) :
    std::atan2(Getpy(),Getpx())+2.0*TMath::Pi();
}

double Mom3::Abs() const {
  return std::sqrt(itsPx*itsPx + itsPy*itsPy + itsPz*itsPz);
}

Mom3 Mom3::operator+=(Mom3 const& rhs)
{
  itsPx += rhs.Getpx();
  itsPy += rhs.Getpy();
  itsPz += rhs.Getpz();
  return *this;
}

Mom3 Mom3::operator-=(Mom3 const& rhs)
{
  itsPx -= rhs.Getpx();
  itsPy -= rhs.Getpy();
  itsPz -= rhs.Getpz();
  return *this;
}

double Mom3::operator* (const Mom3 & rhs) const
{
  return itsPx*rhs.Getpx() + itsPy*rhs.Getpy() + itsPz*rhs.Getpz();
}

Mom3 Mom3::operator*=(double const& rhs)
{
  itsPx *= rhs;
  itsPy *= rhs;
  itsPz *= rhs;

  return *this;
}

Mom4::Mom4():
  Mom3(), itsE(0)
{ ; }

Mom4::Mom4(const double E, const Mom3& p):
  Mom3(p.Getpx(), p.Getpy(), p.Getpz()), itsE(E)
{ ; }

Mom4::Mom4(const double E,
    const double px, const double py, const double pz):
  Mom3(px,py,pz), itsE(E)
{ ; }

double Mom4::Abs() const {
  const double abs2 = *(this) * *(this);
  if (abs2>=0)
    return std::sqrt(abs2);
  else
    return 0;
}

Mom4 Mom4::operator+=(Mom4 const& rhs)
{
  itsPx += rhs.Getpx();
  itsPy += rhs.Getpy();
  itsPz += rhs.Getpz();
  itsE  += rhs.GetE();
  return *this;
}

Mom4 Mom4::operator-=(Mom4 const& rhs)
{
  itsPx -= rhs.Getpx();
  itsPy -= rhs.Getpy();
  itsPz -= rhs.Getpz();
  itsE  -= rhs.GetE();
  return *this;
}

Mom4 Mom4::operator*=(double const& a) {
  itsPx *= a;
  itsPy *= a;
  itsPz *= a;
  itsE  *= a;
  return *this;
}

double Mom4::operator* (const Mom4&rhs) const
{
  return itsE*rhs.GetE() - Mom3(*this)*rhs;
}
