#include <PHParticle.h>
#include <phool.h>

#include <iostream>

ClassImp(PHParticle)

using namespace std;

static int shutup = 0;

void
PHParticle::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int
PHParticle::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
  return 0;
}

void
PHParticle::identify(ostream &os) const
{
  os << "identify yourself: virtual PHParticle object" << endl;
  return ;
}

void
PHParticle::set_npart(const unsigned int /*npart*/)
{
 warning("set_npart(const unsigned int npart)");
 return;
}

unsigned int
PHParticle::get_npart() const
{
 warning("get_npart()");
 return 0;
}

void
PHParticle::set_px(const unsigned int /*itrk*/, const float /*val*/)
{
  warning("set_px(const unsigned int itrk, const float val)");
   return;
}

float
PHParticle::get_px(const unsigned int /*itrk*/) const
{
  warning("get_px(const unsigned int itrk)");
   return NAN;
}

void
PHParticle::set_py(const unsigned int /*itrk*/, const float /*val*/)
{
  warning("set_py(const unsigned int itrk, const float val)");
   return;
}

float
PHParticle::get_py(const unsigned int /*itrk*/) const
{
  warning("get_py(const unsigned int itrk)");
   return NAN;
}

void
PHParticle::set_pz(const unsigned int /*itrk*/, const float /*val*/)
{
  warning("set_pz(const unsigned int itrk, const float val)");
   return;
}

float
PHParticle::get_pz(const unsigned int /*itrk*/) const
{
  warning("get_pz(const unsigned int itrk)");
   return NAN;
}

void
PHParticle::set_E(const unsigned int /*itrk*/, const float /*val*/)
{
  warning("set_E(const unsigned int itrk, const float val)");
   return;
}

float
PHParticle::get_E(const unsigned int /*itrk*/) const
{
  warning("get_E(const unsigned int itrk)");
   return NAN;
}

void
PHParticle::set_charge(const unsigned int /*itrk*/, const short /*val*/)
{
  warning("set_charge(const unsigned int itrk, const short val)");
   return;
}

short
PHParticle::get_charge(const unsigned int /*itrk*/) const
{
  warning("get_charge(const unsigned int itrk)");
   return -9999;
}

void
PHParticle::set_PID(const unsigned int /*itrk*/, const short /*val*/)
{
  warning("set_PID(const unsigned int itrk, const short val)");
   return;
}

short
PHParticle::get_PID(const unsigned int /*itrk*/) const
{
  warning("get_PID(const unsigned int itrk)");
   return -9999;
}



void
PHParticle::warning(const char *fname) const
{
  if (!shutup)
    {
      cout << PHWHERE << "using virtual function " << fname
	   << " doing nothing" << endl;
    }
  return ;
}

void
PHParticle::ShutUp(const int i)
{
  shutup = i;
}

PHParticle* 
PHParticle::clone() const
{ 
  std::cout << "clone(particle) not implemented"  << std::endl;
  return 0;
}
