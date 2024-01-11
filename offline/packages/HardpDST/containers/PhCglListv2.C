#include "PhCglListv2.h"
#include "PhCglSnglv2.h"
#include "phool.h"
#include "TClonesArray.h"

ClassImp(PhCglListv2)

using namespace std;

static const unsigned int PHNTRACKS = 400;

// First we implement the "standard functions"...
PhCglListv2::PhCglListv2()
{
  nCentral = 0;
  Central = new TClonesArray("PhCglSnglv2",PHNTRACKS);
}

PhCglListv2::PhCglListv2(const PhCglListv2& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PhCglListv2&
PhCglListv2::operator=(const PhCglListv2& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PhCglListv2::copyto(PhCglListv2& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PhCglSnglv2",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PhCglSnglv2* src = static_cast<PhCglSnglv2*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PhCglSnglv2* d = static_cast<PhCglSnglv2*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PhCglListv2*
PhCglListv2::clone() const
{
  return new PhCglListv2(*this);
}


PhCglListv2::~PhCglListv2()
{
  Central->Clear();
  delete Central;
}

void PhCglListv2::identify(ostream& os) const
{
  os << "identify yourself: PhCglListv2 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
}

void PhCglListv2::Reset()
{
  Central->Clear();
  if (nCentral>PHNTRACKS)
    {
      Central->Expand(PHNTRACKS);
    }
  nCentral = 0;
}

int PhCglListv2::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PhCglListv2::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PhCglListv2::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PhCglSnglv2();
  return;
}
