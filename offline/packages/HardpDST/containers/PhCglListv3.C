#include "PhCglListv3.h"
#include "PhCglSnglv3.h"
#include "phool.h"
#include "TClonesArray.h"

ClassImp(PhCglListv3)

using namespace std;

static const unsigned int PHNTRACKS = 400;

// First we implement the "standard functions"...
PhCglListv3::PhCglListv3()
{
  nCentral = 0;
  Central = new TClonesArray("PhCglSnglv3",PHNTRACKS);
}

PhCglListv3::PhCglListv3(const PhCglListv3& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PhCglListv3&
PhCglListv3::operator=(const PhCglListv3& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PhCglListv3::copyto(PhCglListv3& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PhCglSnglv3",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PhCglSnglv3* src = static_cast<PhCglSnglv3*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PhCglSnglv3* d = static_cast<PhCglSnglv3*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PhCglListv3*
PhCglListv3::clone() const
{
  return new PhCglListv3(*this);
}


PhCglListv3::~PhCglListv3()
{
  Central->Clear();
  delete Central;
}

void PhCglListv3::identify(ostream& os) const
{
  os << "identify yourself: PhCglListv3 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
}

void PhCglListv3::Reset()
{
  Central->Clear();
  if (nCentral>PHNTRACKS)
    {
      Central->Expand(PHNTRACKS);
    }
  nCentral = 0;
}

int PhCglListv3::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PhCglListv3::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PhCglListv3::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PhCglSnglv3();
  return;
}
