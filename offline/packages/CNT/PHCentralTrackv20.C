#include "PHCentralTrackv20.h"
#include "PHSnglCentralTrackv20.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv20)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv20::PHCentralTrackv20()
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv20",PHNTRACKS);
}

PHCentralTrackv20::PHCentralTrackv20(const PHCentralTrackv20& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv20&
PHCentralTrackv20::operator=(const PHCentralTrackv20& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv20::copyto(PHCentralTrackv20& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv20",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv20* src = static_cast<PHSnglCentralTrackv20*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv20* d = static_cast<PHSnglCentralTrackv20*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}


PHCentralTrackv20*
PHCentralTrackv20::clone() const
{
  return new PHCentralTrackv20(*this);
}


PHCentralTrackv20::~PHCentralTrackv20()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv20::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv20 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv20::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv20::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv20::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv20::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv20();
  return;
}

void  PHCentralTrackv20::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
