#include "PHCentralTrackv21.h"
#include "PHSnglCentralTrackv21.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv21)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv21::PHCentralTrackv21()
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv21",PHNTRACKS);
}

PHCentralTrackv21::PHCentralTrackv21(const PHCentralTrackv21& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv21&
PHCentralTrackv21::operator=(const PHCentralTrackv21& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv21::copyto(PHCentralTrackv21& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv21",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv21* src = static_cast<PHSnglCentralTrackv21*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv21* d = static_cast<PHSnglCentralTrackv21*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}


PHCentralTrackv21*
PHCentralTrackv21::clone() const
{
  return new PHCentralTrackv21(*this);
}


PHCentralTrackv21::~PHCentralTrackv21()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv21::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv21 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv21::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv21::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv21::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv21::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv21();
  return;
}

void  PHCentralTrackv21::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
