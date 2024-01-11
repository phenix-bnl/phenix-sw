#include "PHCentralTrackv23.h"
#include "PHSnglCentralTrackv23.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv23)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv23::PHCentralTrackv23(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv23",PHNTRACKS);
}
PHCentralTrackv23::PHCentralTrackv23(const PHCentralTrackv23& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv23&
PHCentralTrackv23::operator=(const PHCentralTrackv23& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv23::copyto(PHCentralTrackv23& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv23",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv23* src = static_cast<PHSnglCentralTrackv23*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv23* d = static_cast<PHSnglCentralTrackv23*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv23*
PHCentralTrackv23::clone() const
{
  return new PHCentralTrackv23(*this);
}


PHCentralTrackv23::~PHCentralTrackv23()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv23::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv23 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv23::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv23::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv23::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv23::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv23();
  return;
}

void  PHCentralTrackv23::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
