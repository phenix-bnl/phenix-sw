#include "PHCentralTrackv12.h"
//INCLUDECHECKER: Removed this line: #include "PHSnglCentralTrackv12.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv12)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv12::PHCentralTrackv12(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv12",PHNTRACKS);
}
PHCentralTrackv12::PHCentralTrackv12(const PHCentralTrackv12& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv12&
PHCentralTrackv12::operator=(const PHCentralTrackv12& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv12::copyto(PHCentralTrackv12& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv12",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv12* src = static_cast<PHSnglCentralTrackv12*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv12* d = static_cast<PHSnglCentralTrackv12*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv12*
PHCentralTrackv12::clone() const
{
  return new PHCentralTrackv12(*this);
}


PHCentralTrackv12::~PHCentralTrackv12()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv12::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv12 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv12::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv12::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv12::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv12::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv12();
  return;
}

void  PHCentralTrackv12::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
