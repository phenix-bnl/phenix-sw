#include "PHCentralTrackv17.h"
#include "PHSnglCentralTrackv17.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv17)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv17::PHCentralTrackv17(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv17",PHNTRACKS);
}
PHCentralTrackv17::PHCentralTrackv17(const PHCentralTrackv17& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv17&
PHCentralTrackv17::operator=(const PHCentralTrackv17& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv17::copyto(PHCentralTrackv17& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv17",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv17* src = static_cast<PHSnglCentralTrackv17*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv17* d = static_cast<PHSnglCentralTrackv17*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv17*
PHCentralTrackv17::clone() const
{
  return new PHCentralTrackv17(*this);
}


PHCentralTrackv17::~PHCentralTrackv17()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv17::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv17 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv17::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv17::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv17::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv17::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv17();
  return;
}

void  PHCentralTrackv17::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
