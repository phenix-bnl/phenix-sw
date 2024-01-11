#include "PHCentralTrackv14.h"
#include "PHSnglCentralTrackv14.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv14)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv14::PHCentralTrackv14(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv14",PHNTRACKS);
}
PHCentralTrackv14::PHCentralTrackv14(const PHCentralTrackv14& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv14&
PHCentralTrackv14::operator=(const PHCentralTrackv14& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv14::copyto(PHCentralTrackv14& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv14",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv14* src = static_cast<PHSnglCentralTrackv14*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv14* d = static_cast<PHSnglCentralTrackv14*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv14*
PHCentralTrackv14::clone() const
{
  return new PHCentralTrackv14(*this);
}


PHCentralTrackv14::~PHCentralTrackv14()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv14::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv14 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv14::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv14::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv14::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv14::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv14();
  return;
}

void  PHCentralTrackv14::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
