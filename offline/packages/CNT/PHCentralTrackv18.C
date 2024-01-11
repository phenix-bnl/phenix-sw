#include "PHCentralTrackv18.h"
#include "PHSnglCentralTrackv18.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv18)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv18::PHCentralTrackv18(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv18",PHNTRACKS);
}
PHCentralTrackv18::PHCentralTrackv18(const PHCentralTrackv18& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv18&
PHCentralTrackv18::operator=(const PHCentralTrackv18& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv18::copyto(PHCentralTrackv18& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv18",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv18* src = static_cast<PHSnglCentralTrackv18*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv18* d = static_cast<PHSnglCentralTrackv18*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv18*
PHCentralTrackv18::clone() const
{
  return new PHCentralTrackv18(*this);
}


PHCentralTrackv18::~PHCentralTrackv18()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv18::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv18 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv18::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv18::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv18::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv18::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv18();
  return;
}

void  PHCentralTrackv18::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
