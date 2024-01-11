#include "PHCentralTrackv22.h"
#include "PHSnglCentralTrackv22.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv22)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv22::PHCentralTrackv22(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv22",PHNTRACKS);
}
PHCentralTrackv22::PHCentralTrackv22(const PHCentralTrackv22& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv22&
PHCentralTrackv22::operator=(const PHCentralTrackv22& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv22::copyto(PHCentralTrackv22& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv22",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv22* src = static_cast<PHSnglCentralTrackv22*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv22* d = static_cast<PHSnglCentralTrackv22*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv22*
PHCentralTrackv22::clone() const
{
  return new PHCentralTrackv22(*this);
}


PHCentralTrackv22::~PHCentralTrackv22()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv22::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv22 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv22::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv22::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv22::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv22::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv22();
  return;
}

void  PHCentralTrackv22::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
