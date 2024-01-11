#include "PHCentralTrackv15.h"
#include "PHSnglCentralTrackv15.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv15)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv15::PHCentralTrackv15(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv15",PHNTRACKS);
}
PHCentralTrackv15::PHCentralTrackv15(const PHCentralTrackv15& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv15&
PHCentralTrackv15::operator=(const PHCentralTrackv15& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv15::copyto(PHCentralTrackv15& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv15",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv15* src = static_cast<PHSnglCentralTrackv15*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv15* d = static_cast<PHSnglCentralTrackv15*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv15* PHCentralTrackv15::clone() const
{
  return new PHCentralTrackv15(*this);
}


PHCentralTrackv15::~PHCentralTrackv15()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv15::identify(ostream& os) const
{
  os << "PHCentralTrackv15 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv15::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv15::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv15::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv15::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv15();
  return;
}

void  PHCentralTrackv15::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}

