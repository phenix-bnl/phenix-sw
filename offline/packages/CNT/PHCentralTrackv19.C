#include "PHCentralTrackv19.h"
#include "PHSnglCentralTrackv19.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv19)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv19::PHCentralTrackv19(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv19",PHNTRACKS);
}
PHCentralTrackv19::PHCentralTrackv19(const PHCentralTrackv19& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv19&
PHCentralTrackv19::operator=(const PHCentralTrackv19& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv19::copyto(PHCentralTrackv19& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv19",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv19* src = static_cast<PHSnglCentralTrackv19*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv19* d = static_cast<PHSnglCentralTrackv19*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv19*
PHCentralTrackv19::clone() const
{
  return new PHCentralTrackv19(*this);
}


PHCentralTrackv19::~PHCentralTrackv19()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv19::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv19 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv19::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv19::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv19::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv19::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv19();
  return;
}

void  PHCentralTrackv19::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
