#include "PHCentralTrackv16.h"
#include "PHSnglCentralTrackv16.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv16)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv16::PHCentralTrackv16(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv16",PHNTRACKS);
}
PHCentralTrackv16::PHCentralTrackv16(const PHCentralTrackv16& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv16&
PHCentralTrackv16::operator=(const PHCentralTrackv16& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv16::copyto(PHCentralTrackv16& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv16",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv16* src = static_cast<PHSnglCentralTrackv16*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv16* d = static_cast<PHSnglCentralTrackv16*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv16*
PHCentralTrackv16::clone() const
{
  return new PHCentralTrackv16(*this);
}


PHCentralTrackv16::~PHCentralTrackv16()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv16::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv16 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv16::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv16::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv16::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv16::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv16();
  return;
}

void  PHCentralTrackv16::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
