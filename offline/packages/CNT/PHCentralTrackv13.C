#include "PHCentralTrackv13.h"
#include "PHSnglCentralTrackv13.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv13)

using namespace std;

static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv13::PHCentralTrackv13(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv13",PHNTRACKS);
}
PHCentralTrackv13::PHCentralTrackv13(const PHCentralTrackv13& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv13&
PHCentralTrackv13::operator=(const PHCentralTrackv13& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv13::copyto(PHCentralTrackv13& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv13",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv13* src = static_cast<PHSnglCentralTrackv13*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv13* d = static_cast<PHSnglCentralTrackv13*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          cerr << PHWHERE << "src particle is null ?" << endl;
        }
    }
}

PHCentralTrackv13*
PHCentralTrackv13::clone() const
{
  return new PHCentralTrackv13(*this);
}


PHCentralTrackv13::~PHCentralTrackv13()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv13::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv13 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv13::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv13::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv13::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv13::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv13();
  return;
}

void  PHCentralTrackv13::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
