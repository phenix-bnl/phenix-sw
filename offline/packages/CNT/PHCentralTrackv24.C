#include "PHCentralTrackv24.h"
#include "PHSnglCentralTrackv24.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv24)


static const unsigned int PHNTRACKS=400;

  // First we implement the "standard functions"...
PHCentralTrackv24::PHCentralTrackv24(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv24",PHNTRACKS);
}
PHCentralTrackv24::PHCentralTrackv24(const PHCentralTrackv24& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv24&
PHCentralTrackv24::operator=(const PHCentralTrackv24& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv24::copyto(PHCentralTrackv24& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv24",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i )
    {
      PHSnglCentralTrackv24* src = static_cast<PHSnglCentralTrackv24*>
        (get_track(i));
      if ( src )
        {
          dest.AddPHParticle(i);
          PHSnglCentralTrackv24* d = static_cast<PHSnglCentralTrackv24*>
            (dest.get_track(i));
          *d = *src;
        }
      else
        {
          std::cerr << PHWHERE << "src particle is null ?" << std::endl;
        }
    }
}

PHCentralTrackv24*
PHCentralTrackv24::clone() const
{
  return new PHCentralTrackv24(*this);
}


PHCentralTrackv24::~PHCentralTrackv24()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv24::identify(std::ostream& os) const
{
  os << "identify yourself: PHCentralTrackv24 Object" << std::endl;
  os << "No of Tracks: " << nCentral << std::endl;
  return;
}

void PHCentralTrackv24::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv24::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv24::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv24::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv24();
  nCentral++;
  return;
}

void  PHCentralTrackv24::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
