#include "PHCentralTrackv10.h"
#include "PHSnglCentralTrackv10.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv10)

using namespace std;
static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv10::PHCentralTrackv10(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv10",PHNTRACKS);
}

PHCentralTrackv10::PHCentralTrackv10(const PHCentralTrackv10& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv10& 
PHCentralTrackv10::operator=(const PHCentralTrackv10& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv10::copyto(PHCentralTrackv10& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv10",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i ) 
    {
      PHSnglCentralTrackv10* src = static_cast<PHSnglCentralTrackv10*>
	(get_track(i));
      if ( src ) 
	{
	  dest.AddPHParticle(i);
	  PHSnglCentralTrackv10* d = static_cast<PHSnglCentralTrackv10*>
	    (dest.get_track(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

PHCentralTrackv10* 
PHCentralTrackv10::clone() const
{
  return new PHCentralTrackv10(*this);
}

PHCentralTrackv10::~PHCentralTrackv10()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv10::identify(std::ostream& os) const
{
  os << "identify yourself: PHCentralTrackv10 Object\n"
     << "No of Tracks: " << nCentral << std::endl;
  return;
}

void PHCentralTrackv10::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv10::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv10::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv10::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv10();
  return;
}

void  PHCentralTrackv10::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}

