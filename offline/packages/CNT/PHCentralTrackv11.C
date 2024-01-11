#include "PHCentralTrackv11.h"
#include "PHSnglCentralTrackv11.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv11)

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv11::PHCentralTrackv11(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv11",PHNTRACKS);
}

PHCentralTrackv11::PHCentralTrackv11(const PHCentralTrackv11& rhs)
{
  Central=0;
  rhs.copyto(*this);
}

PHCentralTrackv11& 
PHCentralTrackv11::operator=(const PHCentralTrackv11& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
PHCentralTrackv11::copyto(PHCentralTrackv11& dest) const
{
  delete dest.Central;
  dest.Central = new TClonesArray("PHSnglCentralTrackv11",nCentral);
  dest.nCentral = nCentral;
  for ( unsigned int i = 0; i < nCentral; ++i ) 
    {
      PHSnglCentralTrackv11* src = static_cast<PHSnglCentralTrackv11*>
	(get_track(i));
      if ( src ) 
	{
	  dest.AddPHParticle(i);
	  PHSnglCentralTrackv11* d = static_cast<PHSnglCentralTrackv11*>
	    (dest.get_track(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

PHCentralTrackv11* 
PHCentralTrackv11::clone() const
{
  return new PHCentralTrackv11(*this);
}

PHCentralTrackv11::~PHCentralTrackv11()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv11::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv11 Object\n"
     << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv11::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv11::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv11::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv11::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv11();
  return;
}

void  PHCentralTrackv11::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
