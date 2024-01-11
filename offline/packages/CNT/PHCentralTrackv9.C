#include "PHCentralTrackv9.h"
#include "PHSnglCentralTrackv9.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(PHCentralTrackv9)

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv9::PHCentralTrackv9(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv9",PHNTRACKS);

  return;
}

PHCentralTrackv9::~PHCentralTrackv9()
{
  Central->Clear();
  delete Central;
  return;
}


void PHCentralTrackv9::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv9 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv9::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv9::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv9::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv9::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv9();
  return;
}

void  PHCentralTrackv9::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}
