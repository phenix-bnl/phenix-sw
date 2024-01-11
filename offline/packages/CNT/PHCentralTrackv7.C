#include "PHCentralTrackv7.h"
#include "PHSnglCentralTrackv7.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"

ClassImp(PHCentralTrackv7)

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv7::PHCentralTrackv7(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv7",PHNTRACKS);

  return;
}

PHCentralTrackv7::~PHCentralTrackv7()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv7::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv7 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv7::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv7::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv7::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv7::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv7();
  return;
}

void  PHCentralTrackv7::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}

