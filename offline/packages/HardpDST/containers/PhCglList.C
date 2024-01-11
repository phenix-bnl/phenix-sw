#include "PhCglList.h"
#include "PhCglSngl.h"
#include "phool.h"
#include "TClonesArray.h"

ClassImp(PhCglList)

using namespace std;

static const unsigned int PHNTRACKS = 400;

// First we implement the "standard functions"...
PhCglList::PhCglList()
{
  nCentral = 0;
  Central = new TClonesArray("PhCglSngl",PHNTRACKS);
}

PhCglList::~PhCglList()
{
  Central->Clear();
  delete Central;
}

void PhCglList::identify(ostream& os) const
{
  os << "identify yourself: PhCglList Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
}

void PhCglList::Reset()
{
  Central->Clear();
  if (nCentral>PHNTRACKS)
    {
      Central->Expand(PHNTRACKS);
    }
  nCentral = 0;
}
