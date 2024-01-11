#include "PHCentralTrackv3.h"
#include "PHSnglCentralTrackv3.h"
#include "phool.h"
#include "TClonesArray.h"

ClassImp(PHCentralTrackv3)

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv3::PHCentralTrackv3(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv3",PHNTRACKS);

  if(fetch) // then fetch values but for now do nothing
    cout<< PHWHERE <<" Loading values at run time "<<endl;
    else // get default values
      {
	sigmaAlpha = 0.75;
	sigmaMultScatt = 0.57;
	k1 = 84;
	sigmaTOF = 0.140;
	sigmaEMC = 0.340;
	cout << PHWHERE << "Loading default values for momentum and tof resolution, mass centroids" << endl; 
	cout << "Didn't you want them from the database?" << endl;
	cout << "Are you sure your analysis will be correct?" << endl;
	cout<<" sigmaAlpha     =  "<<sigmaAlpha<<" mrad GeV"<<endl;
	cout<<" sigmaMultScatt =  "<<sigmaMultScatt<<" mrad "<<endl;
	cout<<" sigmaTOF       =  "<<sigmaTOF<<" ns "<<endl;
	cout<<" sigmaEMC       =  "<<sigmaEMC<<" ns "<<endl;


      }
  return;
}

PHCentralTrackv3::~PHCentralTrackv3()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv3::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv3 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv3::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv3::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv3::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv3::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv3();
  return;
}

void  PHCentralTrackv3::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}

