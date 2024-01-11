#include "PHCentralTrackv6.h"
#include "PHSnglCentralTrackv6.h"
//INCLUDECHECKER: Removed this line: #include "phool.h"
#include "TClonesArray.h"

ClassImp(PHCentralTrackv6)

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv6::PHCentralTrackv6(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv6",PHNTRACKS);

  if(fetch) // then fetch values but for now do nothing
    cout<< PHWHERE <<" Loading values at run time "<<endl;
    else // get default values
      {
	sigmaAlpha = 0.75;
	sigmaMultScatt = 0.57;
	k1 = 84;
	sigmaTOF = 0.140;
	sigmaEMC = 0.340;
	//cout << PHWHERE << "Loading default values for momentum and tof resolution, mass centroids" << endl; 
	//cout << "Didn't you want them from the database?" << endl;
	//cout << "Are you sure your analysis will be correct?" << endl;
	//cout<<" sigmaAlpha     =  "<<sigmaAlpha<<" mrad GeV"<<endl;
	//cout<<" sigmaMultScatt =  "<<sigmaMultScatt<<" mrad "<<endl;
	//cout<<" sigmaTOF       =  "<<sigmaTOF<<" ns "<<endl;
	//cout<<" sigmaEMC       =  "<<sigmaEMC<<" ns "<<endl;


      }
  return;
}

PHCentralTrackv6::~PHCentralTrackv6()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv6::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv6 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv6::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv6::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv6::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv6::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv6();
  return;
}

void  PHCentralTrackv6::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}

