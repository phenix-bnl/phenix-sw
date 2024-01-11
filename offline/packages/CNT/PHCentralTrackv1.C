#include "PHCentralTrackv1.h"
#include "PHSnglCentralTrackv1.h"
#include "phool.h"
#include "TClonesArray.h"

ClassImp(PHCentralTrackv1);

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv1::PHCentralTrackv1(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv1",PHNTRACKS);

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

PHCentralTrackv1::~PHCentralTrackv1()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv1::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv1 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv1::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv1::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv1::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv1();
  return;
}

void  PHCentralTrackv1::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}


// Implement the determine_PID routine...
float PHCentralTrackv1::determine_PIDtof(const unsigned int itrk, const float m2) const { 
  PHSnglCentralTrackv1 *Particle = (PHSnglCentralTrackv1 *) GetCentral()->UncheckedAt(itrk); 

  if (Particle) {
    float par[3];
    float mass2 = Particle->get_m2tof();
    float t     = Particle->get_ttof();
    float p     = Particle->get_mom();
    float Nsigma=-9999.;
    
    par[0] = sigmaAlpha/k1; // angular resolution
    par[1] = sigmaMultScatt/k1; // multiple scattering 
    par[2] = sigmaTOF;  // tof resolution
    float alpval = par[0]*par[0]*4*mass2*mass2*p*p;
    float msval  = par[1]*par[1]*4*mass2*mass2*(1+mass2/(p*p));
    float tofval = par[2]*par[2]*4*p*p*(mass2+p*p)/t/t;
    float sigma  = sqrt(alpval+msval+tofval);
    
    Nsigma = (mass2-m2)/sigma;
    
    return ( Nsigma );

  }

  return(-999.); 
} 

float PHCentralTrackv1::determine_PIDemc(const unsigned int itrk, const float m2) const { 

  PHSnglCentralTrackv1 *Particle = (PHSnglCentralTrackv1 *) GetCentral()->UncheckedAt(itrk); 

  if (Particle) {
    float par[3];
    float mass2 = Particle->get_m2emc();
    float t     = Particle->get_temc();
    float p     = Particle->get_mom();
    float Nsigma=-9999.;
    
    par[0] = sigmaAlpha/k1; // angular resolution
    par[1] = sigmaMultScatt/k1; // multiple scattering 
    par[2] = sigmaEMC;  // tof resolution
    float alpval = par[0]*par[0]*4*mass2*mass2*p*p;
    float msval  = par[1]*par[1]*4*mass2*mass2*(1+mass2/(p*p));
    float tofval = par[2]*par[2]*4*p*p*(mass2+p*p)/t/t;
    float sigma  = sqrt(alpval+msval+tofval);
    
    Nsigma = (mass2-m2)/sigma;
    
    return ( Nsigma );

  }
  return(-999.); 
} 
