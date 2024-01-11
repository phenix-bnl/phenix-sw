#include "PHCentralTrackv4.h"
#include "PHSnglCentralTrackv4.h"
#include <phool.h>
#include <TClonesArray.h>

ClassImp(PHCentralTrackv4)

using namespace std;

static const unsigned int PHNTRACKS = 400;

  // First we implement the "standard functions"...
PHCentralTrackv4::PHCentralTrackv4(int fetch)
{
  nCentral = 0;
  Central = new TClonesArray("PHSnglCentralTrackv4",PHNTRACKS);

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

PHCentralTrackv4::~PHCentralTrackv4()
{
  Central->Clear();
  delete Central;
  return;
}

void PHCentralTrackv4::identify(ostream& os) const
{
  os << "identify yourself: PHCentralTrackv4 Object" << endl;
  os << "No of Tracks: " << nCentral << endl;
  return;
}

void PHCentralTrackv4::Reset()
{
 Central->Clear();
 if (nCentral>PHNTRACKS)
   {
     Central->Expand(PHNTRACKS);
   }
 nCentral = 0;
 return;
}

int PHCentralTrackv4::isValid() const
{
  return((nCentral>0) ? 1 : 0);
}

int PHCentralTrackv4::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Central->Expand(nhits);
     }
  return nhits;
}

void  PHCentralTrackv4::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Central;
  new(Particle[itrk]) PHSnglCentralTrackv4();
  return;
}

void  PHCentralTrackv4::RemovePHParticle(const unsigned int itrk)
{
  Central->RemoveAt(itrk);
  return;
}



// Implement the get_quality routine...
short PHCentralTrackv4::get_quality(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_quality() : -999); } 

// Implement the set_quality routine...
void PHCentralTrackv4::set_quality(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_quality(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 
//
//... lvl2 electron match
//
long PHCentralTrackv4::get_categoryl2eLowPt(const unsigned int itrk) const 
{ 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_categoryl2eLowPt() : -999); 
} 

void PHCentralTrackv4::set_categoryl2eLowPt(const unsigned int itrk, const long input) 
{ 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_categoryl2eLowPt(input); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; 
} 

//...................
long PHCentralTrackv4::get_categoryl2eHighPt(const unsigned int itrk) const 
{ 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_categoryl2eHighPt() : -999); 
} 

void PHCentralTrackv4::set_categoryl2eHighPt(const unsigned int itrk, const long input) 
{ 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_categoryl2eHighPt(input); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; 
} 

//...................
short PHCentralTrackv4::get_candIDl2e(const unsigned int itrk) const 
{ 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_candIDl2e() : -999); 
} 

void PHCentralTrackv4::set_candIDl2e(const unsigned int itrk, const short input) 
{ 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_candIDl2e(input); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; 
} 


// Implement the get_zed routine...
float PHCentralTrackv4::get_zed(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_zed() : -999); } 

// Implement the set_zed routine...
void PHCentralTrackv4::set_zed(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_zed(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_phi routine...
float PHCentralTrackv4::get_phi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phi() : -999); } 

// Implement the set_phi routine...
void PHCentralTrackv4::set_phi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_alpha routine...
float PHCentralTrackv4::get_alpha(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alpha() : -999); } 

// Implement the set_alpha routine...
void PHCentralTrackv4::set_alpha(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alpha(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_beta routine...
float PHCentralTrackv4::get_beta(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_beta() : -999); } 

// Implement the set_beta routine...
void PHCentralTrackv4::set_beta(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_beta(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_phi0 routine...
float PHCentralTrackv4::get_phi0(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phi0() : -999); } 

// Implement the set_phi0 routine...
void PHCentralTrackv4::set_phi0(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phi0(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_the0 routine...
float PHCentralTrackv4::get_the0(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_the0() : -999); } 

// Implement the set_the0 routine...
void PHCentralTrackv4::set_the0(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_the0(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_mom routine...
float PHCentralTrackv4::get_mom(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mom() : -999); } 

// Implement the set_mom routine...
void PHCentralTrackv4::set_mom(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mom(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_status routine...
short PHCentralTrackv4::get_status(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_status() : -999); } 

// Implement the set_status routine...
void PHCentralTrackv4::set_status(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_status(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_alpha1 routine...
float PHCentralTrackv4::get_alpha1(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alpha1() : -999); } 

// Implement the set_alpha1 routine...
void PHCentralTrackv4::set_alpha1(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alpha1(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_alpha2 routine...
float PHCentralTrackv4::get_alpha2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alpha2() : -999); } 

// Implement the set_alpha2 routine...
void PHCentralTrackv4::set_alpha2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alpha2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_nx1hits routine...
short PHCentralTrackv4::get_nx1hits(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_nx1hits() : -999); } 

// Implement the set_nx1hits routine...
void PHCentralTrackv4::set_nx1hits(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_nx1hits(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_nx2hits routine...
short PHCentralTrackv4::get_nx2hits(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_nx2hits() : -999); } 

// Implement the set_nx2hits routine...
void PHCentralTrackv4::set_nx2hits(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_nx2hits(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_nx1x2fit routine...
short PHCentralTrackv4::get_nx1x2fit(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_nx1x2fit() : -999); } 

// Implement the set_nx1x2fit routine...
void PHCentralTrackv4::set_nx1x2fit(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_nx1x2fit(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_alphaf routine...
float PHCentralTrackv4::get_alphaf(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alphaf() : -999); } 

// Implement the set_alphaf routine...
void PHCentralTrackv4::set_alphaf(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alphaf(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_ppc1x routine...
float PHCentralTrackv4::get_ppc1x(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc1x() : -999); } 

// Implement the set_ppc1x routine...
void PHCentralTrackv4::set_ppc1x(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc1x(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc1y routine...
float PHCentralTrackv4::get_ppc1y(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc1y() : -999); } 

// Implement the set_ppc1y routine...
void PHCentralTrackv4::set_ppc1y(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc1y(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc1z routine...
float PHCentralTrackv4::get_ppc1z(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc1z() : -999); } 

// Implement the set_ppc1z routine...
void PHCentralTrackv4::set_ppc1z(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc1z(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc2x routine...
float PHCentralTrackv4::get_ppc2x(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc2x() : -999); } 

// Implement the set_ppc2x routine...
void PHCentralTrackv4::set_ppc2x(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc2x(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc2y routine...
float PHCentralTrackv4::get_ppc2y(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc2y() : -999); } 

// Implement the set_ppc2y routine...
void PHCentralTrackv4::set_ppc2y(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc2y(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc2z routine...
float PHCentralTrackv4::get_ppc2z(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc2z() : -999); } 

// Implement the set_ppc2z routine...
void PHCentralTrackv4::set_ppc2z(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc2z(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ptecx routine...
float PHCentralTrackv4::get_ptecx(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptecx() : -999); } 

// Implement the set_ptecx routine...
void PHCentralTrackv4::set_ptecx(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptecx(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ptecy routine...
float PHCentralTrackv4::get_ptecy(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptecy() : -999); } 

// Implement the set_ptecy routine...
void PHCentralTrackv4::set_ptecy(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptecy(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ptecz routine...
float PHCentralTrackv4::get_ptecz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptecz() : -999); } 

// Implement the set_ptecz routine...
void PHCentralTrackv4::set_ptecz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptecz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc3x routine...
float PHCentralTrackv4::get_ppc3x(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc3x() : -999); } 

// Implement the set_ppc3x routine...
void PHCentralTrackv4::set_ppc3x(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc3x(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc3y routine...
float PHCentralTrackv4::get_ppc3y(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc3y() : -999); } 

// Implement the set_ppc3y routine...
void PHCentralTrackv4::set_ppc3y(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc3y(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ppc3z routine...
float PHCentralTrackv4::get_ppc3z(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc3z() : -999); } 

// Implement the set_ppc3z routine...
void PHCentralTrackv4::set_ppc3z(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc3z(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pemcx routine...
float PHCentralTrackv4::get_pemcx(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pemcx() : -999); } 

// Implement the set_pemcx routine...
void PHCentralTrackv4::set_pemcx(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pemcx(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pemcy routine...
float PHCentralTrackv4::get_pemcy(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pemcy() : -999); } 

// Implement the set_pemcy routine...
void PHCentralTrackv4::set_pemcy(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pemcy(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pemcz routine...
float PHCentralTrackv4::get_pemcz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pemcz() : -999); } 

// Implement the set_pemcz routine...
void PHCentralTrackv4::set_pemcz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pemcz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ptofx routine...
float PHCentralTrackv4::get_ptofx(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptofx() : -999); } 

// Implement the set_ptofx routine...
void PHCentralTrackv4::set_ptofx(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptofx(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ptofy routine...
float PHCentralTrackv4::get_ptofy(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptofy() : -999); } 

// Implement the set_ptofy routine...
void PHCentralTrackv4::set_ptofy(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptofy(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ptofz routine...
float PHCentralTrackv4::get_ptofz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptofz() : -999); } 

// Implement the set_ptofz routine...
void PHCentralTrackv4::set_ptofz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptofz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pltof routine...
float PHCentralTrackv4::get_pltof(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pltof() : -999); } 

// Implement the set_pltof routine...
void PHCentralTrackv4::set_pltof(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pltof(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_plemc routine...
float PHCentralTrackv4::get_plemc(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_plemc() : -999); } 

// Implement the set_plemc routine...
void PHCentralTrackv4::set_plemc(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_plemc(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sect routine...
short PHCentralTrackv4::get_sect(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sect() : -999); } 

// Implement the set_sect routine...
void PHCentralTrackv4::set_sect(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sect(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ysect routine...
short PHCentralTrackv4::get_ysect(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ysect() : -999); } 

// Implement the set_ysect routine...
void PHCentralTrackv4::set_ysect(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ysect(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_zsect routine...
short PHCentralTrackv4::get_zsect(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_zsect() : -999); } 

// Implement the set_zsect routine...
void PHCentralTrackv4::set_zsect(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_zsect(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ecorr routine...
float PHCentralTrackv4::get_ecorr(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ecorr() : -999); } 

// Implement the set_ecorr routine...
void PHCentralTrackv4::set_ecorr(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ecorr(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ecore routine...
float PHCentralTrackv4::get_ecore(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ecore() : -999); } 

// Implement the set_ecore routine...
void PHCentralTrackv4::set_ecore(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ecore(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_temc routine...
float PHCentralTrackv4::get_temc(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_temc() : -999); } 

// Implement the set_temc routine...
void PHCentralTrackv4::set_temc(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_temc(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_prob routine...
float PHCentralTrackv4::get_prob(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_prob() : -999); } 

// Implement the set_prob routine...
void PHCentralTrackv4::set_prob(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_prob(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ecent routine...
float PHCentralTrackv4::get_ecent(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ecent() : -999); } 

// Implement the set_ecent routine...
void PHCentralTrackv4::set_ecent(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ecent(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_twrhit routine...
short PHCentralTrackv4::get_twrhit(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_twrhit() : -999); } 

// Implement the set_twrhit routine...
void PHCentralTrackv4::set_twrhit(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_twrhit(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_e9 routine...
float PHCentralTrackv4::get_e9(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_e9() : -999); } 

// Implement the set_e9 routine...
void PHCentralTrackv4::set_e9(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_e9(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_re9 routine...
float PHCentralTrackv4::get_re9(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_re9() : -999); } 

// Implement the set_re9 routine...
void PHCentralTrackv4::set_re9(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_re9(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_emcchi2 routine...
float PHCentralTrackv4::get_emcchi2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcchi2() : -999); } 

// Implement the set_emcchi2 routine...
void PHCentralTrackv4::set_emcchi2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcchi2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_secorr routine...
float PHCentralTrackv4::get_secorr(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_secorr() : -999); } 

// Implement the set_secorr routine...
void PHCentralTrackv4::set_secorr(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_secorr(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_secore routine...
float PHCentralTrackv4::get_secore(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_secore() : -999); } 

// Implement the set_secore routine...
void PHCentralTrackv4::set_secore(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_secore(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stemc routine...
float PHCentralTrackv4::get_stemc(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stemc() : -999); } 

// Implement the set_stemc routine...
void PHCentralTrackv4::set_stemc(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stemc(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sprob routine...
float PHCentralTrackv4::get_sprob(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sprob() : -999); } 

// Implement the set_sprob routine...
void PHCentralTrackv4::set_sprob(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sprob(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stwrhit routine...
short PHCentralTrackv4::get_stwrhit(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stwrhit() : -999); } 

// Implement the set_stwrhit routine...
void PHCentralTrackv4::set_stwrhit(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stwrhit(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_semcchi2 routine...
float PHCentralTrackv4::get_semcchi2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcchi2() : -999); } 

// Implement the set_semcchi2 routine...
void PHCentralTrackv4::set_semcchi2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcchi2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_slat routine...
int PHCentralTrackv4::get_slat(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_slat() : -999); } 

// Implement the set_slat routine...
void PHCentralTrackv4::set_slat(const unsigned int itrk, const int val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_slat(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_ttof routine...
float PHCentralTrackv4::get_ttof(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ttof() : -999); } 

// Implement the set_ttof routine...
void PHCentralTrackv4::set_ttof(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ttof(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_etof routine...
float PHCentralTrackv4::get_etof(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_etof() : -999); } 

// Implement the set_etof routine...
void PHCentralTrackv4::set_etof(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_etof(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sttof routine...
float PHCentralTrackv4::get_sttof(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sttof() : -999); } 

// Implement the set_sttof routine...
void PHCentralTrackv4::set_sttof(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sttof(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_setof routine...
float PHCentralTrackv4::get_setof(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_setof() : -999); } 

// Implement the set_setof routine...
void PHCentralTrackv4::set_setof(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_setof(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_n0 routine...
short PHCentralTrackv4::get_n0(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_n0() : -999); } 

// Implement the set_n0 routine...
void PHCentralTrackv4::set_n0(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_n0(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_npe0 routine...
float PHCentralTrackv4::get_npe0(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_npe0() : -999); } 

// Implement the set_npe0 routine...
void PHCentralTrackv4::set_npe0(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_npe0(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_n1 routine...
short PHCentralTrackv4::get_n1(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_n1() : -999); } 

// Implement the set_n1 routine...
void PHCentralTrackv4::set_n1(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_n1(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_npe1 routine...
float PHCentralTrackv4::get_npe1(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_npe1() : -999); } 

// Implement the set_npe1 routine...
void PHCentralTrackv4::set_npe1(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_npe1(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_chi2 routine...
float PHCentralTrackv4::get_chi2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_chi2() : -999); } 

// Implement the set_chi2 routine...
void PHCentralTrackv4::set_chi2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_chi2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_disp routine...
float PHCentralTrackv4::get_disp(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_disp() : -999); } 

// Implement the set_disp routine...
void PHCentralTrackv4::set_disp(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_disp(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tcrk routine...
float PHCentralTrackv4::get_tcrk(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tcrk() : -999); } 

// Implement the set_tcrk routine...
void PHCentralTrackv4::set_tcrk(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tcrk(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_cross_phi routine...
float PHCentralTrackv4::get_cross_phi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_cross_phi() : -999); } 

// Implement the set_cross_phi routine...
void PHCentralTrackv4::set_cross_phi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_cross_phi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_cross_z routine...
float PHCentralTrackv4::get_cross_z(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_cross_z() : -999); } 

// Implement the set_cross_z routine...
void PHCentralTrackv4::set_cross_z(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_cross_z(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_center_phi routine...
float PHCentralTrackv4::get_center_phi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_center_phi() : -999); } 

// Implement the set_center_phi routine...
void PHCentralTrackv4::set_center_phi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_center_phi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_center_z routine...
float PHCentralTrackv4::get_center_z(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_center_z() : -999); } 

// Implement the set_center_z routine...
void PHCentralTrackv4::set_center_z(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_center_z(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sn0 routine...
short PHCentralTrackv4::get_sn0(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sn0() : -999); } 

// Implement the set_sn0 routine...
void PHCentralTrackv4::set_sn0(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sn0(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_snpe0 routine...
float PHCentralTrackv4::get_snpe0(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_snpe0() : -999); } 

// Implement the set_snpe0 routine...
void PHCentralTrackv4::set_snpe0(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_snpe0(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sn1 routine...
short PHCentralTrackv4::get_sn1(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sn1() : -999); } 

// Implement the set_sn1 routine...
void PHCentralTrackv4::set_sn1(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sn1(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_snpe1 routine...
float PHCentralTrackv4::get_snpe1(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_snpe1() : -999); } 

// Implement the set_snpe1 routine...
void PHCentralTrackv4::set_snpe1(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_snpe1(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_schi2 routine...
float PHCentralTrackv4::get_schi2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_schi2() : -999); } 

// Implement the set_schi2 routine...
void PHCentralTrackv4::set_schi2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_schi2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sdisp routine...
float PHCentralTrackv4::get_sdisp(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sdisp() : -999); } 

// Implement the set_sdisp routine...
void PHCentralTrackv4::set_sdisp(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sdisp(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stcrk routine...
float PHCentralTrackv4::get_stcrk(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stcrk() : -999); } 

// Implement the set_stcrk routine...
void PHCentralTrackv4::set_stcrk(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stcrk(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_tecdedx1 routine...
float PHCentralTrackv4::get_tecdedx1(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecdedx1() : -999); } 

// Implement the set_tecdedx1 routine...
void PHCentralTrackv4::set_tecdedx1(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecdedx1(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tecdedx2 routine...
float PHCentralTrackv4::get_tecdedx2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecdedx2() : -999); } 

// Implement the set_tecdedx2 routine...
void PHCentralTrackv4::set_tecdedx2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecdedx2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc2sdphi routine...
float PHCentralTrackv4::get_pc2sdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2sdphi() : -999); } 

// Implement the set_pc2sdphi routine...
void PHCentralTrackv4::set_pc2sdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc2sdz routine...
float PHCentralTrackv4::get_pc2sdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2sdz() : -999); } 

// Implement the set_pc2sdz routine...
void PHCentralTrackv4::set_pc2sdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2sdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc3sdphi routine...
float PHCentralTrackv4::get_pc3sdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3sdphi() : -999); } 

// Implement the set_pc3sdphi routine...
void PHCentralTrackv4::set_pc3sdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc3sdz routine...
float PHCentralTrackv4::get_pc3sdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3sdz() : -999); } 

// Implement the set_pc3sdz routine...
void PHCentralTrackv4::set_pc3sdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3sdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_emcsdphi routine...
float PHCentralTrackv4::get_emcsdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcsdphi() : -999); } 

// Implement the set_emcsdphi routine...
void PHCentralTrackv4::set_emcsdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_emcsdz routine...
float PHCentralTrackv4::get_emcsdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcsdz() : -999); } 

// Implement the set_emcsdz routine...
void PHCentralTrackv4::set_emcsdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcsdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tofsdphi routine...
float PHCentralTrackv4::get_tofsdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofsdphi() : -999); } 

// Implement the set_tofsdphi routine...
void PHCentralTrackv4::set_tofsdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tofsdz routine...
float PHCentralTrackv4::get_tofsdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofsdz() : -999); } 

// Implement the set_tofsdz routine...
void PHCentralTrackv4::set_tofsdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofsdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tecsdphi routine...
float PHCentralTrackv4::get_tecsdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecsdphi() : -999); } 

// Implement the set_tecsdphi routine...
void PHCentralTrackv4::set_tecsdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tecsdalpha routine...
float PHCentralTrackv4::get_tecsdalpha(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecsdalpha() : -999); } 

// Implement the set_tecsdalpha routine...
void PHCentralTrackv4::set_tecsdalpha(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecsdalpha(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_spc2sdphi routine...
float PHCentralTrackv4::get_spc2sdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2sdphi() : -999); } 

// Implement the set_spc2sdphi routine...
void PHCentralTrackv4::set_spc2sdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_spc2sdz routine...
float PHCentralTrackv4::get_spc2sdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2sdz() : -999); } 

// Implement the set_spc2sdz routine...
void PHCentralTrackv4::set_spc2sdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2sdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_spc3sdphi routine...
float PHCentralTrackv4::get_spc3sdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3sdphi() : -999); } 

// Implement the set_spc3sdphi routine...
void PHCentralTrackv4::set_spc3sdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_spc3sdz routine...
float PHCentralTrackv4::get_spc3sdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3sdz() : -999); } 

// Implement the set_spc3sdz routine...
void PHCentralTrackv4::set_spc3sdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3sdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_semcsdphi routine...
float PHCentralTrackv4::get_semcsdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcsdphi() : -999); } 

// Implement the set_semcsdphi routine...
void PHCentralTrackv4::set_semcsdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_semcsdz routine...
float PHCentralTrackv4::get_semcsdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcsdz() : -999); } 

// Implement the set_semcsdz routine...
void PHCentralTrackv4::set_semcsdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcsdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stofsdphi routine...
float PHCentralTrackv4::get_stofsdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofsdphi() : -999); } 

// Implement the set_stofsdphi routine...
void PHCentralTrackv4::set_stofsdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stofsdz routine...
float PHCentralTrackv4::get_stofsdz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofsdz() : -999); } 

// Implement the set_stofsdz routine...
void PHCentralTrackv4::set_stofsdz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofsdz(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stecsdphi routine...
float PHCentralTrackv4::get_stecsdphi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stecsdphi() : -999); } 

// Implement the set_stecsdphi routine...
void PHCentralTrackv4::set_stecsdphi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stecsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_stecsdalpha routine...
float PHCentralTrackv4::get_stecsdalpha(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stecsdalpha() : -999); } 

// Implement the set_stecsdalpha routine...
void PHCentralTrackv4::set_stecsdalpha(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stecsdalpha(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_m2tof routine...
float PHCentralTrackv4::get_m2tof(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_m2tof() : -999); } 

// Implement the set_m2tof routine...
void PHCentralTrackv4::set_m2tof(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_m2tof(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_m2emc routine...
float PHCentralTrackv4::get_m2emc(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_m2emc() : -999); } 

// Implement the set_m2emc routine...
void PHCentralTrackv4::set_m2emc(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_m2emc(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_isPi routine...
float PHCentralTrackv4::get_isPi(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_isPi() : -999); } 

// Implement the set_isPi routine...
void PHCentralTrackv4::set_isPi(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_isPi(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_isK routine...
float PHCentralTrackv4::get_isK(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_isK() : -999); } 

// Implement the set_isK routine...
void PHCentralTrackv4::set_isK(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_isK(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; }

// Implement the get_isP routine...
float PHCentralTrackv4::get_isP(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_isP() : -999); } 

// Implement the set_isP routine...
void PHCentralTrackv4::set_isP(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_isP(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; }

// Implement the get_charge routine...
short PHCentralTrackv4::get_charge(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_charge() : -999); } 

// Implement the set_charge routine...
void PHCentralTrackv4::set_charge(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_charge(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 

//...........................................................................
// Variables below this line were added specially to v4...TKH 5-19-2002
// Implement the get_dcarm routine...
short PHCentralTrackv4::get_dcarm(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_dcarm() : -999); } 

// Implement the set_dcarm routine...
void PHCentralTrackv4::set_dcarm(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_dcarm(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_dcside routine...
short PHCentralTrackv4::get_dcside(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_dcside() : -999); } 

// Implement the set_dcside routine...
void PHCentralTrackv4::set_dcside(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_dcside(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc1sect routine...
short PHCentralTrackv4::get_pc1sect(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc1sect() : -999); } 

// Implement the set_pc1sect routine...
void PHCentralTrackv4::set_pc1sect(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc1sect(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc2sect routine...
short PHCentralTrackv4::get_pc2sect(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2sect() : -999); } 

// Implement the set_pc2sect routine...
void PHCentralTrackv4::set_pc2sect(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2sect(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_pc3sect routine...
short PHCentralTrackv4::get_pc3sect(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3sect() : -999); } 

// Implement the set_pc3sect routine...
void PHCentralTrackv4::set_pc3sect(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3sect(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_emcsdphi_e routine...
float PHCentralTrackv4::get_emcsdphi_e(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcsdphi_e() : -999); } 

// Implement the set_emcsdphi_e routine...
void PHCentralTrackv4::set_emcsdphi_e(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcsdphi_e(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_emcsdz_e routine...
float PHCentralTrackv4::get_emcsdz_e(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcsdz_e() : -999); } 

// Implement the set_emcsdz_e routine...
void PHCentralTrackv4::set_emcsdz_e(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcsdz_e(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_semcsdphi_e routine...
float PHCentralTrackv4::get_semcsdphi_e(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcsdphi_e() : -999); } 

// Implement the set_semcsdphi_e routine...
void PHCentralTrackv4::set_semcsdphi_e(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcsdphi_e(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_semcsdz_e routine...
float PHCentralTrackv4::get_semcsdz_e(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcsdz_e() : -999); } 

// Implement the set_semcsdz_e routine...
void PHCentralTrackv4::set_semcsdz_e(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcsdz_e(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_tecnhit routine...
short PHCentralTrackv4::get_tecnhit(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecnhit() : -999); } 

// Implement the set_tecnhit routine...
void PHCentralTrackv4::set_tecnhit(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecnhit(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_mx1dist routine...
float PHCentralTrackv4::get_mx1dist(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mx1dist() : -999); } 

// Implement the set_mx1dist routine...
void PHCentralTrackv4::set_mx1dist(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mx1dist(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_mx2dist routine...
float PHCentralTrackv4::get_mx2dist(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mx2dist() : -999); } 

// Implement the set_mx2dist routine...
void PHCentralTrackv4::set_mx2dist(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mx2dist(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_mchi2 routine...
float PHCentralTrackv4::get_mchi2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mchi2() : -999); } 

// Implement the set_mchi2 routine...
void PHCentralTrackv4::set_mchi2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mchi2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_n2 routine...
short PHCentralTrackv4::get_n2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_n2() : -999); } 

// Implement the set_n2 routine...
void PHCentralTrackv4::set_n2(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_n2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_npe2 routine...
float PHCentralTrackv4::get_npe2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_npe2() : -999); } 

// Implement the set_npe2 routine...
void PHCentralTrackv4::set_npe2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_npe2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_n3 routine...
short PHCentralTrackv4::get_n3(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_n3() : -999); } 

// Implement the set_n3 routine...
void PHCentralTrackv4::set_n3(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_n3(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_npe3 routine...
float PHCentralTrackv4::get_npe3(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_npe3() : -999); } 

// Implement the set_npe3 routine...
void PHCentralTrackv4::set_npe3(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_npe3(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 


// Implement the get_sn2 routine...
short PHCentralTrackv4::get_sn2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sn2() : -999); } 

// Implement the set_sn2 routine...
void PHCentralTrackv4::set_sn2(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sn2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_snpe2 routine...
float PHCentralTrackv4::get_snpe2(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_snpe2() : -999); } 

// Implement the set_snpe2 routine...
void PHCentralTrackv4::set_snpe2(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_snpe2(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sn3 routine...
short PHCentralTrackv4::get_sn3(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sn3() : -999); } 

// Implement the set_sn3 routine...
void PHCentralTrackv4::set_sn3(const unsigned int itrk, const short val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sn3(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_snpe3 routine...
float PHCentralTrackv4::get_snpe3(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_snpe3() : -999); } 

// Implement the set_snpe3 routine...
void PHCentralTrackv4::set_snpe3(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_snpe3(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_deadmap routine...
int PHCentralTrackv4::get_deadmap(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_deadmap() : -999); } 

// Implement the set_deadmap routine...
void PHCentralTrackv4::set_deadmap(const unsigned int itrk, const int val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_deadmap(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_warnmap routine...
int PHCentralTrackv4::get_warnmap(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_warnmap() : -999); } 

// Implement the set_warnmap routine...
void PHCentralTrackv4::set_warnmap(const unsigned int itrk, const int val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_warnmap(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_sdeadmap routine...
int PHCentralTrackv4::get_sdeadmap(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sdeadmap() : -999); } 

// Implement the set_sdeadmap routine...
void PHCentralTrackv4::set_sdeadmap(const unsigned int itrk, const int val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sdeadmap(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 



// Implement the get_swarnmap routine...
int PHCentralTrackv4::get_swarnmap(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_swarnmap() : -999); } 

// Implement the set_swarnmap routine...
void PHCentralTrackv4::set_swarnmap(const unsigned int itrk, const int val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (Particle) Particle->set_swarnmap(val); 
  else      cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
  return; } 






//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  The following routines are not just simple accessors, but they implement complicated 
//  functionality hiding the true implementation from the user.
// Implement the get_px routine...
float PHCentralTrackv4::get_px(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (!Particle) return -999;

  float p      =     Particle->get_mom ();
  float sinTH  = sin(Particle->get_the0());
  float cosPHI = cos(Particle->get_phi0());
  return p*sinTH*cosPHI; 
} 

// Implement the set_stofsdz routine...
void PHCentralTrackv4::set_px(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (!Particle) {
    cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
    return;
  }

  float p   = Particle->get_mom ();
  float TH  = Particle->get_the0();
  float PHI = Particle->get_phi0();

  float py = p*sin(TH)*sin(PHI);
  float pz = p*cos(TH);

  float px = val;

  p  = sqrt(px*px + py*py + pz*pz);
  TH = acos(pz/p);
  PHI= atan2(py,px);

  Particle->set_mom(p);
  Particle->set_the0(TH);
  Particle->set_phi0(PHI);

  return; } 

// Set and Get py...
float PHCentralTrackv4::get_py(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (!Particle) return -999;

  float p      =     Particle->get_mom ();
  float sinTH  = sin(Particle->get_the0());
  float sinPHI = sin(Particle->get_phi0());
  return p*sinTH*sinPHI; 
} 

void PHCentralTrackv4::set_py(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (!Particle) {
    cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
    return;
  }

  float p   = Particle->get_mom ();
  float TH  = Particle->get_the0();
  float PHI = Particle->get_phi0();

  float px = p*sin(TH)*cos(PHI);
  float pz = p*cos(TH);

  float py = val;  // Overwrite the py with the input...

  p  = sqrt(px*px + py*py + pz*pz);
  TH = acos(pz/p);
  PHI= atan2(py,px);

  Particle->set_mom(p);
  Particle->set_the0(TH);
  Particle->set_phi0(PHI);

  return; } 

// Set and Get pz...
float PHCentralTrackv4::get_pz(const unsigned int itrk) const { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (!Particle) return -999;

  float p      =     Particle->get_mom ();
  float cosTH  = cos(Particle->get_the0());
  return p*cosTH; 
} 

void PHCentralTrackv4::set_pz(const unsigned int itrk, const float val) { 
  PHSnglCentralTrackv4 *Particle = (PHSnglCentralTrackv4 *) GetCentral()->UncheckedAt(itrk); 
  if (!Particle) {
    cout << PHWHERE << "ERROR no PHCentralTrackv4 object found" << endl; 
    return;
  }

  float p   = Particle->get_mom ();
  float TH  = Particle->get_the0();
  float PHI = Particle->get_phi0();

  float px = p*sin(TH)*cos(PHI);
  float py = p*sin(TH)*sin(PHI);

  float pz = val;

  p  = sqrt(px*px + py*py + pz*pz);
  TH = acos(pz/p);
  PHI= atan2(py,px);

  Particle->set_mom(p);
  Particle->set_the0(TH);
  Particle->set_phi0(PHI);

  return; } 




