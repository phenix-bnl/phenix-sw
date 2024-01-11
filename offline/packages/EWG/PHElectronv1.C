#include "PHElectronv1.h"
#include "PHSnglElectronv1.h"

ClassImp(PHElectronv1);

using namespace std;

#define PHNTRACKS 400

  // First we implement the "standard functions"...
PHElectronv1::PHElectronv1()
{
  npart = 0;
  Cent = new TClonesArray("PHSnglElectronv1",PHNTRACKS);
  return;
}

PHElectronv1::~PHElectronv1()
{
  Cent->Clear();
  return;
}

void PHElectronv1::identify(ostream& os) const
{
  os << "identify yourself: PHElectronv1 Object" << endl;
  os << "No of Tracks: " << npart << endl;
  return;
}

void PHElectronv1::Reset()
{
 Cent->Clear();
 if (npart>PHNTRACKS)
   {
     Cent->Expand(PHNTRACKS);
   }
 npart = 0;
 return;
}

int PHElectronv1::isValid() const
{
  return((npart>0) ? 1 : 0);
}

int PHElectronv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > PHNTRACKS)
    {
      Cent->Expand(nhits);
     }
  return nhits;
}

void  PHElectronv1::AddPHParticle(const unsigned int itrk)
{
  TClonesArray &Particle = *Cent;
  new(Particle[itrk]) PHSnglElectronv1();
  return;
}

void  PHElectronv1::RemovePHParticle(const unsigned int itrk)
{
  Cent->RemoveAt(itrk);
  return;
}



// Implement the get_quality routine...
short PHElectronv1::get_quality(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_quality() : -999); } 

// Implement the set_quality routine...
void PHElectronv1::set_quality(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_quality(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_zed routine...
float PHElectronv1::get_zed(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_zed() : -999); } 

// Implement the set_zed routine...
void PHElectronv1::set_zed(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_zed(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_phi routine...
float PHElectronv1::get_phi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phi() : -999); } 

// Implement the set_phi routine...
void PHElectronv1::set_phi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_alpha routine...
float PHElectronv1::get_alpha(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alpha() : -999); } 

// Implement the set_alpha routine...
void PHElectronv1::set_alpha(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alpha(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_beta routine...
float PHElectronv1::get_beta(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_beta() : -999); } 

// Implement the set_beta routine...
void PHElectronv1::set_beta(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_beta(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_phi0 routine...
float PHElectronv1::get_phi0(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phi0() : -999); } 

// Implement the set_phi0 routine...
void PHElectronv1::set_phi0(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phi0(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_the0 routine...
float PHElectronv1::get_the0(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_the0() : -999); } 

// Implement the set_the0 routine...
void PHElectronv1::set_the0(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_the0(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mom routine...
float PHElectronv1::get_mom(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mom() : -999); } 

// Implement the set_mom routine...
void PHElectronv1::set_mom(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mom(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mom routine...
float PHElectronv1::get_mompx(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mompx() : -999); } 

// Implement the set_mom routine...
void PHElectronv1::set_mompx(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mompx(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mom routine...
float PHElectronv1::get_mompy(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mompy() : -999); } 

// Implement the set_mom routine...
void PHElectronv1::set_mompy(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mompy(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mom routine...
float PHElectronv1::get_mompz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mompz() : -999); } 

// Implement the set_mom routine...
void PHElectronv1::set_mompz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mompz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_status routine...
short PHElectronv1::get_status(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_status() : -999); } 

// Implement the set_status routine...
void PHElectronv1::set_status(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_status(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_alpha1 routine...
float PHElectronv1::get_alpha1(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alpha1() : -999); } 

// Implement the set_alpha1 routine...
void PHElectronv1::set_alpha1(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alpha1(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_alpha2 routine...
float PHElectronv1::get_alpha2(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alpha2() : -999); } 

// Implement the set_alpha2 routine...
void PHElectronv1::set_alpha2(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alpha2(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_nx1hits routine...
short PHElectronv1::get_nx1hits(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_nx1hits() : -999); } 

// Implement the set_nx1hits routine...
void PHElectronv1::set_nx1hits(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_nx1hits(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_nx2hits routine...
short PHElectronv1::get_nx2hits(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_nx2hits() : -999); } 

// Implement the set_nx2hits routine...
void PHElectronv1::set_nx2hits(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_nx2hits(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mx1dist routine...
float PHElectronv1::get_mx1dist(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mx1dist() : -999); } 

// Implement the set_mx1dist routine...
void PHElectronv1::set_mx1dist(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mx1dist(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mx2dist routine...
float PHElectronv1::get_mx2dist(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mx2dist() : -999); } 

// Implement the set_mx2dist routine...
void PHElectronv1::set_mx2dist(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mx2dist(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_chi2x1 routine...
float PHElectronv1::get_chi2x1(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_chi2x1() : -999); } 

// Implement the set_chi2x1 routine...
void PHElectronv1::set_chi2x1(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_chi2x1(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_chi2x2 routine...
float PHElectronv1::get_chi2x2(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_chi2x2() : -999); } 

// Implement the set_chi2x2 routine...
void PHElectronv1::set_chi2x2(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_chi2x2(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_nx1x2fit routine...
short PHElectronv1::get_nx1x2fit(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_nx1x2fit() : -999); } 

// Implement the set_nx1x2fit routine...
void PHElectronv1::set_nx1x2fit(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_nx1x2fit(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_mchi2 routine...
float PHElectronv1::get_mchi2(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_mchi2() : -999); } 

// Implement the set_mchi2 routine...
void PHElectronv1::set_mchi2(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_mchi2(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_error routine...
float PHElectronv1::get_error(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_error() : -999); } 

// Implement the set_error routine...
void PHElectronv1::set_error(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_error(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_alphaf routine...
float PHElectronv1::get_alphaf(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_alphaf() : -999); } 

// Implement the set_alphaf routine...
void PHElectronv1::set_alphaf(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_alphaf(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc1id routine...
short PHElectronv1::get_pc1id(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc1id() : -999); } 

// Implement the set_pc1id routine...
void PHElectronv1::set_pc1id(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc1id(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc2id routine...
short PHElectronv1::get_pc2id(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2id() : -999); } 

// Implement the set_pc2id routine...
void PHElectronv1::set_pc2id(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2id(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc3id routine...
short PHElectronv1::get_pc3id(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3id() : -999); } 

// Implement the set_pc3id routine...
void PHElectronv1::set_pc3id(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3id(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_emcid routine...
short PHElectronv1::get_emcid(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcid() : -999); } 

// Implement the set_emcid routine...
void PHElectronv1::set_emcid(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcid(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tofid routine...
short PHElectronv1::get_tofid(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofid() : -999); } 

// Implement the set_tofid routine...
void PHElectronv1::set_tofid(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofid(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tecid routine...
short PHElectronv1::get_tecid(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecid() : -999); } 

// Implement the set_tecid routine...
void PHElectronv1::set_tecid(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecid(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc2id routine...
short PHElectronv1::get_spc2id(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2id() : -999); } 

// Implement the set_spc2id routine...
void PHElectronv1::set_spc2id(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2id(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc3id routine...
short PHElectronv1::get_spc3id(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3id() : -999); } 

// Implement the set_spc3id routine...
void PHElectronv1::set_spc3id(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3id(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_semcid routine...
short PHElectronv1::get_semcid(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcid() : -999); } 

// Implement the set_semcid routine...
void PHElectronv1::set_semcid(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcid(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stofid routine...
short PHElectronv1::get_stofid(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofid() : -999); } 

// Implement the set_stofid routine...
void PHElectronv1::set_stofid(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofid(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stecid routine...
short PHElectronv1::get_stecid(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stecid() : -999); } 

// Implement the set_stecid routine...
void PHElectronv1::set_stecid(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stecid(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc1x routine...
float PHElectronv1::get_ppc1x(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc1x() : -999); } 

// Implement the set_ppc1x routine...
void PHElectronv1::set_ppc1x(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc1x(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc1y routine...
float PHElectronv1::get_ppc1y(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc1y() : -999); } 

// Implement the set_ppc1y routine...
void PHElectronv1::set_ppc1y(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc1y(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc1z routine...
float PHElectronv1::get_ppc1z(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc1z() : -999); } 

// Implement the set_ppc1z routine...
void PHElectronv1::set_ppc1z(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc1z(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc2x routine...
float PHElectronv1::get_ppc2x(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc2x() : -999); } 

// Implement the set_ppc2x routine...
void PHElectronv1::set_ppc2x(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc2x(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc2y routine...
float PHElectronv1::get_ppc2y(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc2y() : -999); } 

// Implement the set_ppc2y routine...
void PHElectronv1::set_ppc2y(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc2y(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc2z routine...
float PHElectronv1::get_ppc2z(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc2z() : -999); } 

// Implement the set_ppc2z routine...
void PHElectronv1::set_ppc2z(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc2z(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ptecx routine...
float PHElectronv1::get_ptecx(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptecx() : -999); } 

// Implement the set_ptecx routine...
void PHElectronv1::set_ptecx(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptecx(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ptecy routine...
float PHElectronv1::get_ptecy(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptecy() : -999); } 

// Implement the set_ptecy routine...
void PHElectronv1::set_ptecy(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptecy(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ptecz routine...
float PHElectronv1::get_ptecz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptecz() : -999); } 

// Implement the set_ptecz routine...
void PHElectronv1::set_ptecz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptecz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc3x routine...
float PHElectronv1::get_ppc3x(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc3x() : -999); } 

// Implement the set_ppc3x routine...
void PHElectronv1::set_ppc3x(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc3x(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc3y routine...
float PHElectronv1::get_ppc3y(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc3y() : -999); } 

// Implement the set_ppc3y routine...
void PHElectronv1::set_ppc3y(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc3y(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ppc3z routine...
float PHElectronv1::get_ppc3z(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ppc3z() : -999); } 

// Implement the set_ppc3z routine...
void PHElectronv1::set_ppc3z(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ppc3z(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pemcx routine...
float PHElectronv1::get_pemcx(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pemcx() : -999); } 

// Implement the set_pemcx routine...
void PHElectronv1::set_pemcx(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pemcx(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pemcy routine...
float PHElectronv1::get_pemcy(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pemcy() : -999); } 

// Implement the set_pemcy routine...
void PHElectronv1::set_pemcy(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pemcy(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pemcz routine...
float PHElectronv1::get_pemcz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pemcz() : -999); } 

// Implement the set_pemcz routine...
void PHElectronv1::set_pemcz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pemcz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ptofx routine...
float PHElectronv1::get_ptofx(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptofx() : -999); } 

// Implement the set_ptofx routine...
void PHElectronv1::set_ptofx(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptofx(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ptofy routine...
float PHElectronv1::get_ptofy(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptofy() : -999); } 

// Implement the set_ptofy routine...
void PHElectronv1::set_ptofy(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptofy(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ptofz routine...
float PHElectronv1::get_ptofz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ptofz() : -999); } 

// Implement the set_ptofz routine...
void PHElectronv1::set_ptofz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ptofz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 


// Implement the get_phbdx routine...
float PHElectronv1::get_phbdx(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phbdx() : -999); } 

// Implement the set_phbdx routine...
void PHElectronv1::set_phbdx(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phbdx(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_phbdy routine...
float PHElectronv1::get_phbdy(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phbdy() : -999); } 

// Implement the set_phbdy routine...
void PHElectronv1::set_phbdy(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phbdy(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_phbdz routine...
float PHElectronv1::get_phbdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_phbdz() : -999); } 

// Implement the set_phbdz routine...
void PHElectronv1::set_phbdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_phbdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pltof routine...
float PHElectronv1::get_pltof(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pltof() : -999); } 

// Implement the set_pltof routine...
void PHElectronv1::set_pltof(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pltof(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_plemc routine...
float PHElectronv1::get_plemc(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_plemc() : -999); } 

// Implement the set_plemc routine...
void PHElectronv1::set_plemc(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_plemc(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc2dphi routine...
float PHElectronv1::get_pc2dphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2dphi() : -999); } 

// Implement the set_pc2dphi routine...
void PHElectronv1::set_pc2dphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2dphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc2dz routine...
float PHElectronv1::get_pc2dz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2dz() : -999); } 

// Implement the set_pc2dz routine...
void PHElectronv1::set_pc2dz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2dz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc3dphi routine...
float PHElectronv1::get_pc3dphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3dphi() : -999); } 

// Implement the set_pc3dphi routine...
void PHElectronv1::set_pc3dphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3dphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc3dz routine...
float PHElectronv1::get_pc3dz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3dz() : -999); } 

// Implement the set_pc3dz routine...
void PHElectronv1::set_pc3dz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3dz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_emcdphi routine...
float PHElectronv1::get_emcdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcdphi() : -999); } 

// Implement the set_emcdphi routine...
void PHElectronv1::set_emcdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_emcdz routine...
float PHElectronv1::get_emcdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcdz() : -999); } 

// Implement the set_emcdz routine...
void PHElectronv1::set_emcdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tofdphi routine...
float PHElectronv1::get_tofdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofdphi() : -999); } 

// Implement the set_tofdphi routine...
void PHElectronv1::set_tofdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tofdz routine...
float PHElectronv1::get_tofdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofdz() : -999); } 

// Implement the set_tofdz routine...
void PHElectronv1::set_tofdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tecdphi routine...
float PHElectronv1::get_tecdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecdphi() : -999); } 

// Implement the set_tecdphi routine...
void PHElectronv1::set_tecdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tecdalpha routine...
float PHElectronv1::get_tecdalpha(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecdalpha() : -999); } 

// Implement the set_tecdalpha routine...
void PHElectronv1::set_tecdalpha(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecdalpha(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc2dphi routine...
float PHElectronv1::get_spc2dphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2dphi() : -999); } 

// Implement the set_spc2dphi routine...
void PHElectronv1::set_spc2dphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2dphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc2dz routine...
float PHElectronv1::get_spc2dz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2dz() : -999); } 

// Implement the set_spc2dz routine...
void PHElectronv1::set_spc2dz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2dz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc3dphi routine...
float PHElectronv1::get_spc3dphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3dphi() : -999); } 

// Implement the set_spc3dphi routine...
void PHElectronv1::set_spc3dphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3dphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc3dz routine...
float PHElectronv1::get_spc3dz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3dz() : -999); } 

// Implement the set_spc3dz routine...
void PHElectronv1::set_spc3dz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3dz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_semcdphi routine...
float PHElectronv1::get_semcdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcdphi() : -999); } 

// Implement the set_semcdphi routine...
void PHElectronv1::set_semcdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_semcdz routine...
float PHElectronv1::get_semcdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcdz() : -999); } 

// Implement the set_semcdz routine...
void PHElectronv1::set_semcdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stofdphi routine...
float PHElectronv1::get_stofdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofdphi() : -999); } 

// Implement the set_stofdphi routine...
void PHElectronv1::set_stofdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stofdz routine...
float PHElectronv1::get_stofdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofdz() : -999); } 

// Implement the set_stofdz routine...
void PHElectronv1::set_stofdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stecdphi routine...
float PHElectronv1::get_stecdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stecdphi() : -999); } 

// Implement the set_stecdphi routine...
void PHElectronv1::set_stecdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stecdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stecdalpha routine...
float PHElectronv1::get_stecdalpha(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stecdalpha() : -999); } 

// Implement the set_stecdalpha routine...
void PHElectronv1::set_stecdalpha(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stecdalpha(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_arm routine...
short PHElectronv1::get_arm(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_arm() : -999); } 

// Implement the set_arm routine...
void PHElectronv1::set_arm(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_arm(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sect routine...
short PHElectronv1::get_sect(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sect() : -999); } 

// Implement the set_sect routine...
void PHElectronv1::set_sect(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sect(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ysect routine...
short PHElectronv1::get_ysect(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ysect() : -999); } 

// Implement the set_ysect routine...
void PHElectronv1::set_ysect(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ysect(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_zsect routine...
short PHElectronv1::get_zsect(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_zsect() : -999); } 

// Implement the set_zsect routine...
void PHElectronv1::set_zsect(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_zsect(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ecorr routine...
float PHElectronv1::get_ecorr(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ecorr() : -999); } 

// Implement the set_ecorr routine...
void PHElectronv1::set_ecorr(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ecorr(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ecore routine...
float PHElectronv1::get_ecore(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ecore() : -999); } 

// Implement the set_ecore routine...
void PHElectronv1::set_ecore(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ecore(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_temc routine...
float PHElectronv1::get_temc(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_temc() : -999); } 

// Implement the set_temc routine...
void PHElectronv1::set_temc(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_temc(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_prob routine...
float PHElectronv1::get_prob(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_prob() : -999); } 

// Implement the set_prob routine...
void PHElectronv1::set_prob(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_prob(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_secorr routine...
float PHElectronv1::get_secorr(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_secorr() : -999); } 

// Implement the set_secorr routine...
void PHElectronv1::set_secorr(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_secorr(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_secore routine...
float PHElectronv1::get_secore(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_secore() : -999); } 

// Implement the set_secore routine...
void PHElectronv1::set_secore(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_secore(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stemc routine...
float PHElectronv1::get_stemc(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stemc() : -999); } 

// Implement the set_stemc routine...
void PHElectronv1::set_stemc(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stemc(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sprob routine...
float PHElectronv1::get_sprob(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sprob() : -999); } 

// Implement the set_sprob routine...
void PHElectronv1::set_sprob(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sprob(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_slat routine...
int PHElectronv1::get_slat(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_slat() : -999); } 

// Implement the set_slat routine...
void PHElectronv1::set_slat(const unsigned int itrk, const int val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_slat(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ttof routine...
float PHElectronv1::get_ttof(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ttof() : -999); } 

// Implement the set_ttof routine...
void PHElectronv1::set_ttof(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ttof(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_etof routine...
float PHElectronv1::get_etof(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_etof() : -999); } 

// Implement the set_etof routine...
void PHElectronv1::set_etof(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_etof(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sttof routine...
float PHElectronv1::get_sttof(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sttof() : -999); } 

// Implement the set_sttof routine...
void PHElectronv1::set_sttof(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sttof(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_setof routine...
float PHElectronv1::get_setof(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_setof() : -999); } 

// Implement the set_setof routine...
void PHElectronv1::set_setof(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_setof(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_acc routine...
short PHElectronv1::get_acc(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_acc() : -999); } 

// Implement the set_acc routine...
void PHElectronv1::set_acc(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_acc(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_ring routine...
int PHElectronv1::get_ring(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_ring() : -999); } 

// Implement the set_ring routine...
void PHElectronv1::set_ring(const unsigned int itrk, const int val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_ring(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_n0 routine...
short PHElectronv1::get_n0(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_n0() : -999); } 

// Implement the set_n0 routine...
void PHElectronv1::set_n0(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_n0(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_npe0 routine...
float PHElectronv1::get_npe0(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_npe0() : -999); } 

// Implement the set_npe0 routine...
void PHElectronv1::set_npe0(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_npe0(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_chi2 routine...
float PHElectronv1::get_chi2(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_chi2() : -999); } 

// Implement the set_chi2 routine...
void PHElectronv1::set_chi2(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_chi2(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_disp routine...
float PHElectronv1::get_disp(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_disp() : -999); } 

// Implement the set_disp routine...
void PHElectronv1::set_disp(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_disp(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tcrk routine...
float PHElectronv1::get_tcrk(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tcrk() : -999); } 

// Implement the set_tcrk routine...
void PHElectronv1::set_tcrk(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tcrk(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sacc routine...
short PHElectronv1::get_sacc(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sacc() : -999); } 

// Implement the set_sacc routine...
void PHElectronv1::set_sacc(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sacc(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sring routine...
int PHElectronv1::get_sring(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sring() : -999); } 

// Implement the set_sring routine...
void PHElectronv1::set_sring(const unsigned int itrk, const int val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sring(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sn0 routine...
short PHElectronv1::get_sn0(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sn0() : -999); } 

// Implement the set_sn0 routine...
void PHElectronv1::set_sn0(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sn0(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_snpe0 routine...
float PHElectronv1::get_snpe0(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_snpe0() : -999); } 

// Implement the set_snpe0 routine...
void PHElectronv1::set_snpe0(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_snpe0(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_schi2 routine...
float PHElectronv1::get_schi2(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_schi2() : -999); } 

// Implement the set_schi2 routine...
void PHElectronv1::set_schi2(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_schi2(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_sdisp routine...
float PHElectronv1::get_sdisp(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_sdisp() : -999); } 

// Implement the set_sdisp routine...
void PHElectronv1::set_sdisp(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_sdisp(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stcrk routine...
float PHElectronv1::get_stcrk(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stcrk() : -999); } 

// Implement the set_stcrk routine...
void PHElectronv1::set_stcrk(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stcrk(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tecdedx1 routine...
float PHElectronv1::get_tecdedx1(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecdedx1() : -999); } 

// Implement the set_tecdedx1 routine...
void PHElectronv1::set_tecdedx1(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecdedx1(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tecdedx2 routine...
float PHElectronv1::get_tecdedx2(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tecdedx2() : -999); } 

// Implement the set_tecdedx2 routine...
void PHElectronv1::set_tecdedx2(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tecdedx2(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc2sdphi routine...
float PHElectronv1::get_pc2sdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2sdphi() : -999); } 

// Implement the set_pc2sdphi routine...
void PHElectronv1::set_pc2sdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc2sdz routine...
float PHElectronv1::get_pc2sdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc2sdz() : -999); } 

// Implement the set_pc2sdz routine...
void PHElectronv1::set_pc2sdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc2sdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc3sdphi routine...
float PHElectronv1::get_pc3sdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3sdphi() : -999); } 

// Implement the set_pc3sdphi routine...
void PHElectronv1::set_pc3sdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_pc3sdz routine...
float PHElectronv1::get_pc3sdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_pc3sdz() : -999); } 

// Implement the set_pc3sdz routine...
void PHElectronv1::set_pc3sdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_pc3sdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_emcsdphi routine...
float PHElectronv1::get_emcsdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcsdphi() : -999); } 

// Implement the set_emcsdphi routine...
void PHElectronv1::set_emcsdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_emcsdz routine...
float PHElectronv1::get_emcsdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_emcsdz() : -999); } 

// Implement the set_emcsdz routine...
void PHElectronv1::set_emcsdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_emcsdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tofsdphi routine...
float PHElectronv1::get_tofsdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofsdphi() : -999); } 

// Implement the set_tofsdphi routine...
void PHElectronv1::set_tofsdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_tofsdz routine...
float PHElectronv1::get_tofsdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_tofsdz() : -999); } 

// Implement the set_tofsdz routine...
void PHElectronv1::set_tofsdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_tofsdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc2sdphi routine...
float PHElectronv1::get_spc2sdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2sdphi() : -999); } 

// Implement the set_spc2sdphi routine...
void PHElectronv1::set_spc2sdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc2sdz routine...
float PHElectronv1::get_spc2sdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc2sdz() : -999); } 

// Implement the set_spc2sdz routine...
void PHElectronv1::set_spc2sdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc2sdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc3sdphi routine...
float PHElectronv1::get_spc3sdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3sdphi() : -999); } 

// Implement the set_spc3sdphi routine...
void PHElectronv1::set_spc3sdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3sdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_spc3sdz routine...
float PHElectronv1::get_spc3sdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_spc3sdz() : -999); } 

// Implement the set_spc3sdz routine...
void PHElectronv1::set_spc3sdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_spc3sdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_semcsdphi routine...
float PHElectronv1::get_semcsdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcsdphi() : -999); } 

// Implement the set_semcsdphi routine...
void PHElectronv1::set_semcsdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_semcsdz routine...
float PHElectronv1::get_semcsdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_semcsdz() : -999); } 

// Implement the set_semcsdz routine...
void PHElectronv1::set_semcsdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_semcsdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stofsdphi routine...
float PHElectronv1::get_stofsdphi(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofsdphi() : -999); } 

// Implement the set_stofsdphi routine...
void PHElectronv1::set_stofsdphi(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofsdphi(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_stofsdz routine...
float PHElectronv1::get_stofsdz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_stofsdz() : -999); } 

// Implement the set_stofsdz routine...
void PHElectronv1::set_stofsdz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_stofsdz(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 



// Implement the get_charge routine...
short PHElectronv1::get_charge(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  return((Particle) ? Particle->get_charge() : -999); } 

// Implement the set_stofsdz routine...
void PHElectronv1::set_charge(const unsigned int itrk, const short val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (Particle) Particle->set_charge(val); 
  else      cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
  return; } 

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  The following routines are not just simple accessors, but they implement complicated 
//  functionality hiding the true implementation from the user.
// Implement the get_px routine...
float PHElectronv1::get_px(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (!Particle) return -999;

  float p      =     Particle->get_mom ();
  float sinTH  = sin(Particle->get_the0());
  float cosPHI = cos(Particle->get_phi0());
  return p*sinTH*cosPHI; 
} 

// Implement the set_stofsdz routine...
void PHElectronv1::set_px(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (!Particle) {
    cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
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
float PHElectronv1::get_py(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (!Particle) return -999;

  float p      =     Particle->get_mom ();
  float sinTH  = sin(Particle->get_the0());
  float sinPHI = sin(Particle->get_phi0());
  return p*sinTH*sinPHI; 
} 

void PHElectronv1::set_py(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (!Particle) {
    cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
    return;
  }

  float p   = Particle->get_mom ();
  float TH  = Particle->get_the0();
  float PHI = Particle->get_phi0();

  float px = p*sin(TH)*cos(PHI);
  float pz = p*cos(TH);

  float py = val;

  p  = sqrt(px*px + py*py + pz*pz);
  TH = acos(pz/p);
  PHI= atan2(py,px);

  Particle->set_mom(p);
  Particle->set_the0(TH);
  Particle->set_phi0(PHI);

  return; } 

// Set and Get pz...
float PHElectronv1::get_pz(const unsigned int itrk) const { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (!Particle) return -999;

  float p      =     Particle->get_mom ();
  float cosTH  = cos(Particle->get_the0());
  return p*cosTH; 
} 

void PHElectronv1::set_pz(const unsigned int itrk, const float val) { 
  PHSnglElectronv1 *Particle = (PHSnglElectronv1 *) GetCent()->UncheckedAt(itrk); 
  if (!Particle) {
    cout << PHWHERE << "ERROR no PHElectronv1 object found" << endl; 
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


