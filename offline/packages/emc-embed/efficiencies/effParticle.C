#include "effParticle.h"
#include <cstdio>

ClassImp(effParticle)

//_____________________________________________________________________________
effParticle::effParticle() : TParticle()
{
}

//_____________________________________________________________________________
effParticle::effParticle(Int_t pdg, Int_t status,
			 Int_t mother1, Int_t mother2,
			 Int_t daughter1, Int_t daughter2,
			 Double_t px, Double_t py, Double_t pz, Double_t etot,
			 Double_t vx, Double_t vy, Double_t vz, Double_t time)
  : TParticle(pdg,status,mother1,mother2,daughter1,daughter2,
	      px,py,pz,etot,vx,vy,vz,time)
{
}
 
//_____________________________________________________________________________
effParticle::effParticle(Int_t pdg, Int_t status,
			 Int_t mother1, Int_t mother2,
			 Int_t daughter1, Int_t daughter2,
			 const TLorentzVector &p,
			 const TLorentzVector &v) :
  TParticle(pdg,status,mother1,mother2,daughter1,daughter2,p,v)
{
}

//_____________________________________________________________________________
effParticle::effParticle(Int_t pdg,  
			 Double_t px, Double_t py, Double_t pz, Double_t etot,
			 double vx, double vy, double vz,
			 double impx, double impy, double impz,
			 double chi2, double tof, bool in_fiducial,
			 int dead, int warn, 
			 int isector, int arm,
			 double impactangle)
  : TParticle(pdg,0,0,0,0,0,px,py,pz,etot,vx,vy,vz,0.0),
    fDead(dead),fWarn(warn),fInFiducial(in_fiducial),fTOF(tof), fChi2(chi2),
    fSector(isector),fImpX(impx),fImpY(impy),fImpZ(impz),fArm(arm),fImpAngle(impactangle)
{
}

//_____________________________________________________________________________
effParticle::effParticle(const effParticle &part) : TParticle(part)
{
  fDead = part.Dead();
  fWarn = part.Warn();
  fInFiducial = part.InFiducial();
  fTOF = part.TOF();
  fChi2 = part.Chi2();
  fSector = part.Sector();
  fArm = part.Arm();
  fImpX = part.ImpX();
  fImpY = part.ImpY();
  fImpZ = part.ImpZ();
  fImpAngle = part.ImpAngle();
}

//_____________________________________________________________________________
effParticle&
effParticle::operator=(const effParticle& part)
{
  if ( this != &part ) 
    {
      ((TParticle&)*this)=((TParticle&)part);
       fDead = part.Dead();
       fWarn = part.Warn();
       fInFiducial = part.InFiducial();
       fTOF = part.TOF();
       fChi2 = part.Chi2();
       fSector = part.Sector();
       fArm = part.Arm();
       fImpX = part.ImpX();
       fImpY = part.ImpY();
       fImpZ = part.ImpZ();
       fImpAngle = part.ImpAngle();
    }
  return *this;
}

//_____________________________________________________________________________
effParticle::~effParticle()
{
}


//_____________________________________________________________________________
void 
effParticle::Print(Option_t *option) const
{
  TString opt = option;
  if (opt == "red")
    {
      printf(" PDG=%d Energy=%e \n",GetPdgCode(),Energy()); 
    }
  else
    {
      printf(" PDG=%d Energy=%e TOF=%e Chi2=%e Dead=%x Warn=%x InFidu=%d\n",
	     GetPdgCode(),Energy(),TOF(),Chi2(),Dead(),Warn(),InFiducial());
    }
}

//_____________________________________________________________________________
int
effParticle::geantToPDG(int geantPID)
{
  int pdgPID = -1;

  if ( geantPID == GEANTCODE_GAMMA ) 
    { 
      pdgPID = PDGCODE_GAMMA; 
    }
  else if ( geantPID == GEANTCODE_ELECTRON ) 
    { 
      pdgPID = PDGCODE_ELECTRON; 
    }
  else if ( geantPID == GEANTCODE_POSITRON ) 
    { 
      pdgPID = PDGCODE_POSITRON; 
    }
  else 
    {
      printf("geantPID=%d ?\n",geantPID);
    }
  return pdgPID;
}
