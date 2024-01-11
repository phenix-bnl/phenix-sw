#include "effPair.h"
#include <cassert>
#include <cstdio>

ClassImp(effPair)

//_____________________________________________________________________________
effPair::effPair(const effParticle& p1, const effParticle& p2)
  : fParticle1(p1), fParticle2(p2)
{
  TLorentzVector P1(0,0,0,0), P2(0,0,0,0);
  
  fParticle1.Momentum(P1);  
  fParticle2.Momentum(P2);

  double asymmetry = TMath::Abs( (P1.E()-P2.E())/(P1.E()+P2.E()) );
	  
  fPTOT = P1;
  fPTOT += P2;

  fPt = fPTOT.Pt();
  fMinv = fPTOT.M();
  fAsymmetry = asymmetry;

  TVector3 V1 = P1.Vect();
  TVector3 V2 = P2.Vect();
  fCosine = TMath::Cos( V1.Angle(V2) );

  // average "impact angle" in EMCal of the 2 partics. of the pair
  // fixme: what if the particles do not have the same vtx ??

  fAverageImpactAngle = (fParticle1.ImpAngle()+fParticle2.ImpAngle())/2.;

  //_____________________________________________________________________________
  
  // x vertex position of the pair [radius from interaction point (0.,0.,0.)]
  // If each particle of the pair does not have a common vtx. (e.g. combination of 1 gamma + 1 e+/-)
  // we set it to the bigger (abs. value) of the 2 vertices

  fxVtx = -999.;

//   TLorentzVector prodVtx1,prodVtx2;

//   fParticle1.ProductionVertex(prodVtx1);
//   fParticle2.ProductionVertex(prodVtx2);  

//   // FIXME: Be careful here: the ("abused") fields "TOF" of our effParticle's (1 and 2) 
//   //        correspond actually to the "Time" of the TParticle productionVtx 4-vector:
//   //        we set the time compontent to zero locally here since we want just to compute
//   //        the *spatial* position of the productionVtx making use of ROOT functions for that

//   prodVtx1.SetT(0.);
//   prodVtx2.SetT(0.);
  
//   if ( prodVtx1 == prodVtx2 )
//     {
//       fVtxRadius = (prodVtx1.Vect()).Mag2(); // magnitude of 3-v part of LorentzVector "prodVtx"
//     }
//   else
//     {
//       fVtxRadius = TMath::Max( (prodVtx1.Vect()).Mag2(), (prodVtx2.Vect()).Mag2());
//     }

  double Vtx_x1 = fParticle1.Vx();
  double Vtx_x2 = fParticle2.Vx();
  
  if ( Vtx_x1 == Vtx_x2 )
    {
      fxVtx = Vtx_x1;
    }
  else if ( TMath::Abs(Vtx_x1) < TMath::Abs(Vtx_x2) )
    {
      fxVtx = Vtx_x2;
    }
  else if ( TMath::Abs(Vtx_x2) < TMath::Abs(Vtx_x1) )
    {
      fxVtx = Vtx_x1;
    }

  //_____________________________________________________________________________

  fCutMask = 0;
  fPureSimulPair = false; // default
}

//_____________________________________________________________________________
effPair::~effPair()
{
}

//_____________________________________________________________________________
void
effPair::identify(ostream& os) const
{
  os << "effPair::effPair" << endl;
}

//_____________________________________________________________________________
const effParticle&
effPair::Particle(size_t i) const
{
  if ( i == 0 ) 
    {
      return fParticle1;
    }
  else if ( i == 1 ) 
    {
      return fParticle2;
    }
  else
    {
      assert(0==1);
    }
}

//_____________________________________________________________________________
void
effPair::Print(Option_t* option) const
{
  printf("effPair:");

  TString opt = option;
  if (opt == "red")
    {
      printf(" pT=%e Minv=%e \n",Pt(),Minv());
    }
  else
    {
      printf("pT=%e Minv=%e Cos=%e Asym=%e Mask=%d\n",Pt(),Minv(),Cosine(),Asymmetry(),CutMask());
      //if (fPureSimulPair) printf("PureSimulatedPair=%d",fPureSimulPair);
    }
  fParticle1.Print(option);
  fParticle2.Print(option);
}
