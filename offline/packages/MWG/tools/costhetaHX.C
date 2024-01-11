#include "Tools.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include <cmath>

// Calculates helicity angle
// Angle between lepton in J/psi rest frame and 
// J/psi momentum in beams CM frame (same as lab. frame for colliders)
// MASS: muon mass, P0[3]: 3-momentum of positive charge, P1[3]: 3-momentum of negative charge 
//___________________________________________________________________
Float_t Tools::costhetaHX(Float_t MASS, Float_t P0[3], Float_t P1[3])
{  
  //put mu+ and mu- into 4 vectors
  Float_t E0 = Tools::E(MASS,P0);
  Float_t E1 = Tools::E(MASS,P1);
  TLorentzVector mu_plus = TLorentzVector(P0[0],P0[1],P0[2],E0);
  TLorentzVector mu_minus = TLorentzVector(P1[0],P1[1],P1[2],E1);

  //put j/psi into a 4 vector
  Float_t dimass = Tools::invMass(MASS,P0,MASS,P1);
  Float_t diP[3]; diP[0]=P0[0]+P1[0]; diP[1]=P0[1]+P1[1]; diP[2]=P0[2]+P1[2];
  Float_t diE = Tools::E(dimass,diP);
  TLorentzVector jpsi = TLorentzVector(diP[0],diP[1],diP[2],diE);

  TVector3 boostV =  -jpsi.BoostVector();

  //lets boost eveybody into the j/psi frame
  mu_plus.Boost(boostV);  mu_minus.Boost(boostV);

  //and find angle to the j/psi
  TVector3 vjpsi = jpsi.Vect();
  Float_t thetaHX = mu_plus.Angle(vjpsi);
  
  return cos(thetaHX);
}
