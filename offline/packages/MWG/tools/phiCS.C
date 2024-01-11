#include "Tools.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include <cmath>

// Calculates phi angle in Collins-Soper frame
// in the range -pi to pi
// z-axis defined by bisector of the angle
// between two colliding protons in J/psi rest frame.
// MASS: muon mass, P0[3]: 3-momentum of positive charge, P1[3]: 3-momentum of negative charge
// beamP[3]: 3-momentum of proton in beam, useful if you'd like 
//             to look at 'forward' and 'backward' separately
//             or need to change beam energy
//___________________________________________________________________
using namespace std;

Float_t Tools::phiCS(Float_t MASS, Float_t P0[3], Float_t P1[3], Float_t beamP[3])
{  
  //put mu+ and mu- into 4 vectors
  Float_t E0 = Tools::E(MASS,P0);
  Float_t E1 = Tools::E(MASS,P1);
  TLorentzVector mu_plus = TLorentzVector(P0[0],P0[1],P0[2],E0);
  TLorentzVector mu_minus = TLorentzVector(P1[0],P1[1],P1[2],E1);

  //put beam into a 4 vector
  // proton mass from PDG on June 30, 2009
  Float_t beamE = Tools::E(0.928272013,beamP);
  TLorentzVector beam1 = TLorentzVector(beamP[0],beamP[1],beamP[2],beamE);
  TLorentzVector beam2 = TLorentzVector(-beamP[0],-beamP[1],-beamP[2],beamE);

  //put j/psi into a 4 vector
  Float_t dimass = Tools::invMass(MASS,P0,MASS,P1);
  Float_t diP[3]; diP[0]=P0[0]+P1[0]; diP[1]=P0[1]+P1[1]; diP[2]=P0[2]+P1[2];
  Float_t diE = Tools::E(dimass,diP);
  TLorentzVector jpsi = TLorentzVector(diP[0],diP[1],diP[2],diE);

  TVector3 boostV =  -jpsi.BoostVector();

  TVector3 vbeam1 = beam1.Vect();
  TVector3 vbeam2 = beam2.Vect();

  //lets boost eveybody into the j/psi frame
  beam1.Boost(boostV);  beam2.Boost(boostV);  mu_plus.Boost(boostV);  mu_minus.Boost(boostV);

  vbeam1 = beam1.Vect();
  vbeam2 = beam2.Vect();

  TVector3 vCS = vbeam1.Unit()-vbeam2.Unit();

  //get axes
  TVector3 zaxis = vCS.Unit();
  TVector3 yaxis = vCS.Cross(vbeam1.Unit());
  yaxis = yaxis.Unit();
  TVector3 xaxis = yaxis.Cross(zaxis);
  xaxis = xaxis.Unit();

  //get into the correct frame
  TRotation frame;
  frame.SetZAxis(zaxis,xaxis);
  //Need to rotate back to this frame
  frame = frame.Inverse();

  TVector3 vmu_plus = mu_plus.Vect();
  vmu_plus.Transform(frame);

  return vmu_plus.Phi();
}
