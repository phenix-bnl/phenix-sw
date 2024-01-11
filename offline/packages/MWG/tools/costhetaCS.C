#include "Tools.h"
#include <cmath>

// Calculates polarization angle in Collins-Soper frame
// Angle between lepton in J/psi rest frame and a bisector of the angle
// between two colliding protons in J/psi rest frame.
// MASS: muon mass, P0[3]: 3-momentum of positive charge, P1[3]: 3-momentum of negative charge
//___________________________________________________________________
Float_t Tools::costhetaCS(Float_t MASS, Float_t P0[3], Float_t P1[3])
{
    //  the formula are valid only for MASS0=MASS1
    Float_t dimass = Tools::invMass(MASS,P0,MASS,P1);
    Float_t diP[3]; diP[0]=P0[0]+P1[0]; diP[1]=P0[1]+P1[1]; diP[2]=P0[2]+P1[2];
    Float_t CosthetaCS = ( Tools::E(MASS,P1)*P0[2] - Tools::E(MASS,P0)*P1[2] )
      / std::sqrt(dimass*dimass + Tools::pT(diP)*Tools::pT(diP))
      / std::sqrt(dimass*dimass/4 - MASS*MASS);
    return CosthetaCS;
}
