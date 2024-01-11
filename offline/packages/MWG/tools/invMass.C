#include "Tools.h"
#include <cmath>

//================================== Compute invariant mass
//________________________________________________________________
Float_t Tools::invMass(Float_t mass1, Float_t P1[3], Float_t mass2, Float_t P2[3])
{
    Float_t Etot = Tools::E(mass1,P1) + Tools::E(mass2,P2);
    Float_t InvMass = sqrt(Etot*Etot 
			   - (P1[0]+P2[0])*(P1[0]+P2[0]) 
			   - (P1[1]+P2[1])*(P1[1]+P2[1]) 
			   - (P1[2]+P2[2])*(P1[2]+P2[2])); 
    return InvMass;
}

//________________________________________________________________
Float_t Tools::invMass(
  Float_t mass1, Float_t px1, Float_t py1, Float_t pz1,
	Float_t mass2, Float_t px2, Float_t py2, Float_t pz2)
{
    Float_t Etot = Tools::E(mass1, px1, py1, pz1) + Tools::E(mass2, px2, py2, pz2);
    Float_t InvMass = sqrt(Etot*Etot - (px1+px2)*(px1+px2) - (py1+py2)*(py1+py2) - (pz1+pz2)*(pz1+pz2));
    return InvMass;
}
