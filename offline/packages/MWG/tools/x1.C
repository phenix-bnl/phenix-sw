#include "Tools.h"
#include <cmath> 

//================================== Compute particle x1
//____________________________________________________
Float_t Tools::x1(Float_t invMass, Float_t p[3], Float_t E_CMS)
{
  Float_t X1  = (invMass/E_CMS)*std::exp(rapidity(invMass,p)); 
  return X1;
}
//____________________________________________________
Float_t Tools::x1(Float_t invMass, Float_t px, Float_t py, Float_t pz, Float_t E_CMS)
{
    Float_t p[3]={px,py,pz};
    return Tools::x1(invMass,p,E_CMS);
}
