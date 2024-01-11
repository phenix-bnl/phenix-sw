#include "Tools.h"
#include <cmath>

//================================== Compute particle x2
//____________________________________________________
Float_t Tools::x2(Float_t invMass, Float_t p[3], Float_t E_CMS)
{
  Float_t X2  = (invMass/E_CMS)*std::exp(-1*Tools::rapidity(invMass,p)); 
    return X2;
}

//____________________________________________________
Float_t Tools::x2(Float_t invMass, Float_t px, Float_t py, Float_t pz, Float_t E_CMS)
{
    Float_t p[3]={px,py,pz};
    return Tools::x2(invMass,p,E_CMS);
}
