#include "Tools.h"

//================================== Compute particle xF
//____________________________________________________
Float_t Tools::xF(Float_t X1, Float_t X2)
{
    Float_t XF = X1-X2;
    return XF;
}

//____________________________________________________
Float_t Tools::xF(Float_t invMass, Float_t P[3], Float_t E_CMS)
{
    Float_t XF = Tools::x1(invMass,P,E_CMS)-Tools::x2(invMass,P,E_CMS);
    return XF;
}

//____________________________________________________
Float_t Tools::xF(Float_t invMass, Float_t px, Float_t py, Float_t pz, Float_t E_CMS)
{
    Float_t P[3]={px,py,pz};
    return Tools::xF(invMass,P,E_CMS);
}

