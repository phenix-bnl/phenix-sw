#include "Tools.h"
#include <cmath>

//========================= Compute particle rapidity
Float_t Tools::rapidity(Float_t e[4]) // e[0]=mass, e[i]=p[i]
{
    Float_t rapidity = 0.5*std::log((Tools::E(e)+e[3])/(Tools::E(e)-e[3]));
    return rapidity;
}
//===================================================
Float_t Tools::rapidity(Float_t mass, Float_t p[3])
{
    Float_t rapidity = 0.5*std::log((Tools::E(mass,p)+p[2])/(E(mass,p)-p[2]));
    return rapidity;
}

//===================================================
Float_t Tools::rapidity(Float_t mass, Float_t px, Float_t py, Float_t pz)
{
    Float_t rapidity = 0.5*std::log((Tools::E(mass,px,py,pz)+pz)/(Tools::E(mass,px,py,pz)-pz));
    return rapidity;
}

//===================================================
Float_t Tools::rapidity(Float_t mass1, Float_t px1, Float_t py1, Float_t pz1, 
			Float_t mass2, Float_t px2, Float_t py2, Float_t pz2)
{
    Float_t Etot = Tools::E(mass1,px1,py1,pz1) + Tools::E(mass2,px2,py2,pz2);
    Float_t pz = pz1+pz2;
    Float_t rapidity = 0.5*std::log((Etot+pz)/(Etot-pz));
    return rapidity;
}
