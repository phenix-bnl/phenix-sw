// $Id: E.C,v 1.1 2007/10/02 19:39:32 mwysocki Exp $

/*
	\file E.C
	\brief calculates Energy
*/

#include "Tools.h"
#include <cmath>

//================================== Compute particle momentum
Float_t Tools::E(Float_t e[4])
{
    Float_t P[3]={e[1],e[2],e[3]};
    Float_t E = sqrt(e[0]*e[0] + Tools::p(P)*Tools::p(P));
    return E;
}
//============================================================
Float_t Tools::E(Float_t mass, Float_t p0[3])
{
    Float_t E = sqrt(mass*mass + p(p0)*p(p0));
    return E;
}
//============================================================
Float_t Tools::E(Float_t mass, Float_t px, Float_t py, Float_t pz)
{
    Float_t P[3]={px,py,pz};
    Float_t E = sqrt(mass*mass + Tools::p(P)*Tools::p(P));
    return E;
}
