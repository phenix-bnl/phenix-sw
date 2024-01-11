#include "Tools.h"
#include <cmath>

//======================== Compute particle pseudorap
Float_t Tools::pseudorap(Float_t p0[3])
{
  if (p(p0) == p0[2]) return -999.999; 
  Float_t pseudorap = 0.5*std::log((Tools::p(p0)+p0[2])/(Tools::p(p0)-p0[2]));
  return pseudorap;
}
//===================================================
Float_t Tools::pseudorap(Float_t px, Float_t py, Float_t pz)
{
  if (Tools::p(px,py,pz) == pz) return -999.999;
  Float_t pseudorap = 0.5*std::log((Tools::p(px,py,pz)+pz)/(Tools::p(px,py,pz)-pz));
  return pseudorap;
}
