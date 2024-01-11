#include "Tools.h"
#include <cmath>

//======================= Compute particle transverse momentum
Float_t Tools::pT(Float_t p[3])
{
  Float_t pt = std::sqrt(p[0]*p[0] + p[1]*p[1]);
  return pt;
}
//============================================================
Float_t Tools::pT(Float_t px, Float_t py)
{
    Float_t pt = std::sqrt(px*px + py*py);
    return pt;
}

