#include "Tools.h"
#include <cmath>
#include <PHMuoTracksOut.h>
//================================== Compute particle momentum
Float_t Tools::p(Float_t p0[3])
{
    Float_t P = sqrt(p0[0]*p0[0] + p0[1]*p0[1] + p0[2]*p0[2]);
    return P;
}
//============================================================
Float_t Tools::p(Float_t px, Float_t py, Float_t pz)
{
    Float_t P = sqrt(px*px + py*py + pz*pz);
    return P;
}
//============================================================
Float_t Tools::p(PHMuoTracksOut* &muo, Int_t idx)
{
  Float_t px = muo->get_px(0,idx);
  Float_t py = muo->get_py(0,idx);
  Float_t pz = muo->get_pz(0,idx);
  Float_t P = sqrt(px*px + py*py + pz*pz);
  return P;
}
