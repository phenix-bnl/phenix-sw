#include "Tools.h"
#include <PHMuoTracksOut.h>
#include <cmath>

//====distance track vertex is from nominal vertex (0,0,BbcZVertex)
Float_t Tools::dist_zvtx(PHMuoTracksOut* &muo, Int_t itrk)
{
  //fyi: track z at vertex equals BBC z-vertex
  Float_t x = muo->get_xpos(0,itrk);
  Float_t y = muo->get_ypos(0,itrk);
  Float_t dist = sqrt(x*x + y*y);
  return dist;
}
