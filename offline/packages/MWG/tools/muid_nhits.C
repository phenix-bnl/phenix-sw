#include "Tools.h"
#include <PHMuoTracksOut.h>

// returns number of hits in the muid for the specified road
//=============================================================================
Int_t Tools::muid_nhits(PHMuoTracksOut* &muo, Int_t itrk, Int_t iroad)
{
  Float_t bits_hit = (Float_t) muo->get_muIDOOhits(iroad, itrk);
  Int_t hits = (Int_t) Tools::sumbit(bits_hit);
  return hits;
}
//=============================================================================
Int_t Tools::muid_nhits(PHMuoTracksOut* &muo, Int_t itrk)
{
  Int_t iroad = Tools::max_road_depth(muo, itrk);  //will use deepest road
  Float_t bits_hit = (Float_t) muo->get_muIDOOhits(iroad, itrk);
  Int_t hits = (Int_t) Tools::sumbit(bits_hit);
  return hits;
}
