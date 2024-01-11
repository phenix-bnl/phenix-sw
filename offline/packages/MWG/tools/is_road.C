#include "Tools.h"
#include <PHMuoTracksOut.h>

//================================================================================
//returns true if there is a road of gap depth 2,3,4 associated with the track
Bool_t Tools::is_road(PHMuoTracksOut* &muo, Int_t itrk)
{
  for(Int_t road_index = 2; road_index >= 0; road_index--)
    if(muo->get_muIDOOhits(road_index, itrk)) return true;
  return false;
}
//================================================================================
//returns true if the deepest road associated with the track is to gap 2 or 3
Bool_t Tools::is_road_shallow(PHMuoTracksOut* &muo, Int_t itrk)
{
  if(muo->get_muIDOOhits(2, itrk)) return false; //gap 4
  if(muo->get_muIDOOhits(1, itrk) ||             //gap 3
     muo->get_muIDOOhits(0, itrk)) return true;  //gap 2
  return false;
}
