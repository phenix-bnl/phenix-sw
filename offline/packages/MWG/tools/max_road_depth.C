#include "Tools.h"
#include <PHMuoTracksOut.h>

// returns depth of deepest road associated with the track
Int_t Tools::max_road_depth(PHMuoTracksOut* &muo, Int_t itrk)
{
  // Each new framework track could associate with three muid roads,
  // one road for each gap from gap 2 to gap 4 if it is possible, 
  // here we determine the deepest road. 
  
  Int_t last_gap = 0;  //gap 2 by default
  
  for(Int_t road_index = 2; road_index >= 0; road_index--) {
    //a road has hits so if the road exists a nonzero number will 
    //be returned when its hits are requested
    //
    if(muo->get_muIDOOhits(road_index, itrk)) {
      // When we filled the muIDOOhit array, we filled it in the following way,
      // the array index of the gap bit of the associated road is the depth of the road minus 2.
      //
      last_gap = road_index;
      break;
    }
  }
  return  last_gap; //returns 0, 1, 2 for gap 2, 3, 4,
                    //note that if no road exists 0 will still be returned
}

