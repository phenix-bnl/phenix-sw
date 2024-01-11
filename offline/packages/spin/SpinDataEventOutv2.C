// SpinDataEventOutv2 Class
//
//  This class will use the following packets.
//
//   GL1     : Global Level 1 trigger
//           
//   GL1P    : A scaler which has scaler count for each bunch crossing
//             The input of GL1P is four of GL1 scaler count.
//             GL1P takes a difference between GL1 scaler count of a trigger
//             for the current recorded event to previous recorded event of the
//             beam crossing. This gives us how many collisions of the trigger
//             for the bunch crossing. This information will be used for
//             the luminosity and the relative luminosity study.
//             During run 3 p+p,  we have two board board.
//
//   GL1PSum : A scaler sum from start of run to the current event for beam
//             crossing. This numbers are summed up on DCM. We have two GL1P
//             board for year 2, therfore this packet has 2*4=8 trigger information
//             for scaler sum. Also there is a corossing counter which count how
//             many time DCM summed for the crossing.


#include "SpinDataEventOutv2.h"

ClassImp(SpinDataEventOutv2)
//________________________________________________________________________________

SpinDataEventOutv2::SpinDataEventOutv2()
{ 
  Reset(); 
}
//________________________________________________________________________________

SpinDataEventOutv2::~SpinDataEventOutv2()
{
  // So far notthing to be done
}
//________________________________________________________________________________

void SpinDataEventOutv2::Reset()
{
  // Initialize variables

  EventSequence = 0;
  GL1CrossingID = 0;
  for (int i=0 ; i<nGL1PBoard ; i++){
    GL1PEventNumber[i] = 0;
    GL1PCrossingID[i] = 0;
    SpinGL1PCrossingID[i] = 0;
    for (int j=0 ; j<nGL1PScaler ; j++){
      GL1PScalerCount[i][j]    = 0;
    }
  }
  GL1PSumEventNumber = 0;
  GL1PSumCrossingID = 0;
  for (int i=0 ; i<nGL1PBoard ; i++){
    for (int j=0 ; j<nGL1PScaler ; j++){
      GL1PSumScalerCount[i][j] = 0;
    }
  }
  SpinGL1CrossingID = 0;
  for (int i=0 ; i<nGL1PBoard ; i++){
    SpinGL1PCrossingID[i] = 0;
  }
  SpinDirectionBlueFromV124 = 0;
  SpinDirectionYellowFromV124 = 0;
  GL1PSumCrossingCount = 0;
  SpinGL1PSumCrossingID = 0;
}
//________________________________________________________________________________
