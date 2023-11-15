#include "MutPISAPara.h"

/*!
  \file  MutPISAPara.cc
  \brief event independent MUT parameters
  \author  T. K. Ghosh
  \version $Revision: 1.5 $
  \date    $Date: 2007/11/13 22:27:50 $
*/

ClassImp(MutPISAPara)

using namespace std;

// Initialize static data members
vector<MutPISAPara> MutPISAPara::_hits;

//________________________________________________________________________
MutPISAPara::MutPISAPara(const Int_t iData [], const Float_t fData [])
{
  
  int i, j, k, p = 0;

  //  float junk = fData[p++];
  p++;
  mumtrflg = (Int_t)fData[p++];
  mum_arms = (Int_t)fData[p++];
  mum_stations = (Int_t)fData[p++];
  mum_channels = (Int_t)fData[p++];
  mum_color = (Int_t)fData[p++];
  ArmMedium = (Int_t)fData[p++];
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 3; i++) {
      PlanesPerStation[j*3+i] = (Int_t)fData[p++];
    }
  }
  StationMedium = (Int_t)fData[p++];
  HoneyMedium = (Int_t)fData[p++];
  FEEFlag = (Int_t)fData[p++];
  for (i = 0; i < 8; i++) {
    StationOneFrame[i] = fData[p++];
  }
  for (i = 0; i < 8; i++) {
    StationOnePanel[i] = fData[p++];
  }
  for (i = 0; i < 4; i++) {
    StationOneAnode[i] = fData[p++];
  }
  for (i = 0; i < 8; i++) {
    StationOneRead[i] = fData[p++];
  }
  for (i = 0; i < 7; i++) {
    StationOneRib[i] = fData[p++];
  }
  for (i = 0; i < 8; i++) {
    StationOneGas[i] = fData[p++];
  }
  for (i = 0; i < 7; i++) {
    StationOneMount[i] = fData[p++];
  }
  for (i = 0; i < 10; i++) {
    StationOneFee[i] = fData[p++];
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 4; i++) {
      for (k=0; k<6; k++){
        StationOneAngles[j*4*6 + i*6 + k] = fData[p++];
      }
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 4; i++) {
      for (k=0; k<3; k++){
        StationOneOffsets[j*4*3 + i*3 + k] = fData[p++];
      }
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 7; i++) {
      StationTwoFFrame[j*7 + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 2; i++) {
      StationTwoBFrame[2*j + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 2; i++) {
      StationTwoFBar[2*j + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 2; i++) {
      StationTwoBBar[2*j + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    StationTwoAnode[j] = fData[p++];
  }
  StationTwoFoilThickness = fData[p++];
  StationTwoAlFoilThickness = fData[p++];
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 4; i++) {
      StationTwoFRib[4*j + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 4; i++) {
      StationTwoBRib[4*j + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 6; i++) {
      StationTwoFGas[6*j + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    StationTwoBGas[j] = fData[p++];
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 8; i++) {
      for (k=0; k<6; k++){
        StationTwoAngles[j*8*6 + i*6 + k] = fData[p++];
      }
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 8; i++) {
      for (k=0; k<3; k++){
        StationTwoOffsets[j*8*3 + i*3 + k] = fData[p++];
      }
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 9; i++) {
      StationThreeFrame[j*9 + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 7; i++) {
      StationThreePanel[j*7 + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 5; i++) {
      StationThreeAnode[j*5 + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 4; i++) {
      StationThreeRib[j*4 + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 6; i++) {
      StationThreeGas[j*6 + i] = fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 8; i++) {
      for (k=0; k<6; k++){
        StationThreeAngles[j*8*6 + i*6 + k] = fData[p++];
      }
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 8; i++) {
      for (k=0; k<3; k++){
        StationThreeOffsets[j*8*3 + i*3 + k] = fData[p++];
      }
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 3; i++) {
      FrameMedium[j*3 + i] = (Int_t)fData[p++];
    }
  }
  for (j = 0; j < 2; j++) {
    for (i = 0; i < 3+1; i++) {
      StationNominalZpos[j*(3+1) + i] = fData[p++];
    }
  }
  SpokeMedium = (Int_t)fData[p++];

}


