#include "EmcSnglCalibTowerv1.h"

ClassImp(EmcSnglCalibTowerv1)

EmcSnglCalibTowerv1::EmcSnglCalibTowerv1()
{
  id = -999;
  type = -999;

  deadmap = -99999;
  hwkey = -99999;
  index = -99999;
  swkey = -99999;
  warnmap = -99999;

  adc = -9999.9;
  ecal = -9999.9;
  tac = -9999.9;
  tof = -9999.9;
  return;
}
