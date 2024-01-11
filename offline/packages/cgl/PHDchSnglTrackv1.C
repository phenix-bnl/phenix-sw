#include "PHDchSnglTrackv1.h"

ClassImp(PHDchSnglTrackv1)

PHDchSnglTrackv1::PHDchSnglTrackv1()
{

  ErrorCode = -999;
  numberOfSuccessfulIterations = -999;
  numberOfX1X2hitsFitted = -999;

  chi2 = -9999.9;
  fittedAlpha = -9999.9;
  fittedBeta = -9999.9;
  fittedPhi = -9999.9;
  fittedPhi0 = -9999.9;
  fittedTheta0 = -9999.9;
  momentum = -9999.9;
}


