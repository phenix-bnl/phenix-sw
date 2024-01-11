#include "CglSnglTrackv9.h"

ClassImp(CglSnglTrackv9)

// v8 adds tofw. Only put tofw-specific code here and inherit the rest from v7.
CglSnglTrackv9::CglSnglTrackv9()
{
  for (int i=0; i<TECPLANES; i++)
    tecplaneid[i] = -1;
  return;
}
