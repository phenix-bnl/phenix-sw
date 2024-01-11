#include "CglSnglTrackv6.h"

ClassImp(CglSnglTrackv6)

CglSnglTrackv6::CglSnglTrackv6()
{
  arm = -1;
  id = -1;
  dctracksid = -1;
  tectrackid = -1;
  pc1clusid = -1;
  pc2clusid = -1;
  pc3clusid = -1;
  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
    svxclusid[ilayer] = -1;
  }
  tofrecid = -1;
  accrecid = -1;
  mrpcrecid = -1;
  emcclusid = -1;
  richringid = -1;
  trackModel = -1;
  quality = -999.9;
  return;
}
