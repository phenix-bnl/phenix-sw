
#include "ReactionPlaneObjectv2.h"

using namespace std;

ClassImp(ReactionPlaneObjectv2)

using namespace std;

ReactionPlaneObjectv2::ReactionPlaneObjectv2()
{
  Reset();
}

int
ReactionPlaneObjectv2::isValid() const
{
  return((BBCrp00 >-9999.) ? 1 : 0);
}

void ReactionPlaneObjectv2::Reset()
{
  BBCsumW00 = -9999.;
  BBCsumW01 = -9999.;
  BBCsumW02 = -9999.;
  BBCsumW10 = -9999.;
  BBCsumW11 = -9999.;
  BBCsumW12 = -9999.;
  SMDsumW00 = -9999.;
  SMDsumW01 = -9999.;
  SMDsumW02 = -9999.;
  FCLsumW00 = -9999.;
  FCLsumW01 = -9999.;
  FCLsumW02 = -9999.;
  CNTsumW10 = -9999.;
  CNTsumW11 = -9999.;
  CNTsumW12 = -9999.;
  CNTsumW13 = -9999.;
  CNTsumW14 = -9999.;

  BBCsumX00 = -9999.;
  BBCsumX01 = -9999.;
  BBCsumX02 = -9999.;
  BBCsumX10 = -9999.;
  BBCsumX11 = -9999.;
  BBCsumX12 = -9999.;
  BBCsumY00 = -9999.;
  BBCsumY01 = -9999.;
  BBCsumY02 = -9999.;
  BBCsumY10 = -9999.;
  BBCsumY11 = -9999.;
  BBCsumY12 = -9999.;

  SMDsumX00 = -9999.;
  SMDsumX01 = -9999.;
  SMDsumX02 = -9999.;
  SMDsumY00 = -9999.;
  SMDsumY01 = -9999.;
  SMDsumY02 = -9999.;
 
  FCLsumX00 = -9999.;
  FCLsumX01 = -9999.;
  FCLsumX02 = -9999.;
  FCLsumY00 = -9999.;
  FCLsumY01 = -9999.;
  FCLsumY02 = -9999.;
  
  CNTsumX10 = -9999.;
  CNTsumX11 = -9999.;
  CNTsumX12 = -9999.;
  CNTsumX13 = -9999.;
  CNTsumX14 = -9999.;
  CNTsumY10 = -9999.;
  CNTsumY11 = -9999.;
  CNTsumY12 = -9999.;
  CNTsumY13 = -9999.;
  CNTsumY14 = -9999.;

  BBCrp00 = -9999.;
  BBCrp01 = -9999.;
  BBCrp02 = -9999.;
  BBCrp10 = -9999.;
  BBCrp11 = -9999.;
  BBCrp12 = -9999.;

  SMDrp00 = -9999.;
  SMDrp01 = -9999.;
  SMDrp02 = -9999.;

  FCLrp00 = -9999.;
  FCLrp01 = -9999.;
  FCLrp02 = -9999.;

  CNTrp10 = -9999.;
  CNTrp11 = -9999.;
  CNTrp12 = -9999.;
  CNTrp13 = -9999.;
  CNTrp14 = -9999.;
}

void ReactionPlaneObjectv2::identify(ostream& os) const
{
  os << "identify yourself: ReactionPlaneObjectv2, Reaction Plane variables." << endl;
}
