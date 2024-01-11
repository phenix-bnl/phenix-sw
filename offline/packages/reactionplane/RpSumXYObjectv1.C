
#include "RpSumXYObjectv1.h"

using namespace std;

ClassImp(RpSumXYObjectv1)

using namespace std;

RpSumXYObjectv1::RpSumXYObjectv1()
{
  Reset();
}

int
RpSumXYObjectv1::isValid() const
{
  return((BBCsumW0 >-9999.) ? 1 : 0);
}

void RpSumXYObjectv1::Reset()
{
  BBCsumW0 = -9999.;
  BBCsumW1 = -9999.;
  BBCsumW2 = -9999.;
  FCLsumW0 = -9999.;
  FCLsumW1 = -9999.;
  FCLsumW2 = -9999.;
  CNTsumW0 = -9999.;
  CNTsumW1 = -9999.;
  CNTsumW2 = -9999.;
  CNTsumW3 = -9999.;
  CNTsumW4 = -9999.;

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

}

void RpSumXYObjectv1::identify(ostream& os) const
{
  os << "identify yourself: RpSumXYObjectv1, Reaction Plane variables." << endl;
}
