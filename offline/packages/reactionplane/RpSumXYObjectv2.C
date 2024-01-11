#include <RpSumXYObjectv2.h>

using namespace std;

ClassImp(RpSumXYObjectv2)

using namespace std;

RpSumXYObjectv2::RpSumXYObjectv2()
{
  Reset();
}

int
RpSumXYObjectv2::isValid() const
{
  return((getBBCsumW0() >-9999.) ? 1 : 0);
}

void RpSumXYObjectv2::Reset()
{
  RpSumXYObjectv1::Reset();
  RXNsumW0 = -9999.;
  RXNsumW1 = -9999.;
  RXNsumW2 = -9999.;
  RXNsumW3 = -9999.;
  RXNsumW4 = -9999.;
  RXNsumW5 = -9999.;
  RXNsumW6 = -9999.;
  RXNsumW7 = -9999.;
  RXNsumW8 = -9999.;

  MPCsumW0 = -9999.;
  MPCsumW1 = -9999.;
  MPCsumW2 = -9999.;

//####################################################################//

  RXNsumX00 = -9999.;
  RXNsumX01 = -9999.;
  RXNsumX02 = -9999.;
  RXNsumX03 = -9999.;
  RXNsumX04 = -9999.;
  RXNsumX05 = -9999.;
  RXNsumX06 = -9999.;
  RXNsumX07 = -9999.;
  RXNsumX08 = -9999.;
  RXNsumY00 = -9999.;
  RXNsumY01 = -9999.;
  RXNsumY02 = -9999.;
  RXNsumY03 = -9999.;
  RXNsumY04 = -9999.;
  RXNsumY05 = -9999.;
  RXNsumY06 = -9999.;
  RXNsumY07 = -9999.;
  RXNsumY08 = -9999.;

  RXNsumX10 = -9999.;
  RXNsumX11 = -9999.;
  RXNsumX12 = -9999.;
  RXNsumX13 = -9999.;
  RXNsumX14 = -9999.;
  RXNsumX15 = -9999.;
  RXNsumX16 = -9999.;
  RXNsumX17 = -9999.;
  RXNsumX18 = -9999.;
  RXNsumY10 = -9999.;
  RXNsumY11 = -9999.;
  RXNsumY12 = -9999.;
  RXNsumY13 = -9999.;
  RXNsumY14 = -9999.;
  RXNsumY15 = -9999.;
  RXNsumY16 = -9999.;
  RXNsumY17 = -9999.;
  RXNsumY18 = -9999.;

  MPCsumX00 = -9999.;
  MPCsumX01 = -9999.;
  MPCsumX02 = -9999.;
  MPCsumY00 = -9999.;
  MPCsumY01 = -9999.;
  MPCsumY02 = -9999.;

  MPCsumX10 = -9999.;
  MPCsumX11 = -9999.;
  MPCsumX12 = -9999.;
  MPCsumY10 = -9999.;
  MPCsumY11 = -9999.;
  MPCsumY12 = -9999.;

}

void RpSumXYObjectv2::identify(ostream& os) const
{
  os << "identify yourself: RpSumXYObjectv2, Reaction Plane variables." << endl;
}
