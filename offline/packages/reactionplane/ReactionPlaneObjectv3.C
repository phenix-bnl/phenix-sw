
#include <ReactionPlaneObjectv3.h>

using namespace std;

ClassImp(ReactionPlaneObjectv3)

using namespace std;

ReactionPlaneObjectv3::ReactionPlaneObjectv3()
{
  Reset();
}

int
ReactionPlaneObjectv3::isValid() const
{
  return((getBBCrp00() >-9999.) ? 1 : 0);
}

void ReactionPlaneObjectv3::Reset()
{
  ReactionPlaneObjectv2::Reset();
  RXNsumW00 = -9999.;
  RXNsumW01 = -9999.;
  RXNsumW02 = -9999.;
  RXNsumW03 = -9999.;
  RXNsumW04 = -9999.;
  RXNsumW05 = -9999.;
  RXNsumW06 = -9999.;
  RXNsumW07 = -9999.;
  RXNsumW08 = -9999.;

  RXNsumW10 = -9999.;
  RXNsumW11 = -9999.;
  RXNsumW12 = -9999.;
  RXNsumW13 = -9999.;
  RXNsumW14 = -9999.;
  RXNsumW15 = -9999.;
  RXNsumW16 = -9999.;
  RXNsumW17 = -9999.;
  RXNsumW18 = -9999.;

  MPCsumW00 = -9999.;
  MPCsumW01 = -9999.;
  MPCsumW02 = -9999.;

  MPCsumW10 = -9999.;
  MPCsumW11 = -9999.;
  MPCsumW12 = -9999.;

//###################################################//


  RXNrp00 = -9999.;
  RXNrp01 = -9999.;
  RXNrp02 = -9999.;
  RXNrp03 = -9999.;
  RXNrp04 = -9999.;
  RXNrp05 = -9999.;
  RXNrp06 = -9999.;
  RXNrp07 = -9999.;
  RXNrp08 = -9999.;

  RXNrp10 = -9999.;
  RXNrp11 = -9999.;
  RXNrp12 = -9999.;
  RXNrp13 = -9999.;
  RXNrp14 = -9999.;
  RXNrp15 = -9999.;
  RXNrp16 = -9999.;
  RXNrp17 = -9999.;
  RXNrp18 = -9999.;

  MPCrp00 = -9999.;
  MPCrp01 = -9999.;
  MPCrp02 = -9999.;

  MPCrp10 = -9999.;
  MPCrp11 = -9999.;
  MPCrp12 = -9999.;

//###################################################################//

}

void ReactionPlaneObjectv3::identify(ostream& os) const
{
  os << "identify yourself: ReactionPlaneObjectv3, Reaction Plane variables." << endl;
}
