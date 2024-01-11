
#include "ReactionPlaneObjectv1.h"

using namespace std;

ClassImp(ReactionPlaneObjectv1)

using namespace std;

ReactionPlaneObjectv1::ReactionPlaneObjectv1()
{
  Reset();
}

int
ReactionPlaneObjectv1::isValid() const
{
  return((BBCrp00 >-9999.) ? 1 : 0);
}

void ReactionPlaneObjectv1::Reset()
{
  BBCrp00 = -9999.;
  BBCrp01 = -9999.;
  BBCrp02 = -9999.;
  BBCrp10 = -9999.;
  BBCrp11 = -9999.;
  BBCrp12 = -9999.;

  SMDrp00 = -9999.;
  SMDrp01 = -9999.;
  SMDrp02 = -9999.;

  MVDrp00 = -9999.;
  MVDrp01 = -9999.;
  MVDrp02 = -9999.;
  MVDrp10 = -9999.;
  MVDrp11 = -9999.;
  MVDrp12 = -9999.;

  FCLrp00 = -9999.;
  FCLrp01 = -9999.;
  FCLrp02 = -9999.;

  CNTrp10 = -9999.;
  CNTrp11 = -9999.;
  CNTrp12 = -9999.;
  CNTrp13 = -9999.;
  CNTrp14 = -9999.;
}

void ReactionPlaneObjectv1::identify(ostream& os) const
{
  os << "identify yourself: ReactionPlaneObjectv1, Reaction Plane variables." << endl;
}
