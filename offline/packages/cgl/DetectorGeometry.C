#include "DetectorGeometry.h"
#include "phool.h"

ClassImp(DetectorGeometry)

using namespace std;

void DetectorGeometry::Reset()
{
  cerr << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}

void DetectorGeometry::identify(ostream& os) const
{
  os << "identify yourself:  DetectorGeometry Object" << endl;
  return ;
}

int DetectorGeometry::isValid() const
{
  cerr << PHWHERE << "isValid not implemented by daughter class" << endl;
  return 0;
}

