#include "ZdcGeaHitsv1.h"
#include <iostream>

using namespace std;

ClassImp(ZdcGeaHitsv1)

ZdcGeaHitsv1::ZdcGeaHitsv1()
{
  nzdcgeahits = 0;
  zdcgeahits = 0;
  Reset();
}

void ZdcGeaHitsv1::identify(ostream& os) const
{
  os << "I am a ZdcGeaHitsv1 object" << endl;
}

void ZdcGeaHitsv1::Reset()
{
  nzdcgeahits = 0;
  zdcgeahits = 0;
}
