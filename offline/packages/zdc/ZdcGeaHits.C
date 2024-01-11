#include <iostream>
#include "ZdcGeaHits.h"
#include "phool.h"

using namespace std;

ClassImp(ZdcGeaHits)

ZdcGeaHits::ZdcGeaHits()
{
}

unsigned int ZdcGeaHits::get_nhits()
{
  cout << PHWHERE << "in virtual base class" << endl;
  return 0;
}

ZdcPISAHit *ZdcGeaHits::get_ZdcPISAHits(unsigned int ihit)
{
  cout << PHWHERE << "in virtual base class" << endl;
  return 0;
}

void ZdcGeaHits::set_nhits(unsigned int)
{
  cout << PHWHERE << "in virtual base class" << endl;
}

void ZdcGeaHits::set_ZdcPISAHits( ZdcPISAHit *hitsarray )
{
  cout << PHWHERE << "in virtual base class" << endl;
}

void ZdcGeaHits::identify(ostream& os) const
{
  os << "virtual ZdcGeaHits object" << endl;
}

void ZdcGeaHits::Reset()
{
  cout << PHWHERE << "Reset not implemented" << endl;
}

int ZdcGeaHits::isValid() const
{
  cout << PHWHERE << "isValid() not implemented" << endl;
  return 0;
}

