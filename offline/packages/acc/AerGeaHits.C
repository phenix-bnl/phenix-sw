#include "AerGeaHits.h"
#include "phool.h"
#include <iostream>

using namespace std;

ClassImp(AerGeaHits)

AerGeaHits::AerGeaHits()
{
}

unsigned int AerGeaHits::get_nhits()
{
  cout << PHWHERE << "in virtual base class" << endl;
  return 0;
}

AerPISAHit *AerGeaHits::get_AerPISAHits(unsigned int ihit)
{
  cout << PHWHERE << "in virtual base class" << endl;
  return 0;
}

void AerGeaHits::set_nhits(unsigned int)
{
  cout << PHWHERE << "in virtual base class" << endl;
}

void AerGeaHits::set_AerPISAHits( AerPISAHit *hitsarray )
{
  cout << PHWHERE << "in virtual base class" << endl;
}

void AerGeaHits::identify(std::ostream& os=std::cout) const
{
  os << "virtual AerGeaHits object" << endl;
}

void AerGeaHits::Reset()
{
  cout << PHWHERE << "Reset not implemented" << endl;
}

int AerGeaHits::isValid() const
{
  cout << PHWHERE << "isValid() not implemented" << endl;
  return 0;
}

