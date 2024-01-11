#include "AerGeaHitsv1.h"
#include <iostream>

using namespace std;

ClassImp(AerGeaHitsv1)

AerGeaHitsv1::AerGeaHitsv1()
{
  naergeahits = 0;
  aergeahits = 0;
  Reset();
}

void AerGeaHitsv1::identify(std::ostream& os=std::cout) const
{
  os << "I am a AerGeaHitsv1 object" << endl;
}

void AerGeaHitsv1::Reset()
{
  naergeahits = 0;
  aergeahits = 0;
}
