#include "BbcNorthSouth.h"

using namespace std;

ClassImp(BbcNorthSouth)

BbcNorthSouth::BbcNorthSouth(const short npmt, const float chargesum, 
                             const float timing)
{
  nPmt   = npmt;
  ChargeSum = chargesum;
  Timing = timing;
}


void BbcNorthSouth::identify(ostream& out) const
{
  out << "identify yourself: I am a BbcNorthSouth object" << endl;
}
