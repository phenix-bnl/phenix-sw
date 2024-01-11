#include "BbcRawHit.h"
#include <iostream>

ClassImp(BbcRawHit)

using namespace std;

BbcRawHit::BbcRawHit(const short pmt, const short adc, 
                     const short tdc0, const short tdc1)
{
  Pmt = pmt;
  Adc = adc;
  Tdc0 = tdc0;
  Tdc1 = tdc1;
}


void BbcRawHit::identify(ostream& out) const
{
  out << "identify yourself: I am a BbcRawHit object" << endl;
}
