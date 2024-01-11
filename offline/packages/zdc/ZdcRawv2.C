#include "ZdcRawv2.h"

ClassImp(ZdcRawv2);

using namespace std;

// you should use only this constructor
// when instantiating a ZdcRawv2 Object
ZdcRawv2::ZdcRawv2(const Int_t npmt)
{
  ZdcRawNpmt = npmt;
  adc = new short[npmt];
  tdc0 = new short[npmt];
  tdc1 = new short[npmt];
  Clear();
}

ZdcRawv2::~ZdcRawv2()
{
  delete [] adc;
  delete [] tdc0;
  delete [] tdc1;
}

void ZdcRawv2::Reset()
{
  Clear();
}

void ZdcRawv2::identify(ostream& out) const
{
  out << "identify yourself: I am a ZdcRawv2 object" << endl;
  return;
}

int ZdcRawv2::isValid() const
{
  if ( adc[0] < 0 ) return 0;	// not valid
  return 1;			// valid
}

void ZdcRawv2::Clear(Option_t *option)
{
  for (int ipmt=0; ipmt<ZdcRawNpmt; ipmt++)
    {
      adc[ipmt] = INVALID_SHORT;
      tdc0[ipmt] = INVALID_SHORT;
      tdc1[ipmt] = INVALID_SHORT;
    }
}

