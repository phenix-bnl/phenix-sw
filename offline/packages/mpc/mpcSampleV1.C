#include <mpcSampleV1.h>

ClassImp(mpcSampleV1)

using namespace std;

mpcSampleV1::mpcSampleV1()
{
  sample  = -99;
  ch      = -9999;
  adc     = -9999;
}

mpcSampleV1::mpcSampleV1(const mpcSample &s)
{
  sample = s.get_sample();
  ch     = s.get_ch();
  adc    = s.get_adc();
}

void mpcSampleV1::print(std::ostream& out)
{
  out << ch << "\t" << sample << "\t" << adc << endl;
}

