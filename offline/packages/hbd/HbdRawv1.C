
#include "HbdRawv1.h"

ClassImp(HbdRawv1)

using namespace std;

HbdRawv1::HbdRawv1()
{

  Reset();

  return;

}

HbdRawv1::HbdRawv1(HbdRawv1 *raw)
{

  if (!raw) return;

  padid = raw->get_padid();
  for(int i=0;i<HBDSAMPLE;i++){
    clock[i] = raw->get_clock(i);
    rawadc[i] = raw->get_rawadc(i);
    charge[i] = raw->get_charge(i);
  }

  return;

}

void HbdRawv1::identify(ostream& os) const
{
  os << "identify yourself: HbdRawv1 Object\n" << std::endl;
  return;
}

void HbdRawv1::Reset()
{

  padid = -9999;
  for(int i=0;i<HBDSAMPLE;i++){
    clock[i] = -9999;
    rawadc[i] = -9999;
    charge[i] = -9999.;
  }

  return;
}

int HbdRawv1::isValid() const
{
  return 1;
}

