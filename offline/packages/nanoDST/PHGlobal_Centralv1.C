#include "PHGlobal_Centralv1.h"

using namespace std;

ClassImp(PHGlobal_Centralv1)

  PHGlobal_Centralv1::PHGlobal_Centralv1()
{
  Reset();
}

void
PHGlobal_Centralv1::Reset()
{
  ndc = -999;
  npc1 = -999;
  npc2 = -999;
  npc3 = -999;
  ntec = -999;
  nemc = -999;
  ntof = -999;
  ncrk = -999;
  etote = -999.;
  etotw = -999.;
}

void PHGlobal_Centralv1::identify(std::ostream& os) const
{
  os << "identify yourself: PHGlobal_Centralv1 Object, Global Event Information." << std::endl;
}

int PHGlobal_Centralv1::isValid() const
{
  return ((ndc > -1) ? 1 : 0);
}

