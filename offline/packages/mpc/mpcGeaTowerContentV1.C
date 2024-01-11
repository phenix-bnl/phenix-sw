#include <mpcGeaTowerContentV1.h>
#include <recoConsts.h>
#include <MpcMap.h>

ClassImp(mpcGeaTowerContentV1)

using namespace std;

mpcGeaTowerContentV1::mpcGeaTowerContentV1()
{
  ch         = -9999;
  itorigin   = -9999;
  idorigin   = -9999;
  itincoming = -9999;
  idincoming = -9999;
  edep       = 0.;
  fraction   = 0.;
}

mpcGeaTowerContentV1::mpcGeaTowerContentV1(const mpcGeaTowerContent &m)
{
  ch         = m.get_ch();
  itorigin   = m.get_itorigin();
  idorigin   = m.get_idorigin();
  itincoming = m.get_itincoming();
  idincoming = m.get_idincoming();
  edep       = m.get_edep();
  fraction   = m.get_fraction();
}

void mpcGeaTowerContentV1::print(std::ostream& out = std::cout) const
{
  MpcMap *mpcmap = MpcMap::instance();
  out << ch << "\t"
      << setw(4) << mpcmap->getGridX(ch)
      << setw(4) << mpcmap->getGridY(ch)
      << setw(6) << itorigin
      << setw(6) << idorigin
      << setw(6) << itincoming
      << setw(6) << idincoming
      << setw(10) << edep
      << setw(10) << fraction
      << endl;
}

