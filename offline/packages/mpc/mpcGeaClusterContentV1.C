#include <mpcGeaClusterContentV1.h>
#include <MpcMap.h>

ClassImp(mpcGeaClusterContentV1)

using namespace std;

mpcGeaClusterContentV1::mpcGeaClusterContentV1()
{
  ch         = -9999;
  itorigin   = -9999;
  idorigin   = -9999;
  itincoming = -9999;
  idincoming = -9999;
  edep       = 0.;
  fraction   = 0.;
  fraction_tr = 0;

  fType = -9999;
  fID = -1;
  fEnergy = -9999;
  fParentType = -9999;
  fParentID = -1;

}

mpcGeaClusterContentV1::mpcGeaClusterContentV1(const mpcGeaClusterContent &m)
{
  ch         = m.get_ch();
  itorigin   = m.get_itorigin();
  idorigin   = m.get_idorigin();
  itincoming = m.get_itincoming();
  idincoming = m.get_idincoming();
  edep       = m.get_edep();
  fraction   = m.get_fraction();
  fraction_tr = m.get_fraction_tr();

  fType = m.get_type();
  fID = m.get_id();
  fEnergy = m.get_py_energy();
  fParentType = m.get_parent_type();
  fParentID = m.get_parent_id();
   
}

void mpcGeaClusterContentV1::print(std::ostream& out = std::cout) const
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
      << setw(10) << fraction_tr
      << "\n Pythia info: "
      << setw(6) << fType
      << setw(6) << fID
      << setw(10) << fEnergy
      << setw(6) << fParentType
      << setw(6) << fParentID
      << endl;
}

