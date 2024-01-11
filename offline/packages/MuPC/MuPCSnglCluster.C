#include "MuPCSnglCluster.h"
#include "phool.h"
#include <iostream>

ClassImp(MuPCSnglCluster)

using namespace std;

void
MuPCSnglCluster::warning(const char* field) const
{
  cout << PHWHERE << "using virtual function, doing nothing" << endl;
  cout << "Single MUPC CLUSTER Offending field == " << field << endl;
}
