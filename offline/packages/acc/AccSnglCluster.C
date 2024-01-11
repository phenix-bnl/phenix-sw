#include "AccSnglCluster.h"
#include "phool.h"
#include <iostream>

ClassImp(AccSnglCluster)

using namespace std;

void
AccSnglCluster::warning(const char* field) const
{
  cout << PHWHERE << "using virtual function, doing nothing" << endl;
  cout << "Single ACC CLUSTER Offending field == " << field << endl;
}
