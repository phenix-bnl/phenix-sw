#include "TecClusterContainer.hh"
#include "phool.h"
#include <iostream>

ClassImp(TecClusterContainer)
using namespace std;

void
TecClusterContainer::Reset()
{
  cout << PHWHERE << "ERROR Reset() not implemented by daughter class" << endl;
  return ;
}
int
TecClusterContainer::isValid() const
{
  cout << PHWHERE << "isValid() not implemented by daughter class" << endl;
  return 0;
}

void
TecClusterContainer::identify(ostream& os) const
{
  os << "virtual TecOut object." << endl;
}

TecCluster*
TecClusterContainer::getTecCluster(const unsigned int i) const
{
  if (!GetTecClusters())
    {
      cout << "No TecCluster TClonesArray, calling identify to show you to whom you are talking:" << endl;
      identify();
      return NULL;
    }

  return (TecCluster*)GetTecClusters()->UncheckedAt(i);
}
