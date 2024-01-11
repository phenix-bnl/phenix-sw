#include "mpcClusterContainer.h"
#include "mpcClusterContent.h"
#include "TClonesArray.h"

ClassImp(mpcClusterContainer)

//_____________________________________________________________________________
mpcClusterContainer& 
mpcClusterContainer::operator=(const mpcClusterContainer&)
{
  return *this;
}

//_____________________________________________________________________________
void
mpcClusterContainer::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getCluster(i)->print(out);
    }
}

mpcClusterContent*
mpcClusterContainer::addCluster(const mpcClusterContent &clus)
{
  // First check if TC array exists (it does not for the base class
  if (!GetTCArray())
    {
      return NULL;
    }
  // this is a bit ugly but GetLast returns the  index-1, so the argument
  // for the ExpandCreate is  GetLast() + 2
  int nnew;
    nnew = GetTCArray()->GetLast() + 2;
  // this is a TCArray method, it creates a new Object of
  // the type which is stored in the TCArray. It uses the default ctor
  GetTCArray()->ExpandCreate(nnew);
  mpcClusterContent *newclus = static_cast<mpcClusterContent *> (GetTCArray()->UncheckedAt(GetTCArray()->GetLast()));
  // Since the ExpandCreate calls the default ctor we still need to copy
  // the actual values from the input particle
  newclus->Copy(clus);
  return newclus;
}
