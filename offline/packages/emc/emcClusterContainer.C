#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "TClonesArray.h"

ClassImp(emcClusterContainer)

//_____________________________________________________________________________
emcClusterContainer& 
emcClusterContainer::operator=(const emcClusterContainer&)
{
  return *this;
}

//_____________________________________________________________________________
void
emcClusterContainer::print(std::ostream& out) const
{
  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      getCluster(i)->print(out);
    }
}

emcClusterContent*
emcClusterContainer::addCluster(const emcClusterContent &clus)
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
  emcClusterContent *newclus = static_cast<emcClusterContent *> (GetTCArray()->UncheckedAt(GetTCArray()->GetLast()));
  // Since the ExpandCreate calls the default ctor we still need to copy
  // the actual values from the input particle
  newclus->Copy(clus);
  return newclus;
}
