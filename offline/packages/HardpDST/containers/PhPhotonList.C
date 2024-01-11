#include "PhPhotonList.h"
#include "PhPhotonSngl.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(PhPhotonList);

using namespace std;

namespace {
  void clear(TClonesArray& array)
  {
#if ROOT_VERSION_CODE > ROOT_VERSION(3,01,5)
    // for recent version, it's taken care of by ROOT itself. Easy.
    array.Clear("C");
#else
    // we have do to it ourselves.
    int n = array.GetEntriesFast();
    for ( int i = 0; i < n; ++i )
      {
	TObject* obj = array.At(i);
	if ( obj )
	  {
	    obj->Clear();
	  }
      }
    array.SetOwner(false);
    array.Clear();
#endif
  }
}

const unsigned int PhPhotonList::fgDefaultSize = 1000;
const unsigned int PhPhotonList::fgMaxSize = 24768;

//_____________________________________________________________________________
PhPhotonList::PhPhotonList() : fEmcClusters(0)
{
  allocate(PhPhotonList::fgDefaultSize);
}

//_____________________________________________________________________________
PhPhotonList::PhPhotonList(const PhPhotonList& o) : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
PhPhotonList&
PhPhotonList::operator=(const PhPhotonList& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
PhPhotonList::~PhPhotonList()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
PhPhotonSngl*
PhPhotonList::addCluster(unsigned int i)
{
  if ( static_cast<int>(i) > fEmcClusters->GetSize() ) 
    {
      bool ok = resize(std::max(capacity()*2,fgMaxSize));
      if ( !ok ) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }
    
  PhPhotonSngl* rv = new((*fEmcClusters)[i]) PhPhotonSngl;
  return rv;
}

//_____________________________________________________________________________
PhPhotonSngl*
PhPhotonList::addCluster(unsigned int i, const emcClusterContent& c)
{
  const PhPhotonSngl* test = 
    dynamic_cast<const PhPhotonSngl*>(&c);

  if (!test)
    {
      cerr << PHWHERE << " emcClusterContent is not of type v4"
	   << endl;
      return 0;
    }

   if ( static_cast<int>(i) > fEmcClusters->GetSize() ) 
    {
      bool ok = resize(std::max(capacity()*2,fgMaxSize));
      if ( !ok ) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }
    
  return new((*fEmcClusters)[i]) PhPhotonSngl(*test);
}

//_____________________________________________________________________________
void
PhPhotonList::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray("PhPhotonSngl",thesize);
}

//_____________________________________________________________________________
unsigned int
PhPhotonList::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
PhPhotonList*
PhPhotonList::clone(void) const
{
  return new PhPhotonList(*this);
}

//_____________________________________________________________________________
PhPhotonList*
PhPhotonList::create(void) const
{
  return new PhPhotonList;
}

//_____________________________________________________________________________
void
PhPhotonList::copy(PhPhotonList& dest) const
{
  TClonesArray* destarray = dest.fEmcClusters;
  if ( !destarray ) 
    {
      dest.allocate(fEmcClusters->GetSize());
      destarray = dest.fEmcClusters;
    }
  else
    {
      dest.Reset();
    }

  unsigned int idest = 0;

  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      PhPhotonSngl* clus = dest.addCluster(idest++);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
PhPhotonSngl*
PhPhotonList::findCluster(int clusterid) const
{
  for ( size_t i = 0; i < size(); ++i ) 
    {
      if ( getCluster(i)->id() == clusterid ) 
	{
	  return getCluster(i);
	}
    }
  return 0;
}

//_____________________________________________________________________________
PhPhotonSngl*
PhPhotonList::getCluster(unsigned int i) const
{
  return static_cast<PhPhotonSngl*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
PhPhotonList::identify(ostream& os) const
{
  cout << "PhPhotonList::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
PhPhotonList::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
PhPhotonList::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
bool
PhPhotonList::removeCluster(unsigned int i)
{
  if ( i < size() ) 
    {
      fEmcClusters->RemoveAt(i);
      fEmcClusters->Compress();
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
PhPhotonList::resize(unsigned int newsize)
{
  if ( newsize < PhPhotonList::fgMaxSize )
    {
      fEmcClusters->Expand(newsize);
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
unsigned int
PhPhotonList::size(void) const
{
  return fEmcClusters->GetLast()+1;
}

