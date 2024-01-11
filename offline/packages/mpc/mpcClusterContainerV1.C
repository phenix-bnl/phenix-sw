#include "mpcClusterContainerV1.h"
#include "mpcClusterContentV1.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(mpcClusterContainerV1);

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

const unsigned int mpcClusterContainerV1::fgDefaultSize = 1000;
const unsigned int mpcClusterContainerV1::fgMaxSize = 24768;

//_____________________________________________________________________________
mpcClusterContainerV1::mpcClusterContainerV1() : fMpcClusters(0)
{
  allocate(mpcClusterContainerV1::fgDefaultSize);
}

//_____________________________________________________________________________
mpcClusterContainerV1::mpcClusterContainerV1(const mpcClusterContainerV1& o) : fMpcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
mpcClusterContainerV1&
mpcClusterContainerV1::operator=(const mpcClusterContainerV1& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
mpcClusterContainerV1::~mpcClusterContainerV1()
{
  clear(*fMpcClusters);
  delete fMpcClusters;
}

//_____________________________________________________________________________
mpcClusterContentV1*
mpcClusterContainerV1::addCluster(unsigned int i)
{
  if ( static_cast<int>(i) > fMpcClusters->GetSize() ) 
    {
      bool ok = resize(std::max(capacity()*2,fgMaxSize));
      if ( !ok ) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }
    
  mpcClusterContentV1* rv = new((*fMpcClusters)[i]) mpcClusterContentV1;
  return rv;
}

//_____________________________________________________________________________
mpcClusterContentV1*
mpcClusterContainerV1::addCluster(unsigned int i, const mpcClusterContent& c)
{
  const mpcClusterContentV1* test = 
    dynamic_cast<const mpcClusterContentV1*>(&c);

  if (!test)
    {
      cerr << PHWHERE << " mpcClusterContent is not of type v4"
	   << endl;
      return 0;
    }

   if ( static_cast<int>(i) > fMpcClusters->GetSize() ) 
    {
      bool ok = resize(std::max(capacity()*2,fgMaxSize));
      if ( !ok ) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }
    
  return new((*fMpcClusters)[i]) mpcClusterContentV1(*test);
}

//_____________________________________________________________________________
void
mpcClusterContainerV1::allocate(unsigned int thesize)
{
  if (fMpcClusters)
    {
      clear(*fMpcClusters);
      delete fMpcClusters;
    }
  fMpcClusters = new TClonesArray("mpcClusterContentV1",thesize);
}

//_____________________________________________________________________________
unsigned int
mpcClusterContainerV1::capacity(void) const
{ 
  return fMpcClusters->GetSize(); 
}

//_____________________________________________________________________________
mpcClusterContainerV1*
mpcClusterContainerV1::clone(void) const
{
  return new mpcClusterContainerV1(*this);
}

//_____________________________________________________________________________
mpcClusterContainerV1*
mpcClusterContainerV1::create(void) const
{
  return new mpcClusterContainerV1;
}

//_____________________________________________________________________________
void
mpcClusterContainerV1::copy(mpcClusterContainerV1& dest) const
{
  TClonesArray* destarray = dest.fMpcClusters;
  if ( !destarray ) 
    {
      dest.allocate(fMpcClusters->GetSize());
      destarray = dest.fMpcClusters;
    }
  else
    {
      dest.Reset();
    }

  unsigned int idest = 0;

  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      mpcClusterContentV1* clus = dest.addCluster(idest++);
// need to fix this coherently
      *clus = *static_cast<mpcClusterContentV1*>(getCluster(i));
    }
}

//_____________________________________________________________________________
mpcClusterContent*
mpcClusterContainerV1::findCluster(int clusterid) const
{
  for ( size_t i = 0; i < size(); ++i ) 
    {
      if ( getCluster(i)->id() == clusterid ) 
	{
	  return static_cast<mpcClusterContentV1*>(getCluster(i));
	}
    }
  return 0;
}

//_____________________________________________________________________________
mpcClusterContent*
mpcClusterContainerV1::getCluster(unsigned int i) const
{
  return static_cast<mpcClusterContentV1*>(fMpcClusters->At(i));
}

//_____________________________________________________________________________
void
mpcClusterContainerV1::identify(ostream& os) const
{
  cout << "mpcClusterContainerV1::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
mpcClusterContainerV1::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
mpcClusterContainerV1::Reset()
{
  clear(*fMpcClusters);
}

//_____________________________________________________________________________
bool
mpcClusterContainerV1::removeCluster(unsigned int i)
{
  if ( i < size() ) 
    {
      fMpcClusters->RemoveAt(i);
      fMpcClusters->Compress();
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
mpcClusterContainerV1::resize(unsigned int newsize)
{
  if ( newsize < mpcClusterContainerV1::fgMaxSize )
    {
      fMpcClusters->Expand(newsize);
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
unsigned int
mpcClusterContainerV1::size(void) const
{
  return fMpcClusters->GetLast()+1;
}

