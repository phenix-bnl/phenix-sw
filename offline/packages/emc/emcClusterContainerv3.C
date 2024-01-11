#include "emcClusterContainerv3.h"
#include "emcClusterContentv3.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcClusterContainerv3);

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

const unsigned int emcClusterContainerv3::fgDefaultSize = 1000;
const unsigned int emcClusterContainerv3::fgMaxSize = 24768;

//_____________________________________________________________________________
emcClusterContainerv3::emcClusterContainerv3() : fEmcClusters(0)
{
  allocate(emcClusterContainerv3::fgDefaultSize);
}

//_____________________________________________________________________________
emcClusterContainerv3::emcClusterContainerv3(const emcClusterContainerv3& o) : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcClusterContainerv3&
emcClusterContainerv3::operator=(const emcClusterContainerv3& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcClusterContainerv3::~emcClusterContainerv3()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
emcClusterContentv3*
emcClusterContainerv3::addCluster(unsigned int i)
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
    
  emcClusterContentv3* rv = new((*fEmcClusters)[i]) emcClusterContentv3;
  return rv;
}

//_____________________________________________________________________________
emcClusterContentv3*
emcClusterContainerv3::addCluster(unsigned int i, const emcClusterContent& c)
{
  const emcClusterContentv3* test = 
    dynamic_cast<const emcClusterContentv3*>(&c);

  if (!test)
    {
      cerr << PHWHERE << " emcClusterContent is not of type v3"
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
    
  return new((*fEmcClusters)[i]) emcClusterContentv3(*test);
}

//_____________________________________________________________________________
void
emcClusterContainerv3::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray("emcClusterContentv3",thesize);
}

//_____________________________________________________________________________
unsigned int
emcClusterContainerv3::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
emcClusterContainerv3*
emcClusterContainerv3::clone(void) const
{
  return new emcClusterContainerv3(*this);
}

//_____________________________________________________________________________
emcClusterContainerv3*
emcClusterContainerv3::create(void) const
{
  return new emcClusterContainerv3;
}

//_____________________________________________________________________________
void
emcClusterContainerv3::copy(emcClusterContainerv3& dest) const
{
  if ( !dest.fEmcClusters )
    {
      dest.allocate(this->fEmcClusters->GetSize());
    }
  else
    {
      dest.Reset();
    }

  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      emcClusterContentv3* clus = dest.addCluster(i);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
emcClusterContentv3*
emcClusterContainerv3::findCluster(int clusterid) const
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
emcClusterContentv3*
emcClusterContainerv3::getCluster(unsigned int i) const
{
  return static_cast<emcClusterContentv3*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
emcClusterContainerv3::identify(ostream& os) const
{
  cout << "emcClusterContainerv3::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
emcClusterContainerv3::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcClusterContainerv3::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
bool
emcClusterContainerv3::removeCluster(unsigned int i)
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
emcClusterContainerv3::resize(unsigned int newsize)
{
  if ( newsize < emcClusterContainerv3::fgMaxSize )
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
emcClusterContainerv3::size(void) const
{
  return fEmcClusters->GetLast()+1;
}
