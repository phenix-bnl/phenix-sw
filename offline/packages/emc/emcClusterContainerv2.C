#include "emcClusterContainerv2.h"
#include "emcClusterContentv2.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcClusterContainerv2);

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

const unsigned int emcClusterContainerv2::fgDefaultSize = 1000;
const unsigned int emcClusterContainerv2::fgMaxSize = 24768;

//_____________________________________________________________________________
emcClusterContainerv2::emcClusterContainerv2() : fEmcClusters(0)
{
  allocate(emcClusterContainerv2::fgDefaultSize);
}

//_____________________________________________________________________________
emcClusterContainerv2::emcClusterContainerv2(const emcClusterContainerv2& o) : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcClusterContainerv2&
emcClusterContainerv2::operator=(const emcClusterContainerv2& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcClusterContainerv2::~emcClusterContainerv2()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
emcClusterContentv2*
emcClusterContainerv2::addCluster(unsigned int i)
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
    
  emcClusterContentv2* rv = new((*fEmcClusters)[i]) emcClusterContentv2;
  return rv;
}

//_____________________________________________________________________________
emcClusterContentv2*
emcClusterContainerv2::addCluster(unsigned int i, const emcClusterContent& c)
{
  const emcClusterContentv2* test = 
    dynamic_cast<const emcClusterContentv2*>(&c);

  if (!test)
    {
      cerr << PHWHERE << " emcClusterContent is not of type v2"
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
    
  return new((*fEmcClusters)[i]) emcClusterContentv2(*test);
}

//_____________________________________________________________________________
void
emcClusterContainerv2::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray("emcClusterContentv2",thesize);
}

//_____________________________________________________________________________
unsigned int
emcClusterContainerv2::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
emcClusterContainerv2*
emcClusterContainerv2::clone(void) const
{
  return new emcClusterContainerv2(*this);
}

//_____________________________________________________________________________
emcClusterContainerv2*
emcClusterContainerv2::create(void) const
{
  return new emcClusterContainerv2;
}

//_____________________________________________________________________________
void
emcClusterContainerv2::copy(emcClusterContainerv2& dest) const
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
      emcClusterContentv2* clus = dest.addCluster(i);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
emcClusterContentv2*
emcClusterContainerv2::findCluster(int clusterid) const
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
emcClusterContentv2*
emcClusterContainerv2::getCluster(unsigned int i) const
{
  return static_cast<emcClusterContentv2*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
emcClusterContainerv2::identify(ostream& os) const
{
  cout << "emcClusterContainerv2::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
emcClusterContainerv2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcClusterContainerv2::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
bool
emcClusterContainerv2::removeCluster(unsigned int i)
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
emcClusterContainerv2::resize(unsigned int newsize)
{
  if ( newsize < emcClusterContainerv2::fgMaxSize )
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
emcClusterContainerv2::size(void) const
{
  return fEmcClusters->GetLast()+1;
}
