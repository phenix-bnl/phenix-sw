#include "emcClusterContainerv1.h"
#include "emcClusterContentv1.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcClusterContainerv1);

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

const unsigned int emcClusterContainerv1::fgDefaultSize = 1000;
const unsigned int emcClusterContainerv1::fgMaxSize = 24768;

//_____________________________________________________________________________
emcClusterContainerv1::emcClusterContainerv1() : fEmcClusters(0)
{
  allocate(emcClusterContainerv1::fgDefaultSize);
}

//_____________________________________________________________________________
emcClusterContainerv1::emcClusterContainerv1(const emcClusterContainerv1& o) : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcClusterContainerv1&
emcClusterContainerv1::operator=(const emcClusterContainerv1& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcClusterContainerv1::~emcClusterContainerv1()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
emcClusterContentv1*
emcClusterContainerv1::addCluster(unsigned int i)
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
    
  emcClusterContentv1* rv = new((*fEmcClusters)[i]) emcClusterContentv1;
  return rv;
}

//_____________________________________________________________________________
emcClusterContentv1*
emcClusterContainerv1::addCluster(unsigned int i, const emcClusterContent& c)
{
  const emcClusterContentv1* test = 
    dynamic_cast<const emcClusterContentv1*>(&c);

  if (!test)
    {
      cerr << PHWHERE << " emcClusterContent is not of type v1"
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
    
  return new((*fEmcClusters)[i]) emcClusterContentv1(*test);
}

//_____________________________________________________________________________
void
emcClusterContainerv1::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray("emcClusterContentv1",thesize);
}

//_____________________________________________________________________________
unsigned int
emcClusterContainerv1::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
emcClusterContainerv1*
emcClusterContainerv1::clone(void) const
{
  return new emcClusterContainerv1(*this);
}

//_____________________________________________________________________________
emcClusterContainerv1*
emcClusterContainerv1::create(void) const
{
  return new emcClusterContainerv1;
}

//_____________________________________________________________________________
void
emcClusterContainerv1::copy(emcClusterContainerv1& dest) const
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
      emcClusterContentv1* clus = dest.addCluster(i);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
emcClusterContentv1*
emcClusterContainerv1::findCluster(int clusterid) const
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
emcClusterContentv1*
emcClusterContainerv1::getCluster(unsigned int i) const
{
  return static_cast<emcClusterContentv1*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
emcClusterContainerv1::identify(ostream& os) const
{
  cout << "emcClusterContainerv1::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
emcClusterContainerv1::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcClusterContainerv1::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
bool
emcClusterContainerv1::removeCluster(unsigned int i)
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
emcClusterContainerv1::resize(unsigned int newsize)
{
  if ( newsize < emcClusterContainerv1::fgMaxSize )
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
emcClusterContainerv1::size(void) const
{
  return fEmcClusters->GetLast()+1;
}
