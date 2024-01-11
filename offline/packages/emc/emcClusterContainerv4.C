#include "emcClusterContainerv4.h"
#include "emcClusterContentv4.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcClusterContainerv4);

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

const unsigned int emcClusterContainerv4::fgDefaultSize = 1000;
const unsigned int emcClusterContainerv4::fgMaxSize = 24768;

//_____________________________________________________________________________
emcClusterContainerv4::emcClusterContainerv4() : fEmcClusters(0)
{
  allocate(emcClusterContainerv4::fgDefaultSize);
}

//_____________________________________________________________________________
emcClusterContainerv4::emcClusterContainerv4(const emcClusterContainerv4& o) : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcClusterContainerv4&
emcClusterContainerv4::operator=(const emcClusterContainerv4& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcClusterContainerv4::~emcClusterContainerv4()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
emcClusterContentv4*
emcClusterContainerv4::addCluster(unsigned int i)
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
    
  emcClusterContentv4* rv = new((*fEmcClusters)[i]) emcClusterContentv4;
  return rv;
}

//_____________________________________________________________________________
emcClusterContentv4*
emcClusterContainerv4::addCluster(unsigned int i, const emcClusterContent& c)
{
  const emcClusterContentv4* test = 
    dynamic_cast<const emcClusterContentv4*>(&c);

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
    
  return new((*fEmcClusters)[i]) emcClusterContentv4(*test);
}

//_____________________________________________________________________________
void
emcClusterContainerv4::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray("emcClusterContentv4",thesize);
}

//_____________________________________________________________________________
unsigned int
emcClusterContainerv4::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
emcClusterContainerv4*
emcClusterContainerv4::clone(void) const
{
  return new emcClusterContainerv4(*this);
}

//_____________________________________________________________________________
emcClusterContainerv4*
emcClusterContainerv4::create(void) const
{
  return new emcClusterContainerv4;
}

//_____________________________________________________________________________
void
emcClusterContainerv4::copy(emcClusterContainerv4& dest) const
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
      emcClusterContentv4* clus = dest.addCluster(i);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
emcClusterContentv4*
emcClusterContainerv4::findCluster(int clusterid) const
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
emcClusterContentv4*
emcClusterContainerv4::getCluster(unsigned int i) const
{
  return static_cast<emcClusterContentv4*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
emcClusterContainerv4::identify(ostream& os) const
{
  cout << "emcClusterContainerv4::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
emcClusterContainerv4::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcClusterContainerv4::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
bool
emcClusterContainerv4::removeCluster(unsigned int i)
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
emcClusterContainerv4::resize(unsigned int newsize)
{
  if ( newsize < emcClusterContainerv4::fgMaxSize )
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
emcClusterContainerv4::size(void) const
{
  return fEmcClusters->GetLast()+1;
}

