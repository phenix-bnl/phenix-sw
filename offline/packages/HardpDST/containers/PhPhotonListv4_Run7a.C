#include "PhPhotonListv4_Run7a.h"
#include "PhPhotonSnglv2.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(PhPhotonListv4_Run7a);

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

const unsigned int PhPhotonListv4_Run7a::fgDefaultSize = 1000;
const unsigned int PhPhotonListv4_Run7a::fgMaxSize = 24768;

//_____________________________________________________________________________
PhPhotonListv4_Run7a::PhPhotonListv4_Run7a() : fEmcClusters(0), evtFilterType(0)
{
  allocate(PhPhotonListv4_Run7a::fgDefaultSize);
}

//_____________________________________________________________________________
PhPhotonListv4_Run7a::PhPhotonListv4_Run7a(const PhPhotonListv4_Run7a& o) : fEmcClusters(0), evtFilterType(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
PhPhotonListv4_Run7a&
PhPhotonListv4_Run7a::operator=(const PhPhotonListv4_Run7a& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
PhPhotonListv4_Run7a::~PhPhotonListv4_Run7a()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonListv4_Run7a::addCluster(unsigned int i)
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
    
  PhPhotonSnglv2* rv = new((*fEmcClusters)[i]) PhPhotonSnglv2;
  return rv;
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonListv4_Run7a::addCluster(unsigned int i, const emcClusterContent& c)
{
  const PhPhotonSnglv2* test = 
    dynamic_cast<const PhPhotonSnglv2*>(&c);

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
    
  return new((*fEmcClusters)[i]) PhPhotonSnglv2(*test);
}

//_____________________________________________________________________________
void
PhPhotonListv4_Run7a::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray("PhPhotonSnglv2",thesize);
}

//_____________________________________________________________________________
unsigned int
PhPhotonListv4_Run7a::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
PhPhotonListv4_Run7a*
PhPhotonListv4_Run7a::clone(void) const
{
  return new PhPhotonListv4_Run7a(*this);
}

//_____________________________________________________________________________
PhPhotonListv4_Run7a*
PhPhotonListv4_Run7a::create(void) const
{
  return new PhPhotonListv4_Run7a;
}

//_____________________________________________________________________________
void
PhPhotonListv4_Run7a::copy(PhPhotonListv4_Run7a& dest) const
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

  dest.evtFilterType = evtFilterType;
  unsigned int idest = 0;

  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      PhPhotonSnglv2* clus = dest.addCluster(idest++);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonListv4_Run7a::findCluster(int clusterid) const
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
PhPhotonSnglv2*
PhPhotonListv4_Run7a::getCluster(unsigned int i) const
{
  return static_cast<PhPhotonSnglv2*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
PhPhotonListv4_Run7a::identify(ostream& os) const
{
  cout << "PhPhotonListv4_Run7a::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
PhPhotonListv4_Run7a::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
PhPhotonListv4_Run7a::Reset()
{
  clear(*fEmcClusters);
  evtFilterType = 0;
}

//_____________________________________________________________________________
bool
PhPhotonListv4_Run7a::removeCluster(unsigned int i)
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
PhPhotonListv4_Run7a::resize(unsigned int newsize)
{
  if ( newsize < PhPhotonListv4_Run7a::fgMaxSize )
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
PhPhotonListv4_Run7a::size(void) const
{
  return fEmcClusters->GetLast()+1;
}

