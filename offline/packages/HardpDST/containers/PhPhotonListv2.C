#include "PhPhotonListv2.h"
#include "PhPhotonSnglv2.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(PhPhotonListv2);

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

const unsigned int PhPhotonListv2::fgDefaultSize = 1000;
const unsigned int PhPhotonListv2::fgMaxSize = 24768;

//_____________________________________________________________________________
PhPhotonListv2::PhPhotonListv2() : fEmcClusters(0)
{
  allocate(PhPhotonListv2::fgDefaultSize);
}

//_____________________________________________________________________________
PhPhotonListv2::PhPhotonListv2(const PhPhotonListv2& o) : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
PhPhotonListv2&
PhPhotonListv2::operator=(const PhPhotonListv2& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
PhPhotonListv2::~PhPhotonListv2()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonListv2::addCluster(unsigned int i)
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
PhPhotonListv2::addCluster(unsigned int i, const emcClusterContent& c)
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
PhPhotonListv2::allocate(unsigned int thesize)
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
PhPhotonListv2::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}

//_____________________________________________________________________________
PhPhotonListv2*
PhPhotonListv2::clone(void) const
{
  return new PhPhotonListv2(*this);
}

//_____________________________________________________________________________
PhPhotonListv2*
PhPhotonListv2::create(void) const
{
  return new PhPhotonListv2;
}

//_____________________________________________________________________________
void
PhPhotonListv2::copy(PhPhotonListv2& dest) const
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
      PhPhotonSnglv2* clus = dest.addCluster(idest++);
      *clus = *(getCluster(i));
    }
}

//_____________________________________________________________________________
PhPhotonSnglv2*
PhPhotonListv2::findCluster(int clusterid) const
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
PhPhotonListv2::getCluster(unsigned int i) const
{
  return static_cast<PhPhotonSnglv2*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
void
PhPhotonListv2::identify(ostream& os) const
{
  cout << "PhPhotonListv2::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
PhPhotonListv2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
PhPhotonListv2::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
bool
PhPhotonListv2::removeCluster(unsigned int i)
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
PhPhotonListv2::resize(unsigned int newsize)
{
  if ( newsize < PhPhotonListv2::fgMaxSize )
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
PhPhotonListv2::size(void) const
{
  return fEmcClusters->GetLast()+1;
}

