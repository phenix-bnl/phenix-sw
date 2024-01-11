#include <mpcClusterContainerV2.h>
#include <mpcClusterContentV2.h>
#include <TClonesArray.h>
#include <iostream>
#include <algorithm>
#include <phool.h>

ClassImp(mpcClusterContainerV2);

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

const unsigned int mpcClusterContainerV2::fgDefaultSize = 1000;
const unsigned int mpcClusterContainerV2::fgMaxSize = 24768;

//_____________________________________________________________________________
mpcClusterContainerV2::mpcClusterContainerV2() : fMpcClusters(0)
{
  allocate(mpcClusterContainerV2::fgDefaultSize);
}

//_____________________________________________________________________________
mpcClusterContainerV2::mpcClusterContainerV2(const mpcClusterContainerV2& o) : fMpcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
mpcClusterContainerV2&
mpcClusterContainerV2::operator=(const mpcClusterContainerV2& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
mpcClusterContainerV2::~mpcClusterContainerV2()
{
  //clear(*fMpcClusters);
  fMpcClusters->Delete();
  delete fMpcClusters;
}

//_____________________________________________________________________________
mpcClusterContentV2*
mpcClusterContainerV2::addCluster(unsigned int i)
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
    
  mpcClusterContentV2* rv = new((*fMpcClusters)[i]) mpcClusterContentV2;
  return rv;
}

//_____________________________________________________________________________
mpcClusterContentV2*
mpcClusterContainerV2::addCluster(unsigned int i, const mpcClusterContent& c)
{
  const mpcClusterContentV2* test = 
    dynamic_cast<const mpcClusterContentV2*>(&c);

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
    
  return new((*fMpcClusters)[i]) mpcClusterContentV2(*test);
}

//_____________________________________________________________________________
void
mpcClusterContainerV2::allocate(unsigned int thesize)
{
  if (fMpcClusters)
    {
      clear(*fMpcClusters);
      delete fMpcClusters;
    }
  fMpcClusters = new TClonesArray("mpcClusterContentV2",thesize);
}

//_____________________________________________________________________________
unsigned int
mpcClusterContainerV2::capacity(void) const
{ 
  return fMpcClusters->GetSize(); 
}

//_____________________________________________________________________________
mpcClusterContainerV2*
mpcClusterContainerV2::clone(void) const
{
  return new mpcClusterContainerV2(*this);
}

//_____________________________________________________________________________
mpcClusterContainerV2*
mpcClusterContainerV2::create(void) const
{
  return new mpcClusterContainerV2;
}

//_____________________________________________________________________________
void
mpcClusterContainerV2::copy(mpcClusterContainerV2& dest) const
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
      mpcClusterContentV2* clus = dest.addCluster(idest++);
// need to fix this coherently
      *clus = *static_cast<mpcClusterContentV2*>(getCluster(i));
    }
}

//_____________________________________________________________________________
mpcClusterContent*
mpcClusterContainerV2::findCluster(int clusterid) const
{
  for ( size_t i = 0; i < size(); ++i ) 
    {
      if ( getCluster(i)->id() == clusterid ) 
	{
	  return static_cast<mpcClusterContentV2*>(getCluster(i));
	}
    }
  return 0;
}

//_____________________________________________________________________________
mpcClusterContent*
mpcClusterContainerV2::getCluster(unsigned int i) const
{
  return static_cast<mpcClusterContentV2*>(fMpcClusters->At(i));
}

//_____________________________________________________________________________
void
mpcClusterContainerV2::identify(ostream& os) const
{
  cout << "mpcClusterContainerV2::identify : size=" << size() << endl;
}

//_____________________________________________________________________________
int
mpcClusterContainerV2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
mpcClusterContainerV2::Reset()
{
  clear(*fMpcClusters);
}

//_____________________________________________________________________________
bool
mpcClusterContainerV2::removeCluster(unsigned int i)
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
mpcClusterContainerV2::resize(unsigned int newsize)
{
  if ( newsize < mpcClusterContainerV2::fgMaxSize )
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
mpcClusterContainerV2::size(void) const
{
  return fMpcClusters->GetLast()+1;
}

