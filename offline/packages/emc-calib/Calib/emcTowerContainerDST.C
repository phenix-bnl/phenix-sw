#include "emcTowerContainerDST.h"
#include "emcTowerContentDST.h"
#include "TClonesArray.h"
#include <cassert>
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcTowerContainerDST);

using namespace std;

const unsigned int emcTowerContainerDST::fgMaxSize = 24768;
const unsigned int emcTowerContainerDST::fgDefaultSize = 1000;

//_____________________________________________________________________________
  emcTowerContainerDST::emcTowerContainerDST() 
    : fEmcTowers(0), 
      fTowerIdToIndexIsUpToDate(false)
{
  //  cout << __FILE__ << "  " << __LINE__ << "  emcTowerContainerDST" << endl;
  allocate(emcTowerContainerDST::fgDefaultSize);
  vertex[0] = vertex[1] = vertex[2] = 0;
}

//_____________________________________________________________________________
emcTowerContainerDST::emcTowerContainerDST(const emcTowerContainerDST& o) 
  : emcTowerContainer(),
    fEmcTowers(0),
    fTowerIdToIndexIsUpToDate(false)
{
  //  cout << __FILE__ << "  " << __LINE__ << "  emcTowerContainerDST" << endl;
  o.copy(*this);
}

//_____________________________________________________________________________
emcTowerContainerDST&
emcTowerContainerDST::operator=(const emcTowerContainerDST& o)
{
  //  cout << __FILE__ << "  " << __LINE__ << "  emcTowerContainerDST" << endl;
  if ( this != &o )
    {
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContainerDST::~emcTowerContainerDST()
{
  delete fEmcTowers;
}

//_____________________________________________________________________________
emcTowerContentDST*
emcTowerContainerDST::addTower(unsigned int i)
{
  if ( i >= capacity() )
    {
      bool ok = expand_for(i);
      if (!ok) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }

  fTowerIdToIndexIsUpToDate = false;
  //  cout << __FILE__ << "  " << __LINE__ << "  emcTowerContainerDST" << endl;

  return new((*fEmcTowers)[i]) emcTowerContentDST;
}

//_____________________________________________________________________________
emcTowerContentDST*
emcTowerContainerDST::addTower(unsigned int i, const emcTowerContent& t)
{
  const emcTowerContentDST* test = 
    dynamic_cast<const emcTowerContentDST*>(&t);

  if (!test) 
    {
      cerr << PHWHERE << " emcTowerContent is not of type DST" << endl;
      return 0;
    }

  if ( i >= capacity() )
    {
      bool ok = expand_for(i);
      if (!ok) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }

  fTowerIdToIndexIsUpToDate = false;
  //  cout << __FILE__ << "  " << __LINE__ << "  emcTowerContainerDST" << endl;
  //test->print();

  return new((*fEmcTowers)[i]) emcTowerContentDST(*test);
}

//_____________________________________________________________________________
void
emcTowerContainerDST::allocate(unsigned int thesize)
{
  delete fEmcTowers;
  fEmcTowers = new TClonesArray("emcTowerContentDST",thesize);
  fTowerIdToIndexIsUpToDate = false;
}

//_____________________________________________________________________________
unsigned int
emcTowerContainerDST::capacity(void) const
{ 
  return fEmcTowers->GetSize(); 
}

//_____________________________________________________________________________
emcTowerContainerDST*
emcTowerContainerDST::clone(void) const
{
  return new emcTowerContainerDST(*this);
}

//_____________________________________________________________________________
emcTowerContainerDST*
emcTowerContainerDST::create(void) const
{
  return new emcTowerContainerDST;
}

//_____________________________________________________________________________
void
emcTowerContainerDST::copy(emcTowerContainerDST& dest) const
{
  if ( !dest.fEmcTowers ) 
    {
      dest.allocate(fEmcTowers->GetSize());
    }
  else
    {
      dest.Reset();
    }

  unsigned int idest = 0;
  for (unsigned int i = 0; i < size(); ++i)
    {
      emcTowerContentDST* tower = dest.addTower(idest++);
      *tower = *(getTower(i));
    }

  dest.fTowerIdToIndexIsUpToDate = fTowerIdToIndexIsUpToDate;
  dest.fTowerIdToIndex = fTowerIdToIndex;
  for (int i = 0; i < 3; i++)
    {
      dest.setVertex(i, getVertex(i) );
    }

}

//_____________________________________________________________________________
bool
emcTowerContainerDST::expand(unsigned int newcap)
{
  if ( newcap <= fgMaxSize ) 
    {
      fEmcTowers->Expand(newcap);
      return true;
    }
  else
    {
      std::cerr << "emcTowerContainerDST::expand : "
		<< " attemting to go above max capacity of "
		<< fgMaxSize << ". That's a *big* failure "
		<< "probably"
		<< std::endl;
      return false;
    }
}

//_____________________________________________________________________________
bool
emcTowerContainerDST::expand_for(unsigned int index)
{
  unsigned int capa = capacity();

  while ( index >= capa && capa < fgMaxSize ) 
    {
      capa = std::min(capa*2,fgMaxSize);
    }
  if ( index >= capa )
    {
      return false;
    }
  else
    {
      assert(capa>capacity());
      return expand(capa);
    }
}

//_____________________________________________________________________________
emcTowerContentDST*
emcTowerContainerDST::findTower(int towerID) const
{
  if ( !fTowerIdToIndexIsUpToDate )
    {
      fTowerIdToIndex.clear();
      for ( size_t i = 0; i < size(); ++i ) 
	{
	  fTowerIdToIndex[getTower(i)->TowerID()] = i;
	}
      fTowerIdToIndexIsUpToDate=true;
    }

  map<int,int>::const_iterator it = fTowerIdToIndex.find(towerID);
  if ( it != fTowerIdToIndex.end() )
    {
      return getTower(it->second);
    }
  else
    {
      return 0;
    }
}

//_____________________________________________________________________________
emcTowerContentDST*
emcTowerContainerDST::getTower(unsigned int i) const
{
  return static_cast<emcTowerContentDST*>(fEmcTowers->At(i));
}

//_____________________________________________________________________________
void
emcTowerContainerDST::identify(ostream& os) const
{
  os << "emcTowerContainerDST::identify : size=" << size() << std::endl;
}

//_____________________________________________________________________________
int
emcTowerContainerDST::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcTowerContainerDST::Reset()
{
  fEmcTowers->Clear();
  fTowerIdToIndex.clear();
  fTowerIdToIndexIsUpToDate=false;
}

//_____________________________________________________________________________
bool
emcTowerContainerDST::removeTower(unsigned int i)
{
  if ( i < size() ) 
    {
      fEmcTowers->RemoveAt(i);
      fEmcTowers->Compress();
      fTowerIdToIndexIsUpToDate=false;
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcTowerContainerDST::resize(unsigned int newsize)
{
  if ( newsize < size() )
    {
      fEmcTowers->Expand(newsize);
      return true;
    }
  else
    {
      return expand(newsize);
    }
}

//_____________________________________________________________________________
unsigned int
emcTowerContainerDST::size(void) const
{
  return fEmcTowers->GetLast()+1;
}
