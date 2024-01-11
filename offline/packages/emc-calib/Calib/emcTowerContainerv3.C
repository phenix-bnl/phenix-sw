#include "emcTowerContainerv3.h"
#include "emcTowerContentv3.h"
#include "TClonesArray.h"
#include <cassert>
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcTowerContainerv3);

using namespace std;

const unsigned int emcTowerContainerv3::fgMaxSize = 24768;
const unsigned int emcTowerContainerv3::fgDefaultSize = 1000;

//_____________________________________________________________________________
  emcTowerContainerv3::emcTowerContainerv3() 
    : fEmcTowers(0), 
      fTowerIdToIndexIsUpToDate(false)
{
  allocate(emcTowerContainerv3::fgDefaultSize);
}

//_____________________________________________________________________________
emcTowerContainerv3::emcTowerContainerv3(const emcTowerContainerv3& o) 
  : emcTowerContainer(),
    fEmcTowers(0),
    fTowerIdToIndexIsUpToDate(false)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcTowerContainerv3&
emcTowerContainerv3::operator=(const emcTowerContainerv3& o)
{
  if ( this != &o )
    {
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContainerv3::~emcTowerContainerv3()
{
  delete fEmcTowers;
}

//_____________________________________________________________________________
emcTowerContentv3*
emcTowerContainerv3::addTower(unsigned int i)
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

  return new((*fEmcTowers)[i]) emcTowerContentv3;
}

//_____________________________________________________________________________
emcTowerContentv3*
emcTowerContainerv3::addTower(unsigned int i, const emcTowerContent& t)
{
  const emcTowerContentv3* test = 
    dynamic_cast<const emcTowerContentv3*>(&t);

  if (!test) 
    {
      cerr << PHWHERE << " emcTowerContent is not of type v3" << endl;
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

  return new((*fEmcTowers)[i]) emcTowerContentv3(*test);
}

//_____________________________________________________________________________
void
emcTowerContainerv3::allocate(unsigned int thesize)
{
  delete fEmcTowers;
  fEmcTowers = new TClonesArray("emcTowerContentv3",thesize);
  fTowerIdToIndexIsUpToDate = false;
}

//_____________________________________________________________________________
unsigned int
emcTowerContainerv3::capacity(void) const
{ 
  return fEmcTowers->GetSize(); 
}

//_____________________________________________________________________________
emcTowerContainerv3*
emcTowerContainerv3::clone(void) const
{
  return new emcTowerContainerv3(*this);
}

//_____________________________________________________________________________
emcTowerContainerv3*
emcTowerContainerv3::create(void) const
{
  return new emcTowerContainerv3;
}

//_____________________________________________________________________________
void
emcTowerContainerv3::copy(emcTowerContainerv3& dest) const
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

  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      emcTowerContentv3* tower = dest.addTower(idest++);
      *tower = *(getTower(i));
    }

  dest.fTowerIdToIndexIsUpToDate = fTowerIdToIndexIsUpToDate;
  dest.fTowerIdToIndex = fTowerIdToIndex;
}

//_____________________________________________________________________________
bool
emcTowerContainerv3::expand(unsigned int newcap)
{
  if ( newcap <= fgMaxSize ) 
    {
      fEmcTowers->Expand(newcap);
      return true;
    }
  else
    {
      std::cerr << "emcTowerContainerv3::expand : "
		<< " attemting to go above max capacity of "
		<< fgMaxSize << ". That's a *big* failure "
		<< "probably"
		<< std::endl;
      return false;
    }
}

//_____________________________________________________________________________
bool
emcTowerContainerv3::expand_for(unsigned int index)
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
emcTowerContentv3*
emcTowerContainerv3::findTower(int towerID) const
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
emcTowerContentv3*
emcTowerContainerv3::getTower(unsigned int i) const
{
  return static_cast<emcTowerContentv3*>(fEmcTowers->At(i));
}

//_____________________________________________________________________________
void
emcTowerContainerv3::identify(ostream& os) const
{
  os << "emcTowerContainerv3::identify : size=" << size() << std::endl;
}

//_____________________________________________________________________________
int
emcTowerContainerv3::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcTowerContainerv3::Reset()
{
  fEmcTowers->Clear();
  fTowerIdToIndex.clear();
  fTowerIdToIndexIsUpToDate=false;
}

//_____________________________________________________________________________
bool
emcTowerContainerv3::removeTower(unsigned int i)
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
emcTowerContainerv3::resize(unsigned int newsize)
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
emcTowerContainerv3::size(void) const
{
  return fEmcTowers->GetLast()+1;
}
