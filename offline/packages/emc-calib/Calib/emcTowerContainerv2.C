#include "emcTowerContainerv2.h"
#include "emcTowerContentv2.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcTowerContainerv2)

  using namespace std;

const unsigned int emcTowerContainerv2::fgMaxSize = 24768;
const unsigned int emcTowerContainerv2::fgDefaultSize = 1000;

//_____________________________________________________________________________
  emcTowerContainerv2::emcTowerContainerv2() 
    : fEmcTowers(0), 
      fTowerIdToIndexIsUpToDate(false)
{
  allocate(emcTowerContainerv2::fgDefaultSize);
}

//_____________________________________________________________________________
emcTowerContainerv2::emcTowerContainerv2(const emcTowerContainerv2& o) 
  : emcTowerContainer(),
    fEmcTowers(0),
    fTowerIdToIndexIsUpToDate(false)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcTowerContainerv2&
emcTowerContainerv2::operator=(const emcTowerContainerv2& o)
{
  if ( this != &o )
    {
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContainerv2::~emcTowerContainerv2()
{
  delete fEmcTowers;
}

//_____________________________________________________________________________
emcTowerContentv2*
emcTowerContainerv2::addTower(unsigned int i)
{
  if ( static_cast<int>(i) >  fEmcTowers->GetSize() )
    {
      bool ok = resize(std::max(capacity()*2,fgMaxSize));
      if (!ok) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }

  fTowerIdToIndexIsUpToDate = false;

  return new((*fEmcTowers)[i]) emcTowerContentv2;
}

//_____________________________________________________________________________
emcTowerContentv2*
emcTowerContainerv2::addTower(unsigned int i, const emcTowerContent& t)
{
  const emcTowerContentv2* test = 
    dynamic_cast<const emcTowerContentv2*>(&t);

  if (!test) 
    {
      cerr << PHWHERE << " emcTowerContent is not of type v2" << endl;
      return 0;
    }

  if ( static_cast<int>(i) >  fEmcTowers->GetSize() )
    {
      bool ok = resize(std::max(capacity()*2,fgMaxSize));
      if (!ok) 
	{
	  cerr << PHWHERE << " object is full ?!" << endl;
	  return 0;
	}
    }

  fTowerIdToIndexIsUpToDate = false;

  return new((*fEmcTowers)[i]) emcTowerContentv2(*test);
}

//_____________________________________________________________________________
void
emcTowerContainerv2::allocate(unsigned int thesize)
{
  delete fEmcTowers;
  fEmcTowers = new TClonesArray("emcTowerContentv2",thesize);
  fTowerIdToIndexIsUpToDate = false;
}

//_____________________________________________________________________________
unsigned int
emcTowerContainerv2::capacity(void) const
{ 
  return fEmcTowers->GetSize(); 
}

//_____________________________________________________________________________
emcTowerContainerv2*
emcTowerContainerv2::clone(void) const
{
  return new emcTowerContainerv2(*this);
}

//_____________________________________________________________________________
emcTowerContainerv2*
emcTowerContainerv2::create(void) const
{
  return new emcTowerContainerv2;
}

//_____________________________________________________________________________
void
emcTowerContainerv2::copy(emcTowerContainerv2& dest) const
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
      emcTowerContentv2* tower = dest.addTower(idest++);
      *tower = *(getTower(i));
    }

  dest.fTowerIdToIndexIsUpToDate = fTowerIdToIndexIsUpToDate;
  dest.fTowerIdToIndex = fTowerIdToIndex;
}

//_____________________________________________________________________________
emcTowerContentv2*
emcTowerContainerv2::findTower(int towerID) const
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
emcTowerContentv2*
emcTowerContainerv2::getTower(unsigned int i) const
{
  return static_cast<emcTowerContentv2*>(fEmcTowers->At(i));
}

//_____________________________________________________________________________
void
emcTowerContainerv2::identify(ostream& os) const
{
  os << "emcTowerContainerv2::identify : size=" << size() << std::endl;
}

//_____________________________________________________________________________
int
emcTowerContainerv2::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcTowerContainerv2::Reset()
{
  fEmcTowers->Clear();
  fTowerIdToIndex.clear();
  fTowerIdToIndexIsUpToDate=false;
}

//_____________________________________________________________________________
bool
emcTowerContainerv2::removeTower(unsigned int i)
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
emcTowerContainerv2::resize(unsigned int newsize)
{
  if ( newsize < emcTowerContainerv2::fgMaxSize )
    {
      fEmcTowers->Expand(newsize);
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
unsigned int
emcTowerContainerv2::size(void) const
{
  return fEmcTowers->GetLast()+1;
}
