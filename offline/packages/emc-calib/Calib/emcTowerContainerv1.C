#include "emcTowerContainerv1.h"
#include "emcTowerContentv1.h"
#include "TClonesArray.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

ClassImp(emcTowerContainerv1)

  using namespace std;

const unsigned int emcTowerContainerv1::fgMaxSize = 24768;
const unsigned int emcTowerContainerv1::fgDefaultSize = 1000;

//_____________________________________________________________________________
emcTowerContainerv1::emcTowerContainerv1() : fEmcTowers(0)
{
  allocate(emcTowerContainerv1::fgDefaultSize);
}

//_____________________________________________________________________________
emcTowerContainerv1::emcTowerContainerv1(const emcTowerContainerv1& o) 
  : emcTowerContainer(), fEmcTowers(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
emcTowerContainerv1&
emcTowerContainerv1::operator=(const emcTowerContainerv1& o)
{
  if ( this != &o )
    {
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContainerv1::~emcTowerContainerv1()
{
  delete fEmcTowers;
}

//_____________________________________________________________________________
emcTowerContentv1*
emcTowerContainerv1::addTower(unsigned int i)
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

  return new((*fEmcTowers)[i]) emcTowerContentv1;
}

//_____________________________________________________________________________
emcTowerContentv1*
emcTowerContainerv1::addTower(unsigned int i, const emcTowerContent& t)
{
  const emcTowerContentv1* test = 
    dynamic_cast<const emcTowerContentv1*>(&t);

  if (!test) 
    {
      cerr << PHWHERE << " emcTowerContent is not of type v1" << endl;
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

  return new((*fEmcTowers)[i]) emcTowerContentv1(*test);
}

//_____________________________________________________________________________
void
emcTowerContainerv1::allocate(unsigned int thesize)
{
  delete fEmcTowers;
  fEmcTowers = new TClonesArray("emcTowerContentv1",thesize);
}

//_____________________________________________________________________________
unsigned int
emcTowerContainerv1::capacity(void) const
{ 
  return fEmcTowers->GetSize(); 
}

//_____________________________________________________________________________
emcTowerContainerv1*
emcTowerContainerv1::clone(void) const
{
  return new emcTowerContainerv1(*this);
}

//_____________________________________________________________________________
emcTowerContainerv1*
emcTowerContainerv1::create(void) const
{
  return new emcTowerContainerv1;
}

//_____________________________________________________________________________
void
emcTowerContainerv1::copy(emcTowerContainerv1& dest) const
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
      emcTowerContentv1* tower = dest.addTower(idest++);
      *tower = *(getTower(i));
    }
}

//_____________________________________________________________________________
emcTowerContentv1*
emcTowerContainerv1::findTower(int towerID) const
{
  for ( size_t i = 0; i < size(); ++i ) 
    {
      if ( getTower(i)->TowerID() == towerID ) 
	{
	  return getTower(i);
	}
    }
  return 0;
}

//_____________________________________________________________________________
emcTowerContentv1*
emcTowerContainerv1::getTower(unsigned int i) const
{
  return static_cast<emcTowerContentv1*>(fEmcTowers->At(i));
}

//_____________________________________________________________________________
void
emcTowerContainerv1::identify(ostream& os) const
{
  os << "emcTowerContainerv1::identify : size=" << size() << std::endl;
}

//_____________________________________________________________________________
int
emcTowerContainerv1::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
void
emcTowerContainerv1::Reset()
{
  fEmcTowers->Clear();
}

//_____________________________________________________________________________
bool
emcTowerContainerv1::removeTower(unsigned int i)
{
  if ( i < size() ) 
    {
      fEmcTowers->RemoveAt(i);
      fEmcTowers->Compress();
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
bool
emcTowerContainerv1::resize(unsigned int newsize)
{
  if ( newsize < emcTowerContainerv1::fgMaxSize )
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
emcTowerContainerv1::size(void) const
{
  return fEmcTowers->GetLast()+1;
}
