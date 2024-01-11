#ifndef __EMCTOWERCONTAINERT_H__
#define __EMCTOWERCONTAINERT_H__

#ifndef __EMCTOWERCONTAINER_H__
#include "emcTowerContainer.h"
#endif
#include <map>

class TClonesArray;

/** (Template) Implementation of emcTowerContainer based on TClonesArray.

Using this class creating a new container (which is usually driven by the need to have a new version of the \e content class emcTowerContent) is very easy. Assuming you want to define version 123 (!) make a new emcTowerContainerv123.h file containing the following lines, defining a typedef using the template :

\code
#ifndef __EMCTOWERCONTAINERV123_H__
#define __EMCTOWERCONTAINERV123_H__

#ifndef __EMCTOWERCONTAINERT_H__
#include "emcTowerContainerT.h"
#endif
#ifndef __EMCTOWERCONTENTV123_H__
#include "emcTowerContentv123.h"
#endif

typedef emcTowerContainerT<emcTowerContentv123> emcTowerContainerv123;

#endif
\endcode

And that's it !

@ingroup calibration
@author Laurent Aphecetche

*/

template<class T>
class emcTowerContainerT : public emcTowerContainer
{
public:

  emcTowerContainerT();

  emcTowerContainerT(unsigned int defaultsize);

  emcTowerContainerT(const emcTowerContainerT&);

  emcTowerContainerT& operator=(const emcTowerContainerT&);

  emcTowerContainerT* clone(void) const;

  emcTowerContainerT* create(void) const;

  virtual ~emcTowerContainerT();

  unsigned int capacity(void) const;

  T* addTower(unsigned int i);

  T* addTower(unsigned int i, const emcTowerContent&);

  T* findTower(int towerID) const;

  T* getTower(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeTower(unsigned int i);

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;

protected:

  TClonesArray* fEmcTowers;

private:
  void allocate(unsigned int thesize);
  void copy(emcTowerContainerT& dest) const;
  bool expand(unsigned int);
  bool expand_for(unsigned int);

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  mutable std::map<int,int> fTowerIdToIndex; //!
  mutable bool fTowerIdToIndexIsUpToDate; //!

  ClassDef(emcTowerContainerT,1) // Array of T
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include "TClonesArray.h"
#include "TClass.h"
#include <iostream>
#include <algorithm>
#include "phool.h"

template<class T> 
const unsigned int emcTowerContainerT<T>::fgMaxSize = 24768;
template<class T> 
const unsigned int emcTowerContainerT<T>::fgDefaultSize = 1000;

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>::emcTowerContainerT() 
  : fEmcTowers(0), 
    fTowerIdToIndexIsUpToDate(false)
{
  allocate(emcTowerContainerT::fgDefaultSize);
}

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>::emcTowerContainerT(unsigned int defaultsize) 
  : emcTowerContainer(),
    fEmcTowers(0), 
    fTowerIdToIndexIsUpToDate(false)
{
  allocate(defaultsize);
}

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>::emcTowerContainerT(const emcTowerContainerT& o) 
  : emcTowerContainer(),
    fEmcTowers(0),
    fTowerIdToIndexIsUpToDate(false)
{
  o.copy(*this);
}

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>&
emcTowerContainerT<T>::operator=(const emcTowerContainerT& o)
{
  if ( this != &o )
    {
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>::~emcTowerContainerT()
{
  delete fEmcTowers;
}

//_____________________________________________________________________________
template<class T>
T*
emcTowerContainerT<T>::addTower(unsigned int i)
{
  if ( i >= capacity() )
    {
      bool ok = expand_for(i);
      if (!ok) 
	{
	  std::cerr << PHWHERE << " object is full ?!" << std::endl;
	  return 0;
	}
    }

  fTowerIdToIndexIsUpToDate = false;

  return new((*fEmcTowers)[i]) T;
}

//_____________________________________________________________________________
template<class T>
T*
emcTowerContainerT<T>::addTower(unsigned int i, const emcTowerContent& t)
{
  const T* test = 
    dynamic_cast<const T*>(&t);

  if (!test) 
    {
      std::cerr << PHWHERE << " emcTowerContent is not of type v3" << std::endl;
      return 0;
    }

  if ( i >= capacity() )
    {
      bool ok = expand_for(i);
      if (!ok) 
	{
	  std::cerr << PHWHERE << " object is full ?!" << std::endl;
	  return 0;
	}
    }

  fTowerIdToIndexIsUpToDate = false;

  return new((*fEmcTowers)[i]) T(*test);
}

//_____________________________________________________________________________
template<class T>
void
emcTowerContainerT<T>::allocate(unsigned int thesize)
{
  delete fEmcTowers;
  fEmcTowers = new TClonesArray(T::Class()->GetName(),thesize);
  fTowerIdToIndexIsUpToDate = false;
}

//_____________________________________________________________________________
template<class T>
unsigned int
emcTowerContainerT<T>::capacity(void) const
{ 
  return fEmcTowers->GetSize(); 
}

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>*
emcTowerContainerT<T>::clone(void) const
{
  return new emcTowerContainerT<T>(*this);
}

//_____________________________________________________________________________
template<class T>
emcTowerContainerT<T>*
emcTowerContainerT<T>::create(void) const
{
  return new emcTowerContainerT<T>;
}

//_____________________________________________________________________________
template<class T>
void
emcTowerContainerT<T>::copy(emcTowerContainerT& dest) const
{
  TClonesArray* destarray = dest.fEmcTowers;
  if ( !destarray ) 
    {
      dest.allocate(fEmcTowers->GetSize());
      destarray = dest.fEmcTowers;
    }
  else
    {
      dest.Reset();
    }

  unsigned int idest = 0;

  for ( unsigned int i = 0; i < size(); ++i ) 
    {
      T* tower = dest.addTower(idest++);
      *tower = *(getTower(i));
    }

  dest.fTowerIdToIndexIsUpToDate = fTowerIdToIndexIsUpToDate;
  dest.fTowerIdToIndex = fTowerIdToIndex;
}

//_____________________________________________________________________________
template<class T>
bool
emcTowerContainerT<T>::expand(unsigned int newcap)
{
  if ( newcap <= fgMaxSize ) 
    {
      fEmcTowers->Expand(newcap);
      return true;
    }
  else
    {
      std::cerr << "emcTowerContainerT<T>::expand : "
		<< " attemting to go above max capacity of "
		<< fgMaxSize << ". That's a *big* failure "
		<< "probably"
		<< std::endl;
      return false;
    }
}

//_____________________________________________________________________________
template<class T>
bool
emcTowerContainerT<T>::expand_for(unsigned int index)
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
template<class T>
T*
emcTowerContainerT<T>::findTower(int towerID) const
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

  std::map<int,int>::const_iterator it = fTowerIdToIndex.find(towerID);
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
template<class T>
T*
emcTowerContainerT<T>::getTower(unsigned int i) const
{
  return static_cast<T*>(fEmcTowers->At(i));
}

//_____________________________________________________________________________
template<class T>
void
emcTowerContainerT<T>::identify(std::ostream& os) const
{
  os << "emcTowerContainerT<"
     << T::Class()->GetName()
     << ">::identify : size=" << size()
     << " capacity=" << capacity() << std::endl;
}

//_____________________________________________________________________________
template<class T>
int
emcTowerContainerT<T>::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
template<class T>
void
emcTowerContainerT<T>::Reset()
{
  fEmcTowers->Clear();
  fTowerIdToIndex.clear();
  fTowerIdToIndexIsUpToDate=false;
}

//_____________________________________________________________________________
template<class T>
bool
emcTowerContainerT<T>::removeTower(unsigned int i)
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
template<class T>
bool
emcTowerContainerT<T>::resize(unsigned int newsize)
{
  Reset();
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
template<class T>
unsigned int
emcTowerContainerT<T>::size(void) const
{
  return fEmcTowers->GetLast()+1;
}

#endif
