#ifndef __emcClusterContainerT_h__
#define __emcClusterContainerT_h__

#include "emcClusterContent.h"
#include "emcClusterContainer.h"
#include <cassert>

class TClonesArray;

/** (TEMPLATE) Implementation of emcClusterContainer based on TClonesArray.
@author Laurent Aphecetche
 */

template<class T>
class emcClusterContainerT : public emcClusterContainer
{
public:

  emcClusterContainerT();
  emcClusterContainerT(unsigned int defaultsize);
  emcClusterContainerT(const emcClusterContainerT&);
  emcClusterContainerT& operator=(const emcClusterContainerT&);

  unsigned int capacity(void) const;

  emcClusterContainerT* clone(void) const;
  emcClusterContainerT* create(void) const;

  virtual ~emcClusterContainerT();

  T* addCluster(unsigned int i);
  
  T* addCluster(unsigned int i, const emcClusterContent& c);
  
  T* findCluster(int clusterid) const;

  T* getCluster(unsigned int i) const;

  void identify(std::ostream& os=std::cout) const;

  int isValid() const;

  bool removeCluster(unsigned int i) ;

  void Reset();

  bool resize(unsigned int newsize);

  unsigned int size(void) const;

protected:

  TClonesArray* fEmcClusters;

private:
  void allocate(unsigned int thesize);
  void copy(emcClusterContainerT<T>& dest) const;
  void clear(TClonesArray& array);
  bool expand(unsigned int);
  bool expand_for(unsigned int);

  static const unsigned int fgDefaultSize;
  static const unsigned int fgMaxSize;

  ClassDef(emcClusterContainerT,1) // Template of an array of emcClusterContent
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include "phool.h"
#include "TClass.h"
#include <iostream>
#include <algorithm>
#include "TClonesArray.h"

template<class T>
const unsigned int emcClusterContainerT<T>::fgDefaultSize = 1000;

template<class T>
const unsigned int emcClusterContainerT<T>::fgMaxSize = 24768;

//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>::emcClusterContainerT() : fEmcClusters(0)
{
  allocate(emcClusterContainerT<T>::fgDefaultSize);
}

//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>::emcClusterContainerT(unsigned int defaultsize) 
  : fEmcClusters(0)
{
  allocate(defaultsize);
}
//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>::emcClusterContainerT(const emcClusterContainerT<T>& o) 
  : fEmcClusters(0)
{
  o.copy(*this);
}

//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>&
emcClusterContainerT<T>::operator=(const emcClusterContainerT<T>& o)
{
  if ( this != &o )
    {
      Reset();
      o.copy(*this);
    }
  return *this;
}

//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>::~emcClusterContainerT()
{
  clear(*fEmcClusters);
  delete fEmcClusters;
}

//_____________________________________________________________________________
template<class T>
T*
emcClusterContainerT<T>::addCluster(unsigned int i)
{
  if ( i > capacity() )
    {
      bool ok = expand_for(i);
      if (!ok) 
	{
	  std::cerr << PHWHERE << " object is full ?!" << std::endl;
	  return 0;
	}
    }

  return new((*fEmcClusters)[i]) T;
}

//_____________________________________________________________________________
template<class T>
T*
emcClusterContainerT<T>::addCluster(unsigned int i, const emcClusterContent& c)
{
  const T* test = 
    dynamic_cast<const T*>(&c);

  if (!test)
    {
      std::cerr << PHWHERE << " emcClusterContent is not of type "
		<< T::Class()->GetName() << std::endl;
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

  return new((*fEmcClusters)[i]) T(*test);
}

//_____________________________________________________________________________
template<class T>
void
emcClusterContainerT<T>::allocate(unsigned int thesize)
{
  if (fEmcClusters)
    {
      clear(*fEmcClusters);
      delete fEmcClusters;
    }
  fEmcClusters = new TClonesArray(T::Class()->GetName(),thesize);
}

//_____________________________________________________________________________
template<class T>
unsigned int
emcClusterContainerT<T>::capacity(void) const
{ 
  return fEmcClusters->GetSize(); 
}


//_____________________________________________________________________________
template<class T>
void emcClusterContainerT<T>::clear(TClonesArray& array)
{
#if ROOT_VERSION_CODE > ROOT_VERSION(3,01,5)
    // for recent version, it's taken care of by ROOT itself. Easy.
    array.Clear("C");
#else
#  pragma error "Code not valid for this old version of root"
#endif
}

//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>*
emcClusterContainerT<T>::clone(void) const
{
  return new emcClusterContainerT<T>(*this);
}

//_____________________________________________________________________________
template<class T>
emcClusterContainerT<T>*
emcClusterContainerT<T>::create(void) const
{
  return new emcClusterContainerT<T>(*this);
}

//_____________________________________________________________________________
template<class T>
void
emcClusterContainerT<T>::copy(emcClusterContainerT<T>& dest) const
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
      T* cluster = dest.addCluster(idest++);
      *cluster = *(getCluster(i));
    }
}

//_____________________________________________________________________________
template<class T>
bool
emcClusterContainerT<T>::expand(unsigned int newcap)
{
  if ( newcap <= fgMaxSize ) 
    {
      fEmcClusters->Expand(newcap);
      return true;
    }
  else
    {
      std::cerr << "emcClusterContainerT<" << T::Class()->GetName()
		<< ">::expand : "
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
emcClusterContainerT<T>::expand_for(unsigned int index)
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
emcClusterContainerT<T>::findCluster(int clusterid) const
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
template<class T>
T*
emcClusterContainerT<T>::getCluster(unsigned int i) const
{
  return static_cast<T*>(fEmcClusters->At(i));
}

//_____________________________________________________________________________
template<class T>
void
emcClusterContainerT<T>::identify(std::ostream& os) const
{
  std::cout << "emcClusterContainerT<" << T::Class()->GetName()
	    << ">::identify : size=" << size() 
	    << " capacity=" << capacity() << std::endl;
}

//_____________________________________________________________________________
template<class T>
int
emcClusterContainerT<T>::isValid() const
{
  return 1;
}

//_____________________________________________________________________________
template<class T>
void
emcClusterContainerT<T>::Reset()
{
  clear(*fEmcClusters);
}

//_____________________________________________________________________________
template<class T>
bool
emcClusterContainerT<T>::removeCluster(unsigned int i)
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
template<class T>
bool
emcClusterContainerT<T>::resize(unsigned int newsize)
{
  Reset();
  if ( newsize < size() )
    {
      fEmcClusters->Expand(newsize);
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
emcClusterContainerT<T>::size(void) const
{
  return fEmcClusters->GetLast()+1;
}

#endif
