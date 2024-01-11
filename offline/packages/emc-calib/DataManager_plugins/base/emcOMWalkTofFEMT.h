#ifndef __EMCOMWALKTOFFEM_H__
#define __EMCOMWALKTOFFEM_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

/**
   (Template) Plugin class for emcWalkTofFEM objects.
   @ingroup dmplugins
*/

template <class T>
class emcOMWalkTofFEMT : public emcOMCalFEMT<T>
{

public:
  emcOMWalkTofFEMT(const char* name, const char* title)
    : emcOMCalFEMT<T>(name, title)
  { }

  virtual ~emcOMWalkTofFEMT()
  { }

  /** We announce that we can read {\tt emcGains} object. */
  virtual bool CanRead(const emcManageable& object) const;

  /** We announce that we can write {\tt emcGains} object. */
  virtual bool CanWrite(const emcManageable& object) const;

  /// Fill an emcCalFEM object from a PdbCalBank
  virtual void FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank);

  /// Get the name of the Objy classname we are dealing with.
  virtual std::string GetPersistentClassName(void) const
  {
    return "PdbEmcWalkTofBank";
  }

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank);

};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#ifndef __EMCWALKTOFFEM_H__
#include "emcWalkTofFEM.h"
#endif
#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif
#include "PdbCalBank.hh"
#include "PdbEmcWalkTof.hh"

#include <cassert>
#include <iostream>
#include <fstream>
#include <cstdio>

//_____________________________________________________________________________
template <class T>
bool
emcOMWalkTofFEMT<T>::CanRead(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcWalkTofFEM only.

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcWalkTofFEM* test = dynamic_cast<const emcWalkTofFEM*>(object_ptr);

  if (test)
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
template <class T>
bool
emcOMWalkTofFEMT<T>::CanWrite(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcWalkTofFEM only.

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcWalkTofFEM* test = dynamic_cast<const emcWalkTofFEM*>(object_ptr);

  if (test)
    {
      return true;
    }
  return false;
}


//_____________________________________________________________________________
template<class T>
void
emcOMWalkTofFEMT<T>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& emcBank)
{
  size_t thesize = emcBank.getLength();
  assert (thesize == 144);
  size_t i;
  float value1;
  float value2;

  PdbEmcWalkTof* pdblct;
  emcWalkTofFEM* tofFEM = dynamic_cast<emcWalkTofFEM*>(&calfem);
  tofFEM->Reset();


  for ( i = 0; i < thesize; i++)
    {
      pdblct = dynamic_cast<PdbEmcWalkTof*>(&(emcBank.getEntry(i)));
      assert(pdblct != 0);
      pdblct->GetWalkTofs(value1, value2);
      tofFEM->AppendOneChannel(value1, value2);
    }
}

//_____________________________________________________________________________
template<class T>
void
emcOMWalkTofFEMT<T>::ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& emcBank)
{
  PdbEmcWalkTof* pdblct;

  int i;
  size_t thesize = calfem.GetNumberOfChannels();
  assert(thesize == 144);

  emcBank.setLength(thesize);

  const emcWalkTofFEM* tofFEM = dynamic_cast<const emcWalkTofFEM*>(&calfem);
  assert(tofFEM != 0);
  assert(tofFEM->GetNumberOfChannels() == 144);

  for ( i = 0; i < 144; i++)
    {
      pdblct = (PdbEmcWalkTof*) & (emcBank.getEntry(i));
      assert(pdblct != 0);
      pdblct->SetWalkTofs(tofFEM->getValue(i, 0), tofFEM->getValue(i, 1));
    }
}

#endif
