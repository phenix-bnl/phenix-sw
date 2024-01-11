#ifndef __EMCOMLCTOFFEMT_H__
#define __EMCOMLCTOFFEMT_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

/**
   (Template) plugin class for emcLCTofFEM objects.
   @ingroup dmplugins
*/

template <class T>
class emcOMLCTofFEMT : public emcOMCalFEMT<T>
{

public:
  emcOMLCTofFEMT(const char* name, const char* title)
    : emcOMCalFEMT<T>(name, title)
  { }

  virtual ~emcOMLCTofFEMT()
  { }

  /** We announce that we can read {\tt emcLCTofFEM} object. */
  virtual bool CanRead(const emcManageable& object) const;

  /** We announce that we can write {\tt emcLCTofFEM} object. */
  virtual bool CanWrite(const emcManageable& object) const;

  /// Fill an emcCalFEM object from a PdbCalBank
  virtual void FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank);

  /// Get the name of the Objy classname we are dealing with.
  virtual std::string GetPersistentClassName(void) const
  {
    return "PdbEmcLCTofBank";
  }

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank);

};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#ifndef __EMCLCTOFFEM_H__
#include "emcLCTofFEM.h"
#endif
#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif

#include "PdbCalBank.hh"
#include "PdbEmcLCTof.hh"

#include <cassert>
#include <iostream>
#include <fstream>
#include <cstdio>

//_____________________________________________________________________________
template <class T>
bool
emcOMLCTofFEMT<T>::CanRead(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcLCTofFEM only.

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  bool rv = false;
  const emcManageable* object_ptr = &object;
  const emcLCTofFEM* test = dynamic_cast<const emcLCTofFEM*>(object_ptr);

  if (test)
    {
      rv = true;
    }
  return rv;
}

//_____________________________________________________________________________
template <class T>
bool
emcOMLCTofFEMT<T>::CanWrite(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcLCTofFEM only.

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  bool rv = false;
  const emcManageable* object_ptr = &object;
  const emcLCTofFEM* test = dynamic_cast<const emcLCTofFEM*>(object_ptr);

  if (test)
    {
      rv = true;
    }
  return rv;
}

//_____________________________________________________________________________
template <class T>
void
emcOMLCTofFEMT<T>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& emcBank)
{
  size_t thesize = emcBank.getLength();
  assert (thesize == 144);
  size_t i;
  float value1;
  float value2;

  PdbEmcLCTof* pdblct;
  emcLCTofFEM* tofFEM = dynamic_cast<emcLCTofFEM*>(&calfem);
  tofFEM->Reset();

  for ( i = 0; i < thesize; i++)
    {
      pdblct = dynamic_cast<PdbEmcLCTof*>(&(emcBank.getEntry(i)));
      assert(pdblct != 0);
      pdblct->GetLCTofs(value1, value2);
      tofFEM->AppendOneChannel(value1, value2);
    }
}

//_____________________________________________________________________________
template<class T>
void
emcOMLCTofFEMT<T>::ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& emcBank)
{
  PdbEmcLCTof* pdblct;

  int i;
  size_t thesize = calfem.GetNumberOfChannels();
  assert(thesize == 144);

  emcBank.setLength(thesize);

  const emcLCTofFEM* tofFEM = dynamic_cast<const emcLCTofFEM*>(&calfem);
  assert(tofFEM != 0);
  assert(tofFEM->GetNumberOfChannels() == 144);

  for ( i = 0; i < 144; i++)
    {
      pdblct = (PdbEmcLCTof*) & (emcBank.getEntry(i));
      assert(pdblct != 0);
      pdblct->SetLCTofs(tofFEM->getValue(i, 0), tofFEM->getValue(i, 1));
    }
}

#endif
