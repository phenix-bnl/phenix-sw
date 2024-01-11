#ifndef __EMCOMQAFEMT_H__
#define __EMCOMQAFEMT_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

#include <string>
#include <map>
#include <vector>

/**
   (Template) Plugin class for emcQAFEM objects.
   @ingroup dmplugins
*/

template<class T>
class emcOMQAFEMT : public emcOMCalFEMT<T>
{

public:
  emcOMQAFEMT(const char* name, const char* title) :
    emcOMCalFEMT<T>(name, title)
  {}

  virtual ~emcOMQAFEMT()
  {}

  /** We announce that we can read {\tt emcQAFEM} object. */
  virtual bool CanRead(const emcManageable& object) const;

  /** We announce that we can write {\tt emcQAFEM} object. */
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

private:
  std::map<std::string, std::vector<int> > fCollectErrors;
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include "PdbCalBank.hh"
#include "PdbEmcLCTof.hh"

#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif
#ifndef __EMCQAFEM_H__
#include "emcQAFEM.h"
#endif

#include <cassert>
#include <ctime>
#include <string>
#include <fstream>

//_____________________________________________________________________________
template<class T>
bool
emcOMQAFEMT<T>::CanRead(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcQAFEM only.

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcQAFEM* test = dynamic_cast<const emcQAFEM*>(object_ptr);

  if (test)
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
template<class T>
bool
emcOMQAFEMT<T>::CanWrite(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcQAFEM only.

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcQAFEM* test = dynamic_cast<const emcQAFEM*>(object_ptr);

  if (test)
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
template<class T>
void
emcOMQAFEMT<T>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& emcBank)
{
  /* Fills calfem from the values in bank. */
  size_t thesize = emcBank.getLength();
  PdbEmcLCTof* pdbqa;
  emcQAFEM* qaFEM = dynamic_cast<emcQAFEM*>(&calfem);
  assert(qaFEM != 0);
  qaFEM->Reset();

  size_t i;
  for ( i = 0; i < thesize; i++)
    {
      pdbqa = dynamic_cast<PdbEmcLCTof*>(&(emcBank.getEntry(i)));
      INT32 error, warning;
      float value1, value2;
      pdbqa->GetLCTofs(value1, value2);
      error = static_cast<INT32>(value1);
      warning = static_cast<INT32>(value2);
      qaFEM->AppendOneChannel(i, error, warning);
    }
}

//_____________________________________________________________________________
template<class T>
void
emcOMQAFEMT<T>::ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& emcBank)
{
  PdbEmcLCTof* pdbqa;

  size_t thesize = 144;

  emcBank.setLength(thesize);

  const emcQAFEM* qaFEM = dynamic_cast<const emcQAFEM*>(&calfem);
  assert(qaFEM != 0);

  emcQAFEM fem(*qaFEM);

  size_t i = 0;
  int channel;
  INT32 error, warning;
  float value1, value2;

  fem.First();

  // Set everything to zero
  for ( i = 0; i < thesize; i++ )
    {
      pdbqa = dynamic_cast<PdbEmcLCTof*>(&(emcBank.getEntry(i)));
      pdbqa->SetLCTofs(0.0, 0.0);
    }

  // Fills only the relevant ones
  while ( fem.Next(channel, error, warning) )
    {
      pdbqa = dynamic_cast<PdbEmcLCTof*>(&(emcBank.getEntry(channel)));
      assert(pdbqa != 0);
      value1 = static_cast<float>(error);
      value2 = static_cast<float>(warning);
      pdbqa->SetLCTofs(value1, value2);
    }
}

#endif
