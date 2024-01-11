#ifndef __EMCOMPEDESTALFEM_H__
#define __EMCOMPEDESTALFEM_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

/**
   (Template) Plugin class for emcPedestalFEM (3 per AMU) objects.
   @ingroup dmplugins
*/

template <class T>
class emcOMPedestalFEMT : public emcOMCalFEMT<T>
{
public:
  emcOMPedestalFEMT(const char* name, const char* title)
    : emcOMCalFEMT<T>(name, title)
  { }
  virtual ~emcOMPedestalFEMT()
  { }

  /** We announce that we can read {\tt emcGains} object. */
  virtual bool CanRead(const emcManageable& object) const;

  /** We announce that we can write {\tt emcGains} object. */
  virtual bool CanWrite(const emcManageable& object) const;

  /// Fill an emcCalFEM object from a PdbCalBank
  virtual void FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank);


  // Get the name of the low-level Objy persistent class we are dealing with.
  virtual std::string GetPersistentClassName(void) const
  {
    return "PdbEmcPedestalBank";
  }

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank);
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#ifndef __EMCPEDESTALFEM_H__
#include "emcPedestalFEM.h"
#endif
#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif

#include "PdbCalBank.hh"
#include "PdbEmcPedestal.hh"

#include <iostream>
#include <fstream>
#include <cassert>
#include <cstdio>

//_____________________________________________________________________________
template <class T>
bool
emcOMPedestalFEMT<T>::CanRead(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcPedestalFEM only.

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcPedestalFEM* test = dynamic_cast<const emcPedestalFEM*>(object_ptr);

  if (test && test->Version() == 0)
    {
      return true;
    }
  return false;
}


//_____________________________________________________________________________
template <class T>
bool
emcOMPedestalFEMT<T>::CanWrite(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcPedestalFEM only.

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcPedestalFEM* test = dynamic_cast<const emcPedestalFEM*>(object_ptr);

  if (test && test->Version() == 0)
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
template<class T>
void
emcOMPedestalFEMT<T>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& emcbank)
{
  assert(calfem.Version() == 0);

  int thesize = emcbank.getLength();

  assert (thesize == 144);

  int i, amu;
  int low, high, tac;
  PdbEmcPedestal* pdbped;
  emcPedestalFEM* pedFEM = dynamic_cast<emcPedestalFEM*>(&calfem);
  assert(pedFEM != 0);

  emcPedestalFEM::AmuVector vlow(64), vhigh(64), vtac(64);

  for ( i = 0; i < thesize; i ++ )
    {

      // Get the PdbEmcPedestal object for channel i from the bank
      pdbped = dynamic_cast<PdbEmcPedestal*>(&(emcbank.getEntry(i)));
      for ( amu = 0; amu < 64; amu ++)
        {
          pdbped->GetValues(amu, low, high, tac);
          vlow[amu] = low;
          vhigh[amu] = high;
          vtac[amu] = tac;
        }
      // it's important to group the following lines (as it's an
      // append for the same channel).
      // FIXME: there's plenty of improvement to do with this
      // nasty set interface !
      // (ideas are welcome)
      pedFEM->AppendOneChannel("LG_Pre-Post", vlow);
      pedFEM->AppendOneChannel("HG_Pre-Post", vhigh);
      pedFEM->AppendOneChannel("TAC", vtac);
    }
}

//_____________________________________________________________________________
template<class T>
void
emcOMPedestalFEMT<T>::ToPdbCalBank(const emcCalFEM& calfem, 
				   PdbCalBank& emcBank)
{
  assert(calfem.Version() == 0);

  size_t thesize = calfem.GetNumberOfChannels();
  assert(thesize == 144);

  emcBank.setLength(thesize);

  PdbEmcPedestal* pdbped;
  const emcPedestalFEM* pedFEM = dynamic_cast<const emcPedestalFEM*>(&calfem);
  assert(pedFEM != 0);

  int i, amu;
  int low, high, tac;

  for ( i = 0; i < 144; i++ )
    {
      pdbped = (PdbEmcPedestal*) & (emcBank.getEntry(i));
      assert(pdbped != 0);
      for ( amu = 0; amu < 64; amu++ )
        {
          low = static_cast<int>(pedFEM->getValue(i, amu, "LG_Pre-Post"));
          high = static_cast<int>(pedFEM->getValue(i, amu, "HG_Pre-Post"));
          tac = static_cast<int>(pedFEM->getValue(i, amu, "TAC"));
          pdbped->Set(amu, low, high, tac);
        }
    }
}

#endif
