#ifndef __EMCOMPEDESTAL5FEM_H__
#define __EMCOMPEDESTAL5FEM_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

/**
   (Template) Plugin class for emcPedestalFEM (5 per amu) object.
   @ingroup dmplugins
*/

template <class T>
class emcOMPedestal5FEMT : public emcOMCalFEMT<T>
{

public:
  emcOMPedestal5FEMT(const char* name, const char* title)
    : emcOMCalFEMT<T>(name, title)
  { }
  virtual ~emcOMPedestal5FEMT()
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
    return "PdbEmcPedestal5Bank";
  }

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank);

};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#ifndef __EMCDATAMANAGER_H__
#include "emcDataManager.h"
#endif
#ifndef __EMCPEDESTALFEM_H__
#include "emcPedestalFEM.h"
#endif
#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif

#include "PdbCalBank.hh"
#include "PdbEmcPedestal5.hh"

#include <iostream>
#include <fstream>
#include <cassert>
#include <cstdio>

//_____________________________________________________________________________
template <class T>
bool
emcOMPedestal5FEMT<T>::CanRead(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcPedestalFEM only.

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  bool rv = false;
  const emcManageable* object_ptr = &object;
  const emcPedestalFEM* test = dynamic_cast<const emcPedestalFEM*>(object_ptr);

  if (test && test->Version() == 1)
    {
      rv = true;
    }
  return rv;
}

//_____________________________________________________________________________
template <class T>
bool
emcOMPedestal5FEMT<T>::CanWrite(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcPedestalFEM only.

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  bool rv = false;
  const emcManageable* object_ptr = &object;
  const emcPedestalFEM* test = dynamic_cast<const emcPedestalFEM*>(object_ptr);

  if (test && test->Version() == 1)
    {
      rv = true;
    }
  return rv;
}

//_____________________________________________________________________________
template<class T>
void
emcOMPedestal5FEMT<T>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& emcBank)
{
  assert(calfem.Version() == 1);

  int thesize = emcBank.getLength();
  assert (thesize == 144);
  int i, amu;

  PdbEmcPedestal5* pdbped;
  emcPedestalFEM* pedFEM = dynamic_cast<emcPedestalFEM*>(&calfem);
  assert(pedFEM != 0);

  emcPedestalFEM::AmuVector vhgpre(64), vhgpost(64),
    vlgpre(64), vlgpost(64), vtac(64);
  int hgpre, hgpost, lgpre, lgpost, tac;

  for ( i = 0; i < thesize; i ++ )
    {
      // Get the PdbEmcPedestal5 object for channel i from the bank
      pdbped = dynamic_cast<PdbEmcPedestal5*>(&(emcBank.getEntry(i)));
      assert(pdbped != 0);
      for ( amu = 0; amu < 64; amu ++)
        {
          pdbped->GetValues(amu, hgpre, hgpost, lgpre, lgpost, tac);
          vhgpre[amu] = hgpre;
          vhgpost[amu] = hgpost;
          vlgpre[amu] = lgpre;
          vlgpost[amu] = lgpost;
          vtac[amu] = tac;
        }
      // it's important to group the following lines (as it's an
      // append for the same channel).
      // FIXME: there's plenty of improvement to do with this
      // nasty set interface !
      // (ideas are welcome)
      pedFEM->AppendOneChannel("LG_Pre", vlgpre);
      pedFEM->AppendOneChannel("HG_Pre", vhgpre);
      pedFEM->AppendOneChannel("LG_Post", vlgpost);
      pedFEM->AppendOneChannel("HG_Post", vhgpost);
      pedFEM->AppendOneChannel("TAC", vtac);
    }
}


//_____________________________________________________________________________
template<class T>
void
emcOMPedestal5FEMT<T>::ToPdbCalBank(const emcCalFEM& calfem, 
				    PdbCalBank& emcBank)
{
  assert(calfem.Version() == 1);

  size_t thesize = calfem.GetNumberOfChannels();
  assert(thesize == 144);

  emcBank.setLength(thesize);

  const emcPedestalFEM* pedFEM = dynamic_cast<const emcPedestalFEM*>(&calfem);
  assert(pedFEM != 0);

  int i, amu;

  PdbEmcPedestal5* pdbped;
  int lgpre, lgpost, hgpre, hgpost, tac;

  for ( i = 0; i < 144; i++ )
    {
      pdbped = (PdbEmcPedestal5*) & (emcBank.getEntry(i));
      assert(pdbped != 0);
      for ( amu = 0; amu < 64; amu++ )
        {
          hgpre = static_cast<int>(pedFEM->getValue(i, amu, "HG_Pre"));
          hgpost = static_cast<int>(pedFEM->getValue(i, amu, "HG_Post"));
          lgpre = static_cast<int>(pedFEM->getValue(i, amu, "LG_Pre"));
          lgpost = static_cast<int>(pedFEM->getValue(i, amu, "LG_Post"));
          tac = static_cast<int>(pedFEM->getValue(i, amu, "TAC"));
          pdbped->Set(amu, hgpre, hgpost, lgpre, lgpost, tac);
        }
    }
}


#endif
