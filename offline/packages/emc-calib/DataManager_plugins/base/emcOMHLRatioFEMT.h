#ifndef __EMCOMHLRATIOFEMT_H__
#define __EMCOMHLRATIOFEMT_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

#include <cassert>

/**
   (Template) Plugin class for emcHLRatioFEM objects.
   @ingroup dmplugins
*/

template <class T>
class emcOMHLRatioFEMT : public emcOMCalFEMT<T>
{
public:
  emcOMHLRatioFEMT(const char* name, const char* title)
    : emcOMCalFEMT<T>(name, title)
  { }

  virtual ~emcOMHLRatioFEMT()
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
    return "PdbEmcHLRatioVectorBank";
  }

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank);
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include "EmcIndexer.h"
#include "emcDefines.h"
#include "PdbEmcHLRatioVector.hh"
#include "emcHLRatioFEM.h"

//_____________________________________________________________________________
template <class T>
bool
emcOMHLRatioFEMT<T>::CanRead(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcHLRatioFEM only.

  bool rv = false ;

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object ;
  const emcHLRatioFEM* test = dynamic_cast<const emcHLRatioFEM*>(object_ptr) ;

  if (test)
    {
      rv = true ;
    }
  return rv ;
}

//_____________________________________________________________________________
template <class T>
bool
emcOMHLRatioFEMT<T>::CanWrite(const emcManageable& object) const
{
  // we advertize here that we can only handle a given
  // number of object types, e.g. emcHLRatioFEM only.

  bool rv = false ;

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object ;
  const emcHLRatioFEM* test = dynamic_cast<const emcHLRatioFEM*>(object_ptr) ;

  if (test)
    {
      rv = true ;
    }
  return rv ;
}

//_____________________________________________________________________________
template <class T>
void
emcOMHLRatioFEMT<T>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& emcBank)
{
  size_t thesize = emcBank.getLength() ;
  assert (thesize == 144);
  PdbEmcHLRatioVector* pdbhlr;
  emcHLRatioFEM* hlrFEM = dynamic_cast<emcHLRatioFEM*>(&calfem) ;
  assert(hlrFEM != 0) ;
  hlrFEM->Reset() ;

  size_t i;
  float average;
  float rms;
  float intercept;
  float slope;

  for ( i = 0 ; i < thesize ; i++)
    {
      pdbhlr = dynamic_cast<PdbEmcHLRatioVector*>(&(emcBank.getEntry(i)));
      pdbhlr->GetRatioVector(average, rms, intercept, slope);
      hlrFEM->AppendOneChannel(average, rms, intercept, slope);
    }
}

//_____________________________________________________________________________
template <class T>
void
emcOMHLRatioFEMT<T>::ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& emcBank)
{

  PdbEmcHLRatioVector* pdbhlr;

  emcBank.setLength(144);

  const emcHLRatioFEM* hlrFEM = dynamic_cast<const emcHLRatioFEM*>(&calfem) ;
  assert(hlrFEM != 0) ;
  assert(hlrFEM->GetNumberOfChannels() == 144) ;

  for ( size_t i = 0 ; i < hlrFEM->GetNumberOfChannels(); i++)
    {
      pdbhlr = (PdbEmcHLRatioVector*) & (emcBank.getEntry(i)) ;
      assert(pdbhlr != 0) ;
      pdbhlr->SetRatioVector(hlrFEM->getValue(i, 0),
                             hlrFEM->getValue(i, 1),
                             hlrFEM->getValue(i, 2),
                             hlrFEM->getValue(i, 3)) ;
    }
}

#endif
