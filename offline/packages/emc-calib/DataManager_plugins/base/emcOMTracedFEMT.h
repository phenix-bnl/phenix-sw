#ifndef __EMCOMTRACEDFEM_H__
#define __EMCOMTRACEDFEM_H__

#ifndef __EMCOMCALFEMT_H__
#include "emcOMCalFEMT.h"
#endif

class emcTracedFEM;

/**
   (Template) Plugin class for emcTracedFEM objects (e.g. Gains)
   @ingroup dmplugins
*/

template <class T, class B>
class emcOMTracedFEMT : public emcOMCalFEMT<T>
{

public:
  emcOMTracedFEMT(const char* name, const char* title) :
    emcOMCalFEMT<T>(name, title)
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
    return "PdbEmcTracedValueBank";
  }

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank);

};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include "PdbCalBank.hh"
#include "PdbEmcTracedValue.hh"

#ifndef __EMCINDEXER_H__
#include "EmcIndexer.h"
#endif
#ifndef __EMCTRACEDVALUE_H__
#include "emcTracedValue.h"
#endif

#include <cassert>
#include <ctime>
#include <string>
#include <fstream>

//_____________________________________________________________________________
template <class T, class B>
bool
emcOMTracedFEMT<T,B>::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const B* test = dynamic_cast<const B*>(object_ptr);

  if (test)
    {
      std::string test_category = B(0).GetCategory();
      if ( test_category == object.GetCategory() )
	{
	  return true;
	}
      else
	{
	  return false;
	}
    }

  return false;
}

//_____________________________________________________________________________
template <class T, class B>
bool
emcOMTracedFEMT<T,B>::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const B* test = dynamic_cast<const B*>(object_ptr);

  if (test)
    {
      std::string test_category = B(0).GetCategory();
      if ( test_category == object.GetCategory() )
	{
	  return true;
	}
      else
	{
	  return false;
	}
    }

  return false;
}

//_____________________________________________________________________________
template<class T, class B>
void
emcOMTracedFEMT<T,B>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank)
{
  // Fills calfem from the values in bank.
  //
  // Max. number of channels handled by this method = 144.
  // (as channel#=144 and 145 do have special meaning).
  //
  // thesize take into account normal items + 2
  // (xmin and xmax)

  B& tracedFEM = dynamic_cast<B&>(calfem);

  int nitems = 0;
  size_t nchannels = 0;
  size_t thesize = bank.getLength();
  int j = 0, lastj = -1;
  int thex;
  float constant, slope;
  PdbEmcTracedValue* pdbtv;

  // find the number of items and number of channels.
  for ( size_t i = 0; i < thesize; i++ )
    {
      pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(i));
      assert(pdbtv != 0);
      pdbtv->Get(j, thex, constant, slope);
      if (j < 144)
        {
          nitems++;
          if ( j > lastj )
            nchannels = j;
        }
      lastj = j;
    }

  nchannels++;

  int extra = thesize - nitems;

  tracedFEM.SetNumberOfChannels(nchannels);
  int n = 0;

  // We loop over the bank to extract data.
  for ( int i = 0; i < nitems; i++ )
    {
      pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(i));
      pdbtv->Get(j, thex, constant, slope);
      tracedFEM.AddNewItem(j, new emcTracedValue(thex, constant, slope));
      n++;
    }

  // Last line of the bank is used for storing xmax (and xmin))

  assert(extra == 1 || extra == 2);

  pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
  assert(pdbtv != 0);
  pdbtv->Get(j, thex, constant, slope);
  assert(constant == 0);
  assert(slope == 0);

  phtime_t xmax = thex;

  if (extra == 2)
    {
      // new version where both xmin and xmax are stored.
      n++;
      pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
      assert(pdbtv != 0);
      pdbtv->Get(j, thex, constant, slope);
      assert(constant == 0);
      assert(slope == 0);
      calfem.SetXmin(thex);
      calfem.SetXmax(xmax);
    }
  else
    {
      // old version where only xmax was put in the db.
      // and xmax was a relative one !
      if ( EmcIndexer::isPbScFEM(calfem.AbsolutePosition()) )
        {
          phtime_t tics0 = calfem.GetStartValTime().getTics();
          calfem.SetXmin(tics0);
          calfem.SetXmax(xmax + tics0);
        }
    }
}

//_____________________________________________________________________________
template<class T, class B>
void
emcOMTracedFEMT<T,B>::ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank)
{
  int nvalues = 0;
  const B& ctracedFEM = static_cast<const B&>(calfem);
  B& tracedFEM = const_cast<B&>(ctracedFEM);

  nvalues += tracedFEM.GetNumberOfItems();

  /* The bank will in fact has size nvalues+2, the last values
     being used only to store the xmax and xmin of this object.
  */

  bank.setLength(nvalues + 2);

  // We then fill the bank

  PdbEmcTracedValue* pdbtv;
  emcTracedValue* val;
  int n = 0;

  for ( size_t i = 0; i < tracedFEM.GetNumberOfChannels(); i++ )
    {
      tracedFEM.FirstItem(i);
      while ( (val = tracedFEM.NextItem()) != 0 )
        {
          pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
          assert(pdbtv != 0);
          pdbtv->Set(i, val->GetX(), val->GetConstant(), val->GetSlope());
          n++;
        }
    }

  pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
  assert(pdbtv != 0);
  pdbtv->Set(tracedFEM.GetNumberOfChannels(), tracedFEM.GetXmax(), 0, 0);
  n++;
  pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
  assert(pdbtv != 0);
  pdbtv->Set(tracedFEM.GetNumberOfChannels() + 1, tracedFEM.GetXmin(), 0, 0);
}

#endif
