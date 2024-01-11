#ifndef __EMCOMTOFT0FEMT_H__
#define __EMCOMTOFT0FEMT_H__

#ifndef __EMCOMTRACEDFEMT_H__
#include "emcOMTracedFEMT.h"
#endif
#ifndef __EMCTOFT0FEM_H__
#include "emcTofT0FEM.h"
#endif

class emcTracedFEM;

/**
   (Template) Partial specialization of emcOMTracedFEMT for emcTofT0FEM objects.
   @ingroup dmplugins
*/

template <class T>
class emcOMTracedFEMT<T,emcTofT0FEM> : public emcOMCalFEMT<T>
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
template <class T>
bool
emcOMTracedFEMT<T, emcTofT0FEM>::CanRead(const emcManageable& object) const
{
  // We advertize here that we can only handle a given
  // number of object types

  if ( object.GetSource() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcTofT0FEM* test = dynamic_cast<const emcTofT0FEM*>(object_ptr);

  if (test)
    {
      std::string test_category = emcTofT0FEM(0,test->Version()).GetCategory();
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
template <class T>
bool
emcOMTracedFEMT<T, emcTofT0FEM>::CanWrite(const emcManageable& object) const
{
  // We advertize here that we can only handle a given
  // number of object types : emcGainFEM or emcTofT0FEM
  // or emcTacPedFEM.

  if ( object.GetDestination() != this->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;
  const emcTofT0FEM* test = dynamic_cast<const emcTofT0FEM*>(object_ptr);

  if (test)
    {
      std::string test_category = emcTofT0FEM(0,test->Version()).GetCategory();
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
template <class T>
void
emcOMTracedFEMT<T, emcTofT0FEM>::FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank)
{
  // Fills calfem from the values in bank.
  //
  // Max. number of channels handled by this method = 144.
  // (as channel#=144 and 145 do have special meaning).
  //
  // thesize take into account normal items + 2
  // (xmin and xmax)

  emcTofT0FEM& tracedFEM = dynamic_cast<emcTofT0FEM&>(calfem);

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

  assert(extra == 1 || extra == 2 || extra==3);

  pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
  assert(pdbtv != 0);
  pdbtv->Get(j, thex, constant, slope);
  assert(constant == 0);
  assert(slope == 0);

  phtime_t xmax = thex;

  if (extra == 3 ) 
    { 
      // version where both xmin and xmax are stored, plus bbct0
      n++;
      pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
      assert(pdbtv != 0);
      pdbtv->Get(j, thex, constant, slope);
      assert(constant == 0);
      assert(slope == 0);
      calfem.SetXmin(thex);
      calfem.SetXmax(xmax);
      ++n;
      pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
      assert(pdbtv != 0);
      pdbtv->Get(j, thex, constant, slope);
      assert(thex==0);
      assert(slope==0);
      tracedFEM.setBBCT0(constant);
    }
  else if (extra == 2)
    {
      // version where both xmin and xmax are stored.
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
template <class T>
void
emcOMTracedFEMT<T,emcTofT0FEM>::ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank)
{
  int nvalues = 0;
  const emcTofT0FEM& ctracedFEM = static_cast<const emcTofT0FEM&>(calfem);
  emcTofT0FEM& tracedFEM = const_cast<emcTofT0FEM&>(ctracedFEM);

  nvalues += tracedFEM.GetNumberOfItems();

  /* The bank will in fact has size nvalues+2(3), the last values
     being used only to store the xmax and xmin of this object.
  */

  int theshift = 2;
  if ( tracedFEM.Version() > 0 )
    {
      theshift = 3;
    }

  bank.setLength(nvalues + theshift);

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
  if ( theshift == 3 )
    {
      ++n;
      pdbtv = (PdbEmcTracedValue*) & (bank.getEntry(n));
      assert(pdbtv != 0);
      float bbct0 = tracedFEM.getValueFast(0);
      pdbtv->Set(tracedFEM.GetNumberOfChannels() + 2,
                 0,
                 bbct0,
                 0);
    }
}
#endif
