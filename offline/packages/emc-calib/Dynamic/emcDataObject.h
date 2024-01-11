#ifndef __EMCDATAOBJECT_H__
#define __EMCDATAOBJECT_H__

// Purpose: Base object for RawDataObject, MixedDataObject and
// CalibratedDataObject
//
// Description: Implements a few methods common to all data objects
//
// To be deprecated as soon as online code does not need it anylonger.
//
// Author: Yves Schutz (schutz@in2p3.fr)

#include "TObject.h"

/** Base DataObject which implements methods common to :
    - emcRawDataObject
    - emcMixedDataObject
    - emcCalibratedDataObject
*/

class emcDataObject : public TObject
{

protected:

  /** You are not supposed to instantiate an object of this class. */
  emcDataObject();
  virtual ~emcDataObject();

public:

  /// Clean the object : all data members initialized
  void Reset();

  void SetDataDescriptionPointers(Int_t * datamap,
				  long * softwareKey,
				  Int_t * dataerrors);
  void GetDataDescriptionPointers(Int_t * &datamap,
				  long * &softwareKey,
				  Int_t * &dataerrors) const;

  void SetDataDescriptionPointers(Int_t * datamap,
				  long * softwareKey,
				  Int_t * dataerrors,
				  Int_t * deadmap);
  void GetDataDescriptionPointers(Int_t * &datamap,
				  long * &softwareKey,
				  Int_t * &dataerrors,
				  Int_t * &deadmap) const;

  // MV 2001/12/08
  void SetDataDescriptionPointers(Int_t * datamap,
				  long * softwareKey,
				  Int_t * dataerrors,
				  Int_t * deadmap,
				  Int_t * warnmap);
  void GetDataDescriptionPointers(Int_t * &datamap,
				  long * &softwareKey,
				  Int_t * &dataerrors,
				  Int_t * &deadmap,
				  Int_t * &warnmap) const;

  /// Returns geometrical location given the software key
  void DecodeKey(long key, Int_t& arm, Int_t& sector, Int_t& yrow, Int_t& zrow) const;

  /** Get status of neighbouring towers.
      For the description of error and warning bits see emcQAs.C
  */
  Int_t GetDead(Int_t index) const;
  Int_t GetWarn(Int_t index) const;

  /// Returns the number of towers of this object defined by the configuration file
  Int_t GetMaxSize(void) const
  {
    return fMaxSize;
  }

  /// Get error related to a given tower
  Int_t GetDataError(Int_t index) const
  {
    return (ValidIndex(index) && (fDataErrors != 0)) ?
      fDataErrors[index] : -9999;
  }
  /// Update data errors if appropriate
  void AddDataError(Int_t index, int flag)
  {
    fDataErrors[index] |= flag;
  }
  /// Get pointer to the fDataErrors array (be careful with this method!)
  Int_t* GetDataErrorPointer(void) const
  {
    return fDataErrors;
  }

  /// Returns the number of towers of this object with non-zero data
  Int_t GetSize(void) const
  {
    return fSize;
  }

  /// Returns the EMCal Object Id given the object naumber in the data map. The Object (Item) Identifier uniquely identifies the EMCal Object sendig its signal to the FEM channel stored as Item  (Absolute Tower Number within PHENIX scope. Note - this tower numbering was extended to include FEM channels used to measure signals from References. The monitoring FEM's are grouped into nonexistent Sector "none"). It is actually implemented in the RawDataObject.
  Int_t GetItemId(Int_t index) const
  {
    return ((fDataMap) ? fDataMap[index] : -1);
  }

  /// Generate software key for a given EMCal Object (sets to -1 for non tower objects)
  long GenerateSoftwareKey(Int_t ItemId) const;

  /// Gets software key for a given EMCal Object (assumes they were generated at instantiate time)
  long GetSoftwareKey(Int_t index) const
  {
    return ((fSoftKey) ? fSoftKey[index] : -1);
  }

  /// Gets error Flag for a given EMCal Object
  long GetErrorFlag(Int_t index) const
  {
    return ((fDataErrors) ? fDataErrors[index] : -1);
  }

  /// Indicates if part(s) of the fDataErrors array is not zero
  bool HasErrors(void) const;

  /// Checks if the index is within the range of the array
  Bool_t ValidIndex(Int_t index) const
  {
    return ( index >= 0 && index < GetMaxSize() );
  }

protected:

  /// Number of towers of this object defined by the configuration file
  Int_t fMaxSize; //!
    /// Number of towers of this object with non-zero data
    Int_t fSize;
    /// Data errors (do not belonge to this object)
    Int_t * fDataErrors; //[fSize]
    /// DataMap (does not belonge to this object)
    Int_t * fDataMap; //[fSize]
    /// Software Keys - belonge to emcRawDataObject object
    long * fSoftKey; //!
    /// Dead map - might belong to this object
    Int_t * fDeadMap; //[fSize]
    /// Tells if the dead map is ours or not.
    bool fOwnDeadMap;
    bool fReadOnly;

    // MV 2001/12/08
    /// Warning map - might belong to this object
    Int_t * fWarnMap; //[fSize]
    /// Tells if the warning map is ours or not.
    bool fOwnWarnMap;

  public:

    // MV 2001/12/12 Incremented class version 1 -> 2
    ClassDef(emcDataObject, 2)
  };

#endif // #ifndef __emcDataObject_h__
