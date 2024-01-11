#ifndef __EMCCALIBRATIONDATAOBJECT_H__
#define __EMCCALIBRATIONDATAOBJECT_H__

#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif
#include "TString.h"
#include <map>


/** (OLD) Stores calibrated towers for whole EMCAL.
 
\deprecated It derives from PHObject so it can be written in a TTree using the
PHNodeIOManager of PHOOL.

See emcTowerContainer and emcTowerContent instead.

@ingroup oldemccalib

*/

class emcCalibratedDataObject : public PHObject
{

public:

  // ctor
  emcCalibratedDataObject();
  // cctor
  emcCalibratedDataObject(const emcCalibratedDataObject& obj);

  emcCalibratedDataObject& operator = (const emcCalibratedDataObject& obj);

  // dtor
  virtual ~emcCalibratedDataObject();

  /** @name Operations (merging of 2 CDOs).*/
  //@{
  ///
  emcCalibratedDataObject& operator +=(const emcCalibratedDataObject& obj);
  ///
  emcCalibratedDataObject operator + (const emcCalibratedDataObject& obj) const
  {
    return emcCalibratedDataObject(*this) += obj;
  }
  //@}

  /// RECOMPUTES the total energy.
  Float_t ComputeTotalEnergy();

  /// Returns number of hits with energies above threshold thres.
  Int_t CountHits(Float_t thres) const;

  /// Returns geometrical location given the software key
  void DecodeKey(long key, Int_t& arm, Int_t& sector, Int_t& yrow,
		 Int_t& zrow) const;

  /** Generate software key for a given EMCal Object
      (sets to -1 for non tower objects) */
  long GenerateSoftwareKey(Int_t ItemId) const;

  /// Get everything using internal indexing
  void Get(Int_t index, Float_t& energy, Float_t& time) const;

  /// Get everything using internal indexing
  void Get(Int_t index, Int_t& TowerId, long & softwareKey,
	   Int_t & errorFlag, Float_t& energy, Float_t& time) const;

  /// Get everything using tower indexing
  void GetByTowerId(Int_t towerId, Float_t& energy, Float_t& time) const;

  /** Get status of neighbouring towers.
      See emcQAs.C for warning and error bit map description
  */
  Int_t GetDead(Int_t index) const;

  /// Get dead using tower index
  Int_t GetDeadByTowerId(Int_t TowerId) const;

  // MV 2001/12/08 added warning map
  Int_t GetWarn(Int_t index) const;

  /// Get warnings using tower index
  Int_t GetWarnByTowerId(Int_t TowerId) const;

  /// Get energy using internal indexing
  Float_t GetEnergy(Int_t index) const;

  /// Get energy using tower indexing
  Float_t GetEnergyByTowerId(Int_t towerId) const;

  /// Get internal index from TowerId
  Int_t GetIndexFromTowerId(Int_t TowerId) const;

  /** Returns the EMCal Object Identifier (same thing as TowerId),
      given the object number in the data map.
      The Object (Item) Identifier uniquely identifies the EMCal Object 
      sending its signal to the FEM channel stored as Item 
      (Absolute Tower Number within PHENIX scope). 
      Note : this tower numbering was extended to include FEM channels used to 
      measure signals from References. The monitoring FEM's are grouped into 
      non-existent Sector "none", absPosition>=172.
  */
  Int_t GetItemId(Int_t index) const
  {
    return GetTowerId(index);
  }

  /// Get Maximum size of the internal arrays
  Int_t GetMaxSize(void) const
  {
    return fMaxSize;
  }

  /** WARNING - WARNING - WARNING -
      This method is to be used in online monitoring processes ONLY.
      This very inelegant method is nevertheless a way of taking care
      of the pre-existing software (which is working).\\      
  */
  void GetPointers(float*& ENERGY, float*& TOF);

  /**  Fills-up array with energies seen in individual calorimeter Sectors.
       Array e is assumed to be of size 8.
  */
  void GetSectorEnergies(Float_t * e);

  /// Returns the number of towers of this object.
  Int_t GetSize(void) const
  {
    return fSize;
  }

  /** Get energies in individual supermodules (144 channels)
      Order is given by RawDataAccessor (i.e. by configuration file).
      Array e is assumed to have the required size.
  */
  void GetSMEnergies(Float_t* e);

  /// Gets software key for a given EMCal Object.
  long GetSoftwareKey(Int_t index) const;

  /// Get time using internal indexing
  Float_t GetTime(Int_t index) const;

  /// Get time using tower indexing
  Float_t GetTimeByTowerId(Int_t towerId) const;

  /// Gets TowerId for a given TOWER (locally stored)
  Int_t GetTowerId(Int_t index) const;

  /** Returns total energy seen in the Calorimeter
      (precomputed while calibrating the data) */
  Float_t GetTotalEnergy()
  {
    return fETotal;
  }

  /// Identify ourselves.
  void identify(std::ostream& out) const;

  /// Tells how far the calibration has been done so far.
  Bool_t IsCalibrated(TString what) const;

  /// Tells if this CalibratedDataObject is zero-suppressed or not.
  Bool_t IsZeroSuppressed(void) const
  {
    return fZeroSuppressed;
  }

  /// Reset
  void Reset(void);

  ///  Resets to zero both Energy and Time at Index = index
  void resetByIndex(int index);

  ///  Resets to zero both Energy and Time for Tower = TowerId
  void resetByTowerId(int TowerId);

  /// Set everything for one tower
  void Set(int index, Int_t TowerId, long softwareKey, Int_t dummy = 0,
	   float energy = 0., float time = 0., Int_t deadNeighbours = 0, Int_t warnNeighbours = 0);

  /// Announce that energy has been calibrated.
  void SetEnergyCalibratedFlag(bool energycalibrated)
  {
    fEnergyCalibrated = energycalibrated;
  }

  /// Set the maximum allowed size for this object.
  void SetMaxSize(Int_t thesize);

  /// Announce that time has been calibrated.
  void SetTimeCalibratedFlag(bool timecalibrated)
  {
    fTimeCalibrated = timecalibrated;
  }

  /// Set the total energy.
  void SetTotalEnergy(Float_t E)
  {
    fETotal = E;
  }

  /// Announce that this object is (will be) zero-suppressed.
  void SetZeroSuppressedFlag(Bool_t flag = true)
  {
    fZeroSuppressed = flag;
  }

  /// Give a string with the status of this object.
  TString Status(void);

  /// Update some internal values (e.g. fETotal)
  void Update(void);

  /// output on stream.
  friend std::ostream& operator << (std::ostream&, const emcCalibratedDataObject&);

private:

  // Copy obj into this
  void copyTo(emcCalibratedDataObject& obj) const;

  bool ValidIndex(Int_t index) const
  {
    return ( index >= 0 && index < fSize );
  }

  //==========================================================================
  //============================== Data members below ========================

private:

  /// Actual size of the arrays below
  Int_t fSize;
  /// Max size of arrays
  Int_t fMaxSize; //!
  /// Total Energy in this event
  Float_t fETotal; //!
  /// Calibrated Energies
  Float_t* fEnergy; //[fSize]
  /// Calibrated Times
  Float_t* fTime; //[fSize]
  /// Tower indices (ItemId's)
  Int_t * fTowerId; //[fSize]
  /// Dead map
  Int_t * fDeadNeighbours; //[fSize]
  /// Software keys
  long * fSoftwareKey; //!

  /// tells if data are calibrated or not
  Bool_t fEnergyCalibrated;
  /// tells if data are calibrated or not
  Bool_t fTimeCalibrated;
  /// tells if data are zero suppressed or not
    Bool_t fZeroSuppressed;

    Bool_t fReadOnly; //!

    // MV 2001/12/08
    /// Warning map
    Int_t *fWarnNeighbours; //[fSize]

    /** A map to get the relation between TowerId and the index
        of the other arrays (fEnergy, fTime, etc...) */
    std::map<int, int> fIndexMap; //!

  public:

    // MV 2001/12/12 Incremented version number 2 -> 3
    ClassDef(emcCalibratedDataObject, 3)
  };

#endif // #ifndef __emcCalibratedDataObject_h__
