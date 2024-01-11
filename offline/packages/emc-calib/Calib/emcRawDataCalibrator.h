#ifndef __EMCRAWDATACALIBRATOR_H__
#define __EMCRAWDATACALIBRATOR_H__

#ifndef __EMCCALIBRATOR_H__
#include "emcCalibrator.h"
#endif
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#include <vector>

class emcRawDataObject;
class emcMixedDataObject;
class emcCalibratedDataObject;
class emcCalibrationDataHelper;
class emcFEMtuple ;
class emcWalkTofs ;
class emcLCTofs ;
class emcTofT0s;
class emcGains ;
class emcQAs ;
class emcPedestals ;
class emcHLRatios ;
class emcTacPeds;

/** (OLD) Implementation of emcCalibrator for Run1.

\deprecated

    Goes from raw data to calibrated data.
 
-# subtract pedestals
-# applies high/low gain ratios
-# calibrates adc and tdc.

@ingroup oldemccalib
*/

class emcRawDataCalibrator : public emcCalibrator
{
public:

  /// ctor
  emcRawDataCalibrator() ;
  /// dtor
  virtual ~emcRawDataCalibrator() ;

  /// Changes raw data into mixed data
  virtual bool Calibrate(const emcRawDataObject& rdo,
			 emcMixedDataObject& mdo,
			 const PHTimeStamp& when) ;
  /// Changes mixed data into calibrated data
  virtual bool Calibrate(const emcMixedDataObject& mdo,
			 emcCalibratedDataObject& cdo,
			 const PHTimeStamp& when) ;

  /// short cut to go from raw to calibrated
  virtual bool Calibrate(const emcRawDataObject& rdo,
			 emcCalibratedDataObject& cdo,
			 const PHTimeStamp& when) ;

  /** Get status of last try of collecting data.
      For the moment type can be "Pedestals", "HLRatios", "Tofs", 
      or "*" (i.e. all)
  */
  virtual bool GetCollectionStatus(const char* type) const ;

  /// the print utility to inform what the calibrator is doing
  virtual void Print() const ;

  /// the print utility to print data for individual tower
  virtual void  printData(const emcRawDataObject& rdo, const int towerId) const {;}

  /// Complete reset.
  virtual void Reset(void) ;

  /** select the source of the calibration parameters.
      Valid types are:   Pedestals, HLRatios, ToF, QAs, IniCal
      Valid sources are: kDB_Objy = Objectivity Data base, 
      kFile_ASCII = ASCII Files.
  */
  virtual bool SelectSource(const char* type, emcManageable::EStorage source);

  /** Set the filename of a file containing a list of extra towers
      to be rejected, as compared to those in Q&A objects. */
  virtual void SetExtraRejectListFilename(const char* filename = "" )
  {
    fExtraRejectListFilename = filename ;
  }

  /** Set the filename of a file containing a list of corrections to be applied to calibration coefficients in individual supermodules */
  virtual void SetSMBasedCorrectionFilename(const char* filename = "" )
  {
    fSMBasedCorrectionFilename = filename ;
  }
  /// Set Global T0 for every calorimeter Tower
  virtual void SetTwrGlobalT0(char * filename = 0);

private:

  bool CalibrateEnergy(Float_t & adc, const Int_t index,
		       const Int_t TowerId, int incrementalTime) ;

  bool CalibrateTime(Float_t & tdc, float adc, const Int_t index,
		     const Int_t TowerId, int incrementalTime) ;

  void CollectForMDO(const PHTimeStamp& when) ;
  void CollectForCDO(const PHTimeStamp& when) ;

  bool GetECalAtT0(const PHTimeStamp& when, bool normalizationON = false ) ;

private:

  std::string fExtraRejectListFilename ;
  std::string fSMBasedCorrectionFilename ;

  /// Q&A object
  emcQAs* fQA ;
  /// energy calibration at time 0
  std::vector<float> fECalAtT0 ;
  /// tells where to load pedestals data from
  emcManageable::EStorage fPedestalsSource ;
  /// tells where to load high/low gain ratios data from
  emcManageable::EStorage fHLRatiosSource ;
  /// tells where to load gain data from
  emcManageable::EStorage fGainsSource ;
  /// tells where to load ToF data from
  emcManageable::EStorage fTofSource ;
  /// tells where to load Q&A data from
  emcManageable::EStorage fQASource ;
  /// tells where to load Initial Calibration data from
  emcManageable::EStorage fIniCalSource ;

  /// Status of the last try of collecting Pedestals
  bool fCollectPedestalStatus ;
  /// Status of the last try of collecting HLRatios
  bool fCollectHLRatioStatus ;
  /// Status of the last try of collecting Gains
  bool fCollectGainStatus ;
  /// Status of the last try of collecting TOF LC and WALKS
  bool fCollectTofStatus ;
  /// Maximum Number of failed attempts to collect Monitoring Data of a given kind
    Int_t maxFailPed, maxFailHLR, maxFailGain, maxFailTof;

    emcPedestals *fPedestals ;
    emcHLRatios *fHLRatios ;
    emcGains *fGains ;
    emcLCTofs *fLCTofs ;
    emcWalkTofs *fWalkTofs ;
    emcTofT0s *fTofT0s ;
    emcTacPeds *fTacPeds ;
  } ;

#endif
