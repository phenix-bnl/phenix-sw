#ifndef __EMCCALIBRATOR_H__
#define __EMCCALIBRATOR_H__

#include <cstdlib>
#include <iostream>
#include <string>
#ifndef __EMCRAWDATAOBJECT_H__
#include <emcRawDataObject.h>
#endif
#ifndef __EMCMANAGEABLE_H__
#include <emcManageable.h>
#endif

class emcMixedDataObject;
class emcCalibratedDataObject;
class emcCalibrationDataHelper;
class PHTimeStamp;


/** (OLD) Virtual base class of a calibrator.
 
This class defines the old calibration interface.    

\deprecated (as soon as online code does not need it anylonger).
See emcDataProcessor instead.

@ingroup oldemccalib

*/

class emcCalibrator
{

public:

  /// ctor
  emcCalibrator()
  {
    SetHighLowLimit();
    SetThresholdADC();
    SetGlobalT0();
    fTwrGlobalT0 = 0;
    SetVerbose();
    SetUseAmpPedestals(true);
    SetUseOnlyLowGain(false);
    fName = "none";
  }
  virtual ~emcCalibrator()
  {}

  /** Calibrate interface going
      from RawDataObject to MixedDataObject.
  */
  virtual bool Calibrate(const emcRawDataObject& rdo,
			 emcMixedDataObject& mdo,
			 const PHTimeStamp& when) = 0;

  /** Calibrate interface going
      from MixedDataObject to CalibratedDataObject.
  */
  virtual bool Calibrate(const emcMixedDataObject& mdo,
			 emcCalibratedDataObject& cdo,
			 const PHTimeStamp& when) = 0;

  /// short cut to go from raw to calibrated
  virtual bool Calibrate(const emcRawDataObject& rdo,
			 emcCalibratedDataObject& cdo,
			 const PHTimeStamp& when) = 0;

  virtual void ForceDBCollection(const PHTimeStamp& /*when*/)
  {
    std::cerr << "emcCalibrator::ForceDBCollection : virtual function called!"
	      << std::endl;
    exit(1);
  }

  /** Get ADC threshold (below this threshold we do not try to calibrate
      energy). */
  virtual float GetThresholdADC(void) const
  {
    return fThresholdADC;
  }

  /** Get Global T0 value.
      which in principle may be run dependent and which is needed 
      almost entirely for presentation purposes (also if we want to use 
      timing in Clustering and need it to be approximately correct) */
  virtual float GetGlobalT0(void) const
  {
    return fGlobalT0;
  }

  /// returns tower dependent corrections to T0
  virtual float GetGlobalT0(int index) const
  {
    return ((fTwrGlobalT0) ? fTwrGlobalT0[index] : 0.);
  }

  /// Get status of last try of collecting calibration data
  virtual bool GetCollectionStatus(const char* /*type*/) const
  {
    return false;
  }

  /// Get ADC channel count limit between high and low gain
  virtual int GetHighLowLimit(void) const
  {
    return fHLLimit;
  }

  /** Get the name of this calibrator.
      (has to be the classname, so
      the CalibratorFactory can detect changes). */
  virtual const char* GetName(void) const
  {
    return fName.c_str();
  }

  /// Get verbose level
  virtual int GetVerbose(void) const
  {
    return fVerbose;
  }

  /// A print utility to inform what the calibrator is doing
  virtual void Print(void) const = 0;

  /// the print utility to print data for individual tower
  virtual void  printData(const emcRawDataObject& rdo, const int towerId) const =0;

  /// Make this calibrator a brand new fresh baby just born (got it?)
  virtual void Reset(void) = 0;

  /// Select the source of calibration data.
  virtual bool SelectSource(const char* type, emcManageable::EStorage source) = 0;
  virtual bool hasHelper(){return false;}
  virtual void storeHelper(emcCalibrationDataHelper * fch){;}

  /** Set the filename of a file containing a list of extra towers
      to be rejected, as compared to those in Q&A objects. */
  virtual void SetExtraRejectListFilename(const char* /*filename*/ = 0)
  { }

  /** Set the filename of a file containing a list of corrections to be applied to calibration coefficients in individual supermodules */
  virtual void SetSMBasedCorrectionFilename(const char* /*filename*/ = "" )
  {
  }

  /// Set Global T0
  virtual void SetGlobalT0(float T0 = 0.0)
  {
    fGlobalT0 = T0;
  }

  /** Set Global T0 for every calorimeter Tower.
      (implementation is user dependent) - DEFAULT - ZERO.*/
  virtual void SetTwrGlobalT0(char * filename = 0)
  {
    if (fTwrGlobalT0 || filename == 0)
      delete [] fTwrGlobalT0;
    fTwrGlobalT0 = NULL;
  }

  /// Set ADC threshold
  virtual void SetThresholdADC(float thres = 10.)
  {
    fThresholdADC = thres;
    std::cout << "<RDC-I> ADC threshold is set to " << fThresholdADC << std::endl;
  }

  /// Set ADC channel count limit between high and low gain
  virtual void SetHighLowLimit(int lim = 1024)
  {
    fHLLimit = lim;
  }

  /// Set the verbose level
  virtual void SetVerbose(int level = 0)
  {
    fVerbose = level;
  }

  /// switches on and off the zero suppression flag
  void SetZeroSuppression(bool swi = true)
  {
    fZeroSuppression = swi;
    std::cout << "<RDC-I> Zero suppression        " << (fZeroSuppression ? "enabled" : "inhibited") << std::endl;
  }
  Bool_t GetZeroSuppression() {return fZeroSuppression;}

  /// Sets UseAmplitudePedestals flag (default - false, no pre/post pedestal subtruction in amplitude measurements)
  void SetUseAmpPedestals(Bool_t flag = false)
  {
    fUseAmpPedestals = flag;
    std::cout << "<RDC-I> Pedestal subtruction    " << (fUseAmpPedestals ? "enabled" : "inhibited") << std::endl;
  }
  /// Sets USE-ONLY-LOW-GAIN flag (default - false, unless otherwise agreed - this is for monitoring purposes only)
  Bool_t GetUseAmpPedestals() {return fUseAmpPedestals;}
  void   SetUseOnlyLowGain(Bool_t flag = false)
  {
    fUseOnlyLowGain = flag;
    std::cout << "<RDC-I> High gain data range    " << ((!fUseOnlyLowGain) ? "enabled" : "inhibited") << std::endl;
  }
  Bool_t GetUseOnlyLowGain(){return fUseOnlyLowGain;}

protected:

  ///  Name of the calibrator
    std::string fName;
    /// tells if zero's should be suppressed from raw data
    Bool_t fZeroSuppression;
    /// tells Calibrator to subtruct pedestals from pre and post amplitude measurements
    Bool_t fUseAmpPedestals;
    /// tells Calibrator NOT TO USE HIGH GAINS AND High-to-Low GAIN ratios
    Bool_t fUseOnlyLowGain;
    /// Verbosity level
    int fVerbose;
    float * fTwrGlobalT0;

  private:

    /// ADC channel count limit between high and low gain
    int fHLLimit;
    /// ADC threshold
    float fThresholdADC;
    /// GlobalT0
    float fGlobalT0;

  };

#endif // #ifndef __emcCalibrator_h__


