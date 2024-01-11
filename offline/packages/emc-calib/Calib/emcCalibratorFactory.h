#ifndef __EMCCALIBRATORFACTORY_H__
#define __EMCCALIBRATORFACTORY_H__

class emcCalibrator ;

/** (OLD,Factory) Build emcCalibrator objects.
 * 
 *  USAGE: 
 *
 * -# The factory must be initialized:
 *
 *      emcCalibratorFactory::Initialize("X") ; 
 *
 *  where X is one out of: 
 *  - emcRawDataCalibrator    : the calibrator for real raw online data (Run2000)
 *  - emcRawDataCalibratorV1  : the calibrator for real raw online data (Run2001)
 *  - emcRawDataCalibratorV2  : the calibrator for real raw online data (Run2002-2003)
 *
 *  <BR>   
 * -#  Get the instance of the selected calibrator:
 *
 *      emcCalibrator* rdc = emcCalibratorFactory::GetCalibrator() ;  
 *
 *  @author L. Aphecetche (aphecetc@in2p3.fr)
 *  \deprecated To be depcreated as soon as online code does not use 
 *  it anylonger, as emcCalibrator class itself is to be deprecated.
 * @ingroup oldemccalib
 */

class emcCalibratorFactory
{

public:

  /** Initialize the calibrator factory. The calibrator_classname is
      the name of the calibrator class (which must derive from the ABC
      emcCalibrator. */
  static bool Initialize(const char* calibrator_classname);

  /** Return a calibrator of the class defined by the Initialize.
      If the factory is not initialized, it will return 0 and print
      an error message.
  */
  static emcCalibrator* GetCalibrator(void);

public:

  /** dtor not to be used.
      Unfortunately it seems that CINT requires public dtor.
      But should be private really.*/
  virtual ~emcCalibratorFactory();

private:

  /// ctor is not to be used.
  emcCalibratorFactory()
  { }

  /// the instance of the selected calibration method
  static emcCalibrator* fCalibrator;

  /// Copy constructor and assignement are disabled on purpose.
  emcCalibratorFactory(const emcCalibratorFactory& obj);
  emcCalibratorFactory& operator = (const emcCalibratorFactory& obj);
  };

#endif

