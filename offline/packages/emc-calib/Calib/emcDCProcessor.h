#ifndef __EMCDCPROCESSOR_H__
#define __EMCDCPROCESSOR_H__

#include <ctime>
#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif

class emcTowerContainer;

/** (ABC) (ADC,TDC) to (GeV,ns) convertor.
@ingroup calibration
@ingroup interface
@sa emcDataProcessor
 */

class emcDCProcessor : public PHObject
{
public:

  virtual ~emcDCProcessor();

  virtual bool calibrate(emcTowerContainer* pbsc, 
			 emcTowerContainer* pbgl,
			 time_t incrTime=0) = 0;

  ClassDef(emcDCProcessor,0) // ABC of a calibrator (ADC,TDC to GeV,ns).
};
#endif
