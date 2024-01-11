#ifndef __EMCRAWDATAPROCESSOR_H__
#define __EMCRAWDATAPROCESSOR_H__

#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif

class emcTowerContainer;
class emcBadModules;

/** (ABC) Raw samples to (ADC,TDC) convertor.
@ingroup calibration
@ingroup interface
@sa emcDataProcessor
 */

class emcRawDataProcessor : public PHObject
{
public:

  emcRawDataProcessor();
  virtual ~emcRawDataProcessor();

  virtual bool toADCandTDC(emcTowerContainer* pbsc, 
			   emcTowerContainer* pbgl,
			   const emcBadModules&) = 0;

  ClassDef(emcRawDataProcessor,0) // ABC of a raw to ADC,TDC -values convertor.
};

#endif
