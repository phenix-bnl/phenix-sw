#ifndef __EMCDATAPROCESSOR_H__
#define __EMCDATAPROCESSOR_H__

#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif
#include <ctime> // for time_t

class Event;
class emcTowerContainer;
class emcBadModules;

/** (ABC) Raw-to-calibrated-data processor.

    Processing is decomposed in three steps:

    1) decode() = unpacking the event (i.e. go from Packet structure
       to our internal structures)

    2) toADCandTDC() = combine pre and post samples to make ADC and TDC
       counts

    3) calibrate() = convert ADC and TDC into GeV and ns.

    Each step might be performed separately for PbSc and PbGl.

    In all three methods above, a null pointer for one of the
    emcTowerContainer pointer means "do not consider this calorimeter".

    @ingroup calibration
    @ingroup interface
*/

class emcDataProcessor : public PHObject
{
public:

  /// Ctor.
  emcDataProcessor();

  virtual ~emcDataProcessor();

  /** Calibrate current event (ADC,TDC)->(Energy,TOF).
      See toADCandTDC for warning about pbsc/pbgl.
   */
  virtual bool calibrate(emcTowerContainer* pbsc, 
			 emcTowerContainer* pbgl,
			 time_t incrementalTime=0) = 0;
  
  /** Decode one event (Packet->H/L/pre/post/Tac) and fills
      seperately PbSc and PbGl towers. */
  virtual bool decode(const Event&, 
		      emcTowerContainer* pbsc, 
		      emcTowerContainer* pbgl) = 0;

  /** After event decoding, get ADC and TDC values from
      H/L/pre/post/TAC values.
      You're supposed to give as input separately PbSc and PbGl
      towers. No further check is done about that. Is you mix
      things, you'll get garbage most probably !
      If pbsc or pbgl is a null pointer, it will simply be ignored.
  */
  virtual bool toADCandTDC(emcTowerContainer* pbsc, 
			   emcTowerContainer* pbgl,
			   const emcBadModules&) = 0;

  //  virtual void setRunNumber(int runnumber) = 0;

  ClassDef(emcDataProcessor,0) // EMCAL Raw Data Processor (ABC)
};

#endif
