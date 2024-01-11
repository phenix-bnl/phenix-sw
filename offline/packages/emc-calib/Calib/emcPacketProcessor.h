#ifndef __EMCPACKETPROCESSOR_H__
#define __EMCPACKETPROCESSOR_H__

class Packet;
class emcTowerContainer;

#ifndef __PHOBJECT_H__
#include "PHObject.h"
#endif

/** (ABC) Packet processor.

@sa emcDataProcessor
@ingroup calibrated
@ingroup interface
 */

class emcPacketProcessor : public PHObject
{
public:

  virtual ~emcPacketProcessor();

  /** Decode the packet to fill the towercontainer.
      Might also check the data. */
  virtual bool process(const Packet&, emcTowerContainer&) = 0;

private:

  ClassDef(emcPacketProcessor,0) // ABC of EMCAL Packet processor.
};

#endif
