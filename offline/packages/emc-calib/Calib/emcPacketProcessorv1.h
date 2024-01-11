#ifndef __EMCPACKETPROCESSORV1_H__
#define __EMCPACKETPROCESSORV1_H__

#ifndef __EMCPACKETPROCESSOR_H__
#include "emcPacketProcessor.h"
#endif

/** Implementation of emcPacketProcessor.
@ingroup calibration
 */

class emcPacketProcessorv1 : public emcPacketProcessor
{
public:

  emcPacketProcessorv1();
  virtual ~emcPacketProcessorv1();

  void identify(std::ostream& os = std::cout) const;

  int isValid() const;

  bool process(const Packet&, emcTowerContainer&);

  void Reset();

private:
  int** fDataArray;

  ClassDef(emcPacketProcessorv1,1)
};

#endif
