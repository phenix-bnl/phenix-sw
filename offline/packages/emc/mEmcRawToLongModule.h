#ifndef __MEMCRAWTOLONGMODULE_H__
#define __MEMCRAWTOLONGMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>
#include "PHString.h"

/** (STAF) see mEmcRawToLong_().

This module converts the "raw" output data of the simulation response 
    chain artificially into data in the IDPBSC_DCM32 format.

    @ingroup staf
 */
class PHCompositeNode;

class mEmcRawToLongModule: public SubsysReco {
public:
  mEmcRawToLongModule(): SubsysReco("mEmcRawToLongModule") {}
  virtual ~mEmcRawToLongModule(){}
  int process_event(PHCompositeNode *);
  int process_event(PHCompositeNode *,PHString dEmcRawLongNodeName,PHString dEmcDCMLongNodeName);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCRAWTOLONGMODULE_H__*/
