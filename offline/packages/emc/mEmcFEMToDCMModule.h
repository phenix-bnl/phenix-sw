#ifndef __MEMCFEMTODCMMODULE_H__
#define __MEMCFEMTODCMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>
/** (STAF) see mEmcFEMToDCM_().

@ingroup staf
*/

class PHCompositeNode;

class mEmcFEMToDCMModule: public SubsysReco {
public:
  mEmcFEMToDCMModule(): SubsysReco("mEmcFEMToDCMModule"){}
  virtual ~mEmcFEMToDCMModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCFEMTODCMMODULE_H__*/
