#ifndef __MEMCDEFGEOMMODULE_H__
#define __MEMCDEFGEOMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) See mEmcDefGeom_().

 This module generates the default geometry (as built in PISA)
    for runs where the PISA GEOM bank is not available.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
@ingroup staf
 */

class PHCompositeNode;

class mEmcDefGeomModule: public SubsysReco {
public:
  mEmcDefGeomModule(): SubsysReco("mEmcDefGeomModule"){}
  virtual ~mEmcDefGeomModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCDEFGEOMMODULE_H__*/
