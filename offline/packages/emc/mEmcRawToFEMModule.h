#ifndef __MEMCRAWTOFEMMODULE_H__
#define __MEMCRAWTOFEMMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) see mEmcRawToFEM_()/

This module converts the "raw" output data of the simulation response 
    chain artificially into data in the FEM format, using a special,
    simulation-specific map.  This is a step needed to bring simulated
    data into the same PRDF format as the real data, so that the
    reconstruction chain would not know whether it works on simulated
    or real data.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
    @ingroup staf
 */

class PHCompositeNode;

class mEmcRawToFEMModule: public SubsysReco {
public:
  mEmcRawToFEMModule(): SubsysReco("mEmcRawToFEMModule"){}
  virtual ~mEmcRawToFEMModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCRAWTOFEMMODULE_H__*/
