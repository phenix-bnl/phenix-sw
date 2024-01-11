#ifndef __MEMCGEAEVENTMODULE_H__
#define __MEMCGEAEVENTMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) see mEmcGeaEvent_().

This module gets the actual vertex in the event when processing
    simulated data.  It uses "header" as an input.  The vertex info
    is needed in order to properly calculate TOF, position and PC3
    projection.  It produces the same output table as mEmcEvent for
    real events.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
@ingroup staf
*/

class PHCompositeNode;

class mEmcGeaEventModule: public SubsysReco {
public:
  mEmcGeaEventModule();
  virtual ~mEmcGeaEventModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCGEAEVENTMODULE_H__*/
