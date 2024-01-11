#ifndef __MEMCGEAMAKERAWMODULE_H__
#define __MEMCGEAMAKERAWMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) see mEmcGeaMakeRaw_().
 This is the core module in calculating detector response for
    simulated data.  It is fairly complicated and has lots of
    functionality, governed by the dEmcRespPar parameter table.
    If you don't know what you are doing darned well, stick to
    the default values...
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
    @ingroup staf
*/

class PHCompositeNode;

class mEmcGeaMakeRawModule: public SubsysReco {
public:
  mEmcGeaMakeRawModule();
  virtual ~mEmcGeaMakeRawModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(float r_lowgain_convfac, float r_highgain_convfac, PHPointerList<PHNode>&);

  bool firstevent;
  float lowgain_convfac;
  float highgain_convfac;
};
#endif /*__MEMCGEAMAKERAWMODULE_H__*/
