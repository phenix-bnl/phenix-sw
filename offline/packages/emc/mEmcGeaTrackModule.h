#ifndef __MEMCGEATRACKMODULE_H__
#define __MEMCGEATRACKMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>
/** (STAF) see mEmcGeaTrack_().

 This module fetches information from "fkin" (hidden, accessed via
    dio_ptrkstack) for each track that deposited energy in the calorimeter.
    (The info here is essentially a subset of the fkin info).
    Also, for each particle reaching the calorimeter it will tell you
    whether it is a primary (generated in the vertex), or it has
    "ancestors" - and if so, lists data of the ancestors up to an
    arbitrary number of "generations".  The default is that the
    search is cut off at the third generation.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
@ingroup staf

 */

class PHCompositeNode;

class mEmcGeaTrackModule: public SubsysReco {
public:
  mEmcGeaTrackModule();
  virtual ~mEmcGeaTrackModule(){}
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCGEATRACKMODULE_H__*/
