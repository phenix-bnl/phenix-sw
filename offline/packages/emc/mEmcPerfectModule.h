#ifndef __MEMCPERFECTMODULE_H__
#define __MEMCPERFECTMODULE_H__

#include "phool.h"
#include "PHNode.h"
#include "PHPointerList.h"

/** (STAF) see mEmcPerfect_().

 This module is used for "perfect tracking"; for each track that
    reaches the calorimeter it will store the impact point and the
    energy deposited by this track - irrespective of the fact whether
    an actual clustering would find this particle or not.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0

@ingroup staf

*/
class PHCompositeNode;

class mEmcPerfectModule
{
public:
  mEmcPerfectModule(){}
  virtual ~mEmcPerfectModule(){}
  PHBoolean event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCPERFECTMODULE_H__*/
