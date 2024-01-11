#ifndef __MEMCGEACLUSTEREVALMODULE_H__
#define __MEMCGEACLUSTEREVALMODULE_H__

#include "phool.h"
#include "PHCompositeNode.h"
#include "PHPointerList.h"
#include <SubsysReco.h>

/** (STAF) see mEmcGeaClusterEval_().

Evaluator module of the first pass (EMCal information only)
    clustering routines.  It requires the "Extended" cluster output.
    Two tables are generated: dEmcGeaTrackCluster looks at each
    track that reaches the calorimeter and gives information on the
    clusters to which this track contirbuted (ideally only to one...).
    The other table, dEmcGeaClusterTrack has on row for each cluster,
    and gives information on the tracks that contributed to this
    cluster (ideally only one...).  The results are very useful
    when optimizing algorithms, establishing cuts, calculating
    efficiencies of particle identification.
    Detailed Documentation:
    \URL{http://www.phenix.bnl.gov/WWW/emcal/documentation/offline/doc99}
    @author Gabor David \URL{mailto:david@bnl.gov}
    @version 1.0
@ingroup staf
 */

class mEmcGeaClusterEvalModule: public SubsysReco {
public:
  mEmcGeaClusterEvalModule();
  ~mEmcGeaClusterEvalModule();
  int process_event(PHCompositeNode *);

protected:
  PHBoolean callPAM(PHPointerList<PHNode>&);
};
#endif /*__MEMCGEACLUSTEREVALMODULE_H__*/
