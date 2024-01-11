#ifndef __mEmcGeaMakeClusterEvaluation_h__
#define __mEmcGeaMakeClusterEvaluation_h__

#include <SubsysReco.h>

/** (STAF) Evaluator module of the first pass (EMCal information only)
 *    clustering routines.  
 *
 *  It requires the new clustercontainer-type emcal object.
 *  It's an interim one, as the output are still STAF table (burk).
 *
 *  Two tables are generated: dEmcGeaTrackCluster looks at each
 *  track that reaches the calorimeter and gives information on the
 *  clusters to which this track contirbuted (ideally only to one...).
 *  The other table, dEmcGeaClusterTrack has on row for each cluster,
 *  and gives information on the tracks that contributed to this
 *  cluster (ideally only one...).  The results are very useful
 *  when optimizing algorithms, establishing cuts, calculating
 *  efficiencies of particle identification.
 * @ingroup staf
*/

class mEmcGeaMakeClusterEvaluation : public SubsysReco
{
public:
  mEmcGeaMakeClusterEvaluation();
  virtual ~mEmcGeaMakeClusterEvaluation();

  int process_event(PHCompositeNode*);

};

#endif
