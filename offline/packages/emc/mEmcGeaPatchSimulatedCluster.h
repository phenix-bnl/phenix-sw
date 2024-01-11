#ifndef __mEmcGeaPatchSimulatedCluster_h__
#define __mEmcGeaPatchSimulatedCluster_h__

#include <SubsysReco.h>

/** Tiny module to set the simfrac() field of simulated clusters to 1. */

class mEmcGeaPatchSimulatedCluster : public SubsysReco
{
public:
  mEmcGeaPatchSimulatedCluster();
  virtual ~mEmcGeaPatchSimulatedCluster();

  int process_event(PHCompositeNode*);
};

#endif
