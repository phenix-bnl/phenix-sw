#ifndef __EmcEmbedMergedClusterUpdater_h__
#define __EmcEmbedMergedClusterUpdater_h__

#include "SubsysReco.h"
#include <string>

class EmcEmbedMergedClusterUpdater: public SubsysReco
{
 public:

  
  /** TOP is the node under which merging occurs.
      tmernode is the node, below mergednode, in which the pieces
      needed for reclustering will sit.
      It MUST be different from DST so we get a chance to filter out
      the clusters obtained from reclustering, i.e. read them from
      tmernode and copy only the relevant ones to mergednode/DST
      (the latter location being imposed by Fun4AllOutputManager).
  */
  EmcEmbedMergedClusterUpdater(const char* mergedNode = "TOP",
			       const char* tmerNode = "EMC");

  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:

  std::string fMergedNodeName;
  std::string fTempMergedNode;

};

#endif
