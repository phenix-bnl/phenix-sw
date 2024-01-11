#ifndef __EMCEMBEDOUTPUTMOVER_H__
#define __EMCEMBEDOUTPUTMOVER_H__

/** Takes both sim and merged information
    and place those in the relevant nodes to be output-able
    by Fun4AllOuputManager(s).
*/

#include "SubsysReco.h"
#include <string>

class EmcEmbedOutputMover : public SubsysReco
{
 public:

  EmcEmbedOutputMover(const char* realnode="REAL",
		      const char* simunode="SIMU",
		      const char* mergednode="TOP");

  int process_event(PHCompositeNode*);

 private:

  bool moveMergedClusters();
  bool movePHGlobal();
  bool moveSimulatedClusters();
  bool moveSTAFtables();
  bool moveSync();
  bool moveVtxOut();

  std::string fRealNode;
  std::string fSimuNode;
  std::string fMergedNode;
};

#endif
