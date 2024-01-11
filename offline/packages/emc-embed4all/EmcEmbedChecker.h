#ifndef __EMCEMBEDRECHECKER_H__
#define __EMCEMBEDRECHECKER_H__

#include "SubsysReco.h"
#include "mEmcGeometryModule.h"

/** Checkerr module for EMCAL embedding.
 */

class EmcEmbedChecker : public SubsysReco
{
 public:

  EmcEmbedChecker(const char* realnode = "REAL",
		   const char* simunode = "SIMU",
		   const char* mergednode = "TOP",
		   const char* tmernode = "EMC");

  int process_event(PHCompositeNode*);

 private:

  std::string fRealNode;
  std::string fSimuNode;
  std::string fMergedNode;
  std::string fTempMergedNode;
};

#endif
