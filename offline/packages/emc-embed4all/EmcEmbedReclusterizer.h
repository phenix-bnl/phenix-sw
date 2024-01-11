#ifndef __EMCEMBEDRECLUSTERIZER_H__
#define __EMCEMBEDRECLUSTERIZER_H__

#include "SubsysReco.h"
#include "mEmcGeometryModule.h"

/** Reclusterizer module for EMCAL embedding.
 */

class EmcEmbedReclusterizer : public SubsysReco
{
 public:

  /** The clustering will be done on the towers found under TOP node,
      but the other 2 nodes may be needed to get access to the
      vertex (through VtxOut object).
      The tmernode is the node, below mergednode, in which the pieces
      needed for reclustering will sit.
      It MUST be different from DST so we get a chance to filter out
      the clusters obtained from reclustering, i.e. read them from
      tmernode and copy only the relevant ones to mergednode/DST
      (the latter location being imposed by Fun4AllOutputManager).
  */
  EmcEmbedReclusterizer(const char* realnode = "REAL",
			const char* simunode = "SIMU",
			const char* mergednode = "TOP",
			const char* tmernode = "EMC",
			mEmcGeometryModule::ERealm geom = mEmcGeometryModule::kReal);

  int InitRun(PHCompositeNode*);

  int process_event(PHCompositeNode*);

 private:

  std::string fRealNode;
  std::string fSimuNode;
  std::string fMergedNode;
  std::string fTempMergedNode;
  mEmcGeometryModule::ERealm fGeometryType;
  SubsysReco* fClusterizer;

  static const float fgTowerThresholdPbSc;
  static const float fgTowerThresholdPbGl;
  static const float fgMinClusterEnergyPbSc;
  static const float fgMinClusterEnergyPbGl;
};
#endif
