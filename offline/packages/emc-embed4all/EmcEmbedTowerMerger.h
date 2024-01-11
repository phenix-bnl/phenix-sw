#ifndef __EMCEMBEDTOWERMERGER_H__
#define __EMCEMBEDTOWERMERGER_H__

#include "SubsysReco.h"
#include <vector>
#include <string>

/** Merge EMCAL emcTowerContainers from 2 different topNodes.
 */

class emcTowerContent;
class emcTowerContainer;

class EmcEmbedTowerMerger : public SubsysReco
{
 public:
  /** 1 and 2 are the input topNodes, and NodeMerged the
      node where the resulting merged towerContainer will be.
  */
  EmcEmbedTowerMerger(const char* topNode1Name="REAL", 
		      const char* topNode2Name="SIMU",
		      const char* topNodeMergedName="TOP");

  /// Insure that there is an emcTowerContainer in mergedNode.
  int InitRun(PHCompositeNode*);

  /** All 3 nodes must contain an emcTowerContainer node prior
      to the call to this method.
      The one in topNodeMergedName will be reset and refill
      at each event.
  */
  int process_event(PHCompositeNode*);

  /** Copy source to destination.
      @return false if destination is incompatible with source.
      For a definition of imcompatible see the .C file.
  */
  bool copy(const emcTowerContent& source, emcTowerContent& destination);

  /** Copy source to destination.
      @return false if destination is incompatible with source.
      Uses copy(const emcTowerContent&, emcTowerContent);
  */
  bool copy(const emcTowerContainer& source, emcTowerContainer& destination);


 public:

  void merge(const emcTowerContainer& input1,
	     const emcTowerContainer& input2,
	     emcTowerContainer& result);

  void merge(const emcTowerContent& t1, 
	     emcTowerContent& t2);

 private:
  std::vector<std::string> fTopNodeNames;
};
#endif
