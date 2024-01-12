#include <ReadbackCompactPWG.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>
#include <EmcClusterContainerResurrector.h>
#include "emcNodeHelper.h"
#include "emcClusterContainerv6.h"
#include <iostream>
#include <getClass.h>

typedef PHIODataNode<PHObject> PHObjectNode_t;

ReadbackCompactPWG::ReadbackCompactPWG(const std::string& name) : Recalibrator(name) {
  baseclasses.insert("emcTowerContainerDST");
  fEmcClusterContainerResurrector = new EmcClusterContainerResurrector();
}

ReadbackCompactPWG::~ReadbackCompactPWG(){
  delete fEmcClusterContainerResurrector;
}

int ReadbackCompactPWG::Init(PHCompositeNode *topNode)
{
  // Create the emcClusterContainer node here, so that later recalibratots
  // that specify it as their base class will be registered

  // Find the DST tree and make the emcClusterContainer
  emcClusterContainer *emcclus_exists = findNode::getClass<emcClusterContainerv6>(topNode, "emcClusterContainer");

  if(!emcclus_exists)
    {
      PHNodeIterator iter(topNode);
      PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

      emcClusterContainerv6 *emc = new emcClusterContainerv6();

      // Make node
      PHObjectNode_t *recal_emc_node = new PHObjectNode_t(emc, "emcClusterContainer", "PHObject");

      // Put the node in the tree below "DST"...
      dstNode->addNode(recal_emc_node);

      std::cout << PHWHERE << "      Added emcClusterContainer node to node tree " << std::endl;
    }

  return EVENT_OK;

/*
  emcNodeHelper nh;

  PHCompositeNode* dstNode =  nh.findCompositeNode(topNode,"DST");

  nh.addObject<emcClusterContainerv6>(dstNode,"emcClusterContainer");

  return EVENT_OK;
*/
}

int ReadbackCompactPWG::isValidRun(const int irun) const {
  //valid for Run-10 right now
  if(irun >= BEGIN_OF_RUN10)
    return 1;
  return 0;
}

int ReadbackCompactPWG::InitRun(PHCompositeNode *topNode){
  return fEmcClusterContainerResurrector->InitRun(topNode);
}

int ReadbackCompactPWG::process_event(PHCompositeNode *topNode){
  return fEmcClusterContainerResurrector->process_event(topNode);
}
