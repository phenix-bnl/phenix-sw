#include "EmcEmbedMergedClusterUpdater.h"
#include <iostream>
#include <cassert>
//INCLUDECHECKER: Removed this line: #include "PHCompositeNode.h"

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"

#include "emcNodeHelper.h"

//_____________________________________________________________________________
EmcEmbedMergedClusterUpdater::EmcEmbedMergedClusterUpdater
(const char* mergedNode,
 const char* tmernode)
  : SubsysReco("EmcEmbedMergedClusterUpdater"),
    fMergedNodeName(mergedNode),
    fTempMergedNode(tmernode)
{}

//_____________________________________________________________________________
int 
EmcEmbedMergedClusterUpdater::InitRun(PHCompositeNode* )
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* topNode = se->topNode(fMergedNodeName.c_str());

  if (!topNode)
    {
      std::cerr << PHWHERE << " Could not find my working topNode="
		<< fMergedNodeName << std::endl;
      return ABORTRUN;
    }

  return 0;
}

//_____________________________________________________________________________
int 
EmcEmbedMergedClusterUpdater::process_event(PHCompositeNode* )
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNodeName.c_str());
  assert(mergedTopNode!=0);

  // WARNING : we use here the fTempMergedNode node, not the DST one.
  PHCompositeNode* tmerNode = 
    emcNodeHelper::findCompositeNode(mergedTopNode,fTempMergedNode.c_str());

  emcClusterContainer* clusters =
    findNode::getClass<emcClusterContainer>(tmerNode, "emcClusterContainer");

  if (!clusters)
    {
      std::cerr << PHWHERE << " No emcClusterContainer object !"
		<< std::endl;
      return 0;
    }

  emcTowerContainer* towers =
    findNode::getClass<emcTowerContainer>(tmerNode, "emcTowerContainer");

  if (!towers)
    {
      std::cerr << PHWHERE << " No emcTowerContainer object !"
		<< std::endl;
      return 0;
    }

  size_t clussize = clusters->size();

  for ( size_t i = 0; i < clussize ; ++i )
    {
      emcClusterContent* clus = clusters->getCluster(i);
      assert(clus != 0);

      float energy = 0;
      float simEnergy = 0;

      for ( int index = 0; index < clus->multiplicity(); ++index )
        {
          int towerID = clus->towerid(index);
          emcTowerContent *t = towers->findTower(towerID);
          assert(t != 0);

          energy += t->Energy();
          simEnergy += t->SimFrac() * t->Energy();
        }

      if (energy != 0)
        {
          float frac = simEnergy / energy;
          clus->set_simfrac(frac);
        }
    }

  return 0;
}




