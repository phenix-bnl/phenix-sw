#include "GlobalReco_central.h"

#include "PHGlobalv10.h"
#include "PHGlobalv11.h"
#include "PHGlobal_Centralv1.h"
#include "PHGlobal_Muonv1.h"

#include "Bbc.hh"
#include "BbcOut.h"
#include "CrkHit.h"
#include "DchTrack.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "PadCluster.h"
#include "SmdOut.h"
#include "TecOut.hh"
#include "TofOut.h"
#include "Zdc.hh"
#include "ZdcOut.h"

// new muon framework maps
#include "TMuiHitMapO.h"
#include "TMutHitMap.h"

#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"

#include <iostream>

using namespace std;

//________________________________________________________________
GlobalReco_central::GlobalReco_central( const string &name ):
  SubsysReco(name)
{ return; }

//________________________________________________________________
int GlobalReco_central::InitRun(PHCompositeNode *topNode)
{
  
  // look for existing PHGlobal from DST node. Create one otherwise
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  
  // create the central arm PHGlobal only if a DchTrack Node is around
  PHGlobal_Central *global_central = findNode::getClass<PHGlobal_Central>(dstNode, "PHGlobal_CENTRAL");
  if ( !global_central && findNode::getClass<DchTrack>(topNode, "DchTrack"))
  {
    global_central = new PHGlobal_Centralv1();
    PHIODataNode<PHObject> *PHGlobalNode = new PHIODataNode<PHObject>(global_central, "PHGlobal_CENTRAL", "PHObject");
    dstNode->addNode(PHGlobalNode);
  }
  
  return 0;
}

//________________________________________________________________
int
  GlobalReco_central::process_event(PHCompositeNode *topNode)
{

  // Fill global information in Central arm
  fillCentral(topNode);  
  return EVENT_OK;
  
}

//________________________________________________________________
int GlobalReco_central::fillCentral(PHCompositeNode *topNode)
{
  
  PHGlobal_Central *global_central = findNode::getClass<PHGlobal_Central>(topNode, "PHGlobal_CENTRAL");
  
  // Sometimes there is no need for a global central (eg, in p+p and in many simulations)
  if ( global_central==0 ) return 0;
  
  // Crk Hits
  CrkHit *crk = findNode::getClass<CrkHit>(topNode, "CrkHit");
  if (crk)
  {
    global_central->setNumberCerenkovHits( crk->get_CrkNHit() );
  }
  
  // Dch Tracks
  DchTrack *dchtrack = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (dchtrack)
  {
    global_central->setNumberDchTracks(dchtrack->get_DchNTrack());
  }
  
  // Emc Clusters and Energy
  emcClusterContainer *emc = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  if (emc)
  {
    global_central->setNumberEmcClusters( emc->size() );
    float energyEast = 0;
    float energyWest = 0;
    for (unsigned int iclus = 0; iclus < emc->size(); iclus++)
    {
      emcClusterContent* clus = emc->getCluster(iclus);
      int arm = clus->arm();
      if (arm == 1)
      {
        energyEast += clus->e();
      }
      else
      {
        energyWest += clus->e();
      }
    }
    global_central->setEmcEnergyEW( energyEast, energyWest );
  }
  
  // Pad Chamber hits
  PadCluster *pc1 = findNode::getClass<PadCluster>(topNode, "Pc1Cluster");
  if (pc1)
  {
    global_central->setNumberPC1Hits(pc1->get_PadNCluster());
  }
  PadCluster *pc2 = findNode::getClass<PadCluster>(topNode, "Pc2Cluster");
  if (pc2)
  {
    global_central->setNumberPC2Hits(pc2->get_PadNCluster());
  }
  PadCluster *pc3 = findNode::getClass<PadCluster>(topNode, "Pc3Cluster");
  if (pc3)
  {
    global_central->setNumberPC3Hits(pc3->get_PadNCluster());
  }
  
  // Tec Tracks
  TecOut *tec = findNode::getClass<TecOut>(topNode, "TecOut");
  if (tec)
  {
    global_central->setNumberTecTracks(tec->get_TecNTrack());
  }
  // Tof Hits
  TofOut *tof = findNode::getClass<TofOut>(topNode, "TofOut");
  if (tof)
  {
    global_central->setNumberTofHits( tof->get_TofNHit() );
  }
  
  
  return EVENT_OK;
}
