#include "EmcEmbedReclusterizer.h"

#include <cassert>
//INCLUDECHECKER: Removed this line: #include <iostream>
//INCLUDECHECKER: Removed this line: #include <memory>

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "emcNodeHelper.h"
#include "emcTowerContainer.h"
#include "mEmcClusterizerv0.h"
#include "VtxOut.h"


const float EmcEmbedReclusterizer::fgTowerThresholdPbSc = 0.010;
const float EmcEmbedReclusterizer::fgTowerThresholdPbGl = 0.014;  
const float EmcEmbedReclusterizer::fgMinClusterEnergyPbSc = 0.015;
const float EmcEmbedReclusterizer::fgMinClusterEnergyPbGl = 0.060;

//_____________________________________________________________________________
EmcEmbedReclusterizer::EmcEmbedReclusterizer(const char* realnode /*="REAL" */,
					     const char* simunode /*="SIMU" */,
					     const char* mergednode /*="TOP"*/,
					     const char* tmernode /* ="EMC" */,
					     mEmcGeometryModule::ERealm geom)
  : SubsysReco("EmcEmbedReclusterizer"),
    fRealNode(realnode),
    fSimuNode(simunode),
    fMergedNode(mergednode),
    fTempMergedNode(tmernode),
    fGeometryType(geom),
    fClusterizer(0)
{
}

//_____________________________________________________________________________
int
EmcEmbedReclusterizer::InitRun(PHCompositeNode*)
{
  // To be able to work, we need:
  // a) emcal towers
  // b) a vertex object
  // c) a clustering module (initialized with geometry)

  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode!=0);

  emcTowerContainer* towers = 
    findNode::getClass<emcTowerContainer>(mergedTopNode,"emcTowerContainer");
  assert(towers!=0);

  VtxOut* vmerged = findNode::getClass<VtxOut>(mergedTopNode,"VtxOut");
  assert(vmerged!=0);

  delete fClusterizer;

  std::auto_ptr<mEmcGeometryModule> geom(new mEmcGeometryModule(fGeometryType));

  mEmcClusterizerv0* clusterizer = new mEmcClusterizerv0(geom.get());

  clusterizer->SetTowerThresholdPbSc(fgTowerThresholdPbSc);
  clusterizer->SetTowerThresholdPbGl(fgTowerThresholdPbGl);
  clusterizer->SetMinClusterEnergyPbSc(fgMinClusterEnergyPbSc);
  clusterizer->SetMinClusterEnergyPbGl(fgMinClusterEnergyPbGl);
 
  fClusterizer = clusterizer;

  return 0;
}

//_____________________________________________________________________________
int
EmcEmbedReclusterizer::process_event(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode!=0);
  
  // WARNING : if fTempMergedNode is specified we use here that node, not the DST one.

  PHCompositeNode* tmerNode = mergedTopNode;
  
  if( fTempMergedNode != "" )
    tmerNode = emcNodeHelper::findCompositeNode(mergedTopNode,fTempMergedNode.c_str());
  
  return fClusterizer->process_event(tmerNode);
}
