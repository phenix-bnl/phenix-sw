#include "EmcEmbedChecker.h"

#include <Fun4AllServer.h>
#include <getClass.h>
#include <emcNodeHelper.h>
#include <emcTowerContainer.h>
#include <emcClusterContainer.h>
#include <EventHeader.h>
#include <VtxOut.h>
#include <PHGlobal.h>
#include <fkinDumper.h>

#include <cassert>
#include <iostream>

//_____________________________________________________________________________
EmcEmbedChecker::EmcEmbedChecker(const char* realnode /*="REAL" */,
				 const char* simunode /*="SIMU" */,
				 const char* mergednode /*="TOP"*/,
				 const char* tmernode /* ="EMC" */)
  : SubsysReco("EmcEmbedChecker"),
    fRealNode(realnode),
    fSimuNode(simunode),
    fMergedNode(mergednode),
    fTempMergedNode(tmernode)
{
}

//_____________________________________________________________________________
int
EmcEmbedChecker::process_event(PHCompositeNode*)
{
  Fun4AllServer* se = Fun4AllServer::instance();

  PHCompositeNode* mergedTopNode = se->topNode(fMergedNode.c_str());
  assert(mergedTopNode!=0);
  
  PHCompositeNode* realTopNode = se->topNode(fRealNode.c_str());
  assert(realTopNode!=0);

  PHCompositeNode* tempMergedNode = 
    emcNodeHelper::findCompositeNode(mergedTopNode,fTempMergedNode.c_str());
  assert(tempMergedNode!=0);

  PHCompositeNode* simuTopNode = se->topNode(fSimuNode.c_str());
  assert(simuTopNode!=0);
 
  emcTowerContainer* simuTowers = 
    findNode::getClass<emcTowerContainer>(simuTopNode,"emcTowerContainer");
  assert(simuTowers!=0);
 
  emcTowerContainer* realTowers = 
    findNode::getClass<emcTowerContainer>(realTopNode,"emcTowerContainer");
  assert(realTowers!=0);

  emcTowerContainer* mergedTowers = 
    findNode::getClass<emcTowerContainer>(mergedTopNode,"emcTowerContainer");
  assert(mergedTowers!=0);

  EventHeader* evt =
    findNode::getClass<EventHeader>(realTopNode,"EventHeader");
  assert(evt!=0);

  std::cout << std::string(80,'-') << std::endl;
  std::cout << PHWHERE << " : EVENT " << evt->get_EvtSequence()
	    << " TYPE " << evt->get_EvtType()
	    << std::endl;

  VtxOut* simuVertex =
    findNode::getClass<VtxOut>(simuTopNode,"VtxOut");
  assert(simuVertex!=0);

  PHGlobal* realVertex =
    findNode::getClass<PHGlobal>(realTopNode,"PHGlobal");
  assert(realVertex!=0);
 
  PHGlobal* mergedVertex =
    findNode::getClass<PHGlobal>(mergedTopNode,"PHGlobal");
  assert(mergedVertex!=0);

  std::cout << "VERTICES : simu : " << simuVertex->get_ZVertex()
	    << " real : " << realVertex->getZVertex()
	    << " merged : " << mergedVertex->getZVertex()
	    << std::endl;

  std::cout << "NUMBER OF TOWERS : simu : " << simuTowers->size()
	    << " real : " << realTowers->size()
	    << " merged : " << mergedTowers->size()
	    << " diff : " 
	    << static_cast<int>(mergedTowers->size()-realTowers->size()-simuTowers->size())
	    << std::endl;

  emcClusterContainer* simuClusters = 
    findNode::getClass<emcClusterContainer>(simuTopNode,"emcClusterContainer");
  assert(simuClusters!=0);

  emcClusterContainer* realClusters = 
    findNode::getClass<emcClusterContainer>(realTopNode,"emcClusterContainer");
  assert(realClusters!=0);

  emcClusterContainer* mergedClusters = 
    findNode::getClass<emcClusterContainer>(tempMergedNode,"emcClusterContainer");
  assert(mergedClusters!=0);

  emcClusterContainer* outClusters = 
    findNode::getClass<emcClusterContainer>(mergedTopNode,"emcClusterContainer");
  assert(outClusters!=0);

  emcClusterContainer* outsimClusters = 
    findNode::getClass<emcClusterContainer>(mergedTopNode,"emcSimClusterContainer");
  assert(outsimClusters!=0);


  std::cout << "NUMBER OF CLUSTERS : simu : " << simuClusters->size()
	    << " real : " << realClusters->size()
	    << " merged : " << mergedClusters->size()
	    << " diff : " 
	    << static_cast<int>(mergedClusters->size()-realClusters->size()-simuClusters->size())
	    << " output : " << outClusters->size()
	    << " output sim : " << outsimClusters->size()
	    << std::endl;

  if ( verbosity > 0 )
    {
      std::cout << "outClusters=" << std::endl;
      outClusters->print();
      
      std::cout << "outsimClusters=" << std::endl;
      outsimClusters->print();
   
      primaryWrapper* primary = findNode::getClass<primaryWrapper>(simuTopNode,"primary");
      assert(primary!=0);
      fkinDumper::dump(*primary,"PRIMARY SIMU");
      
      fkinWrapper* fkin = findNode::getClass<fkinWrapper>(simuTopNode,"fkin");
      assert(fkin!=0);
      fkinDumper::dump(*fkin,"FKIN SIMU");
      
      primaryWrapper* primary_m = findNode::getClass<primaryWrapper>(mergedTopNode,"primary");
      assert(primary_m!=0);
      fkinDumper::dump(*primary_m,"PRIMARY MERGED");
      
      fkinWrapper* fkin_m = findNode::getClass<fkinWrapper>(mergedTopNode,"fkin");
      assert(fkin_m!=0);
      fkinDumper::dump(*fkin_m,"FKIN MERGED");
    }

  return 0;
}
