#include <Fun4AllReturnCodes.h>
#include "mEmcRecoModuleSimulationYear1.h"
#include "PHCompositeNode.h"
#include "emcNodeHelper.h"

// Table wrappers
#include "dEmcRawDataWrapper.h"
#include "dEmcFEMDataWrapper.h"
#include "dEmcDCMDataWrapper.h"
#include "dEmcGeaTrackTowerWrapper.h"
#include "dEmcGeaTrackWrapper.h"
#include "dEmcGeaTowerTrackWrapper.h"
#include "dEmcGeaClusterTrackWrapper.h"
#include "dEmcGeaTrackClusterWrapper.h"
#include "dEmcGeaParamsWrapper.h"
#include "dEmcGeaHitWrapper.h"
#include "dEmcRespParWrapper.h"
#include "dEmcGeometryWrapper.h"
#include "dEmcEventWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "dEmcClusterLocalWrapper.h"
#include "dEmcCalibTowerWrapper.h"

#include "emcparWrapper.h"

// Modules
#include "mEmcDefGeomModule.h"    
#include "mEmcGeaParamsModule.h"  
#include "mEmcGeaMakeRawModule.h"
#include "mEmcGeaTrackModule.h"  
#include "mEmcDCMToRawModule.h"  
#include "mEmcGeaEventModule.h"  
#include "mEmcCalibTowerModule.h"
#include "mEmcGeaClusterEvalModule.h"
#include "mEmcRawToFEMModule.h"
#include "mEmcFEMToDCMModule.h"
#include "mEmcGeometryModule.h"
#include "mEmcClusterNewModule.h"
#include "mEmcGetDCMModule.h"
#include "mEmcPutDCMModule.h"

#include "PHFlag.h"

#include <cassert>
#include <cstdlib>

using namespace std;


const float mEmcRecoModuleSimulationYear1::fgPbScTowerThreshold = 0.003; // 3 MeV   
const float mEmcRecoModuleSimulationYear1::fgPbGlTowerThreshold = 0.014; // 14 MeV  
							      
const float mEmcRecoModuleSimulationYear1::fgPbScMinClusterEnergy = 0.015; // 15 MeV
const float mEmcRecoModuleSimulationYear1::fgPbGlMinClusterEnergy = 0.060; // 60 MeV

//_____________________________________________________________________________
mEmcRecoModuleSimulationYear1::mEmcRecoModuleSimulationYear1
(const PHFlag& flags):
  SubsysRecoStack("mEmcRecoModuleSimulationYear1")
{
  if ( flags.get_IntFlag("EVALUATIONFLAG") ) 
    {
      fEvaluation = true;
    }
  else
    {
      fEvaluation = false;
    }

  fSimulationFlag = flags.get_IntFlag("SIMULATIONFLAG");
  
  assert(fSimulationFlag>0 && fSimulationFlag<=3);
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear1::setupAna(PHCompositeNode* topNode)
{
  if ( fSimulationFlag == 1 ) 
    {
      // PRDF to DST
      push_back(new mEmcGetDCMModule);
      push_back(new mEmcDCMToRawModule);
      push_back(new mEmcDefGeomModule);
      push_back(new mEmcGeaEventModule);
      push_back(new mEmcGeaTrackModule);
      push_back(new mEmcCalibTowerModule);
      push_back(setupClustering(topNode));
    }
  else if ( fSimulationFlag == 2 )
    {
      // PISA to DST
      push_back(new mEmcGeaParamsModule);
      push_back(new mEmcGeaMakeRawModule);
      push_back(new mEmcGeaTrackModule);
      push_back(new mEmcCalibTowerModule);
      push_back(setupClustering(topNode));
    }
  else if ( fSimulationFlag == 3 ) 
    {
      // PISA to PRDF
      push_back(new mEmcGeaParamsModule);
      push_back(new mEmcGeaEventModule);
      push_back(new mEmcGeaMakeRawModule);
      push_back(new mEmcGeaTrackModule);
      push_back(new mEmcRawToFEMModule);
      push_back(new mEmcFEMToDCMModule);
      push_back(new mEmcPutDCMModule);
    }

  if ( fEvaluation ) 
    {
      push_back(new mEmcGeaClusterEvalModule);
    }

  cout << "mEmcRecoModuleSimulationYear1::setupAna : module list : "
       << endl;

  Print();
}

//_____________________________________________________________________________
int
mEmcRecoModuleSimulationYear1::InitRun(PHCompositeNode* topNode)
{
  //---- First thing is to check that we do have all our nodes there.

  emcNodeHelper nodeHelper;

  if ( !nodeHelper.makeDSTnodes(topNode) ) 
    {      
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Something went wrong while making EMC related nodes."
	   << endl;
      return ABORTRUN;
    }

  //---- Then make our tables/objects

  PHCompositeNode* emcNode = emcNodeHelper::findCompositeNode(topNode,"EMC");
  PHCompositeNode* dcmNode = emcNodeHelper::findCompositeNode(topNode,"DCM");
  PHCompositeNode* evaNode = emcNodeHelper::findCompositeNode(topNode,"EVA");
  PHCompositeNode* parNode = emcNodeHelper::findCompositeNode(topNode,"PAR");
  PHCompositeNode* geaNode = emcNodeHelper::findCompositeNode(topNode,"GEA");
  PHCompositeNode* dstNode = emcNodeHelper::findCompositeNode(topNode,"DST");

  assert(emcNode!=0);
  assert(dcmNode!=0);
  assert(evaNode!=0);
  assert(parNode!=0);
  assert(dstNode!=0);

  // Raw PISA Data
  emcNodeHelper::addTable<dEmcRawDataWrapper>(emcNode,"dEmcRawData",25000);
  // Simulated FEM Data
  emcNodeHelper::addTable<dEmcFEMDataWrapper>(emcNode,"dEmcFEMData",500);

  // Simulated DCM Data
  emcNodeHelper::addTable<dEmcDCMDataWrapper>(dcmNode,"dEmcDCMData",500);

  // Geant Track Tower
  emcNodeHelper::addTable<dEmcGeaTrackTowerWrapper>
    (evaNode,"dEmcGeaTrackTower",15000);
  // Geant Tower Track
  emcNodeHelper::addTable<dEmcGeaTowerTrackWrapper>
    (evaNode,"dEmcGeaTowerTrack",15000);
  // Geant Track
  emcNodeHelper::addTable<dEmcGeaTrackWrapper>(evaNode,"dEmcGeaTrack",7500);

  // Geant Parameters
  emcNodeHelper::addTable<dEmcGeaParamsWrapper>(parNode,"dEmcGeaParams",8);
  // Response parameters
  dEmcRespParWrapper* dEmcRespPar = 
    emcNodeHelper::addTable<dEmcRespParWrapper>(parNode,"dEmcRespPar",1);
  setupResponse(dEmcRespPar);

  // Geometry
  emcNodeHelper::addTable<dEmcGeometryWrapper>(parNode,"dEmcGeometry",30000);
  // Emc Parameters
  emcNodeHelper::addTable<emcparWrapper>(parNode,"emcpar",8);

  // Geant Hits
  bool transient = true;
  emcNodeHelper::addTable<dEmcGeaHitWrapper>
    (geaNode,"dEmcGeaHit",525000,transient);

  // EMC objects needed by clustering module

  emcNodeHelper::addTable<dEmcClusterLocalWrapper>
    (emcNode,"dEmcClusterLocal",30000);
  emcNodeHelper::addTable<dEmcEventWrapper>(emcNode,"dEmcEvent",1);

  // Output tables/objects

  emcNodeHelper::addTable<dEmcCalibTowerWrapper>
    (dstNode,"dEmcCalibTower",25000);
  emcNodeHelper::addTable<dEmcClusterLocalExtWrapper>
    (dstNode,"dEmcClusterLocalExt",25000);

  // Don't forget evaluation if requested...

  if ( fEvaluation ) 
    {
      setupEvaluation(topNode);
    }

  // Then go to setup the modules needed.

  setupAna(topNode);

  topNode->print();

  return 0;
}

//_____________________________________________________________________________
SubsysReco*
mEmcRecoModuleSimulationYear1::setupClustering(PHCompositeNode* topNode)
{
  
  // CGL need mEmcGeometry module to be in PAR node

  PHCompositeNode* parNode = 
    emcNodeHelper::findCompositeNode(topNode,"PAR");  
  mEmcGeometryModule* mEmcGeometry = 
    emcNodeHelper::addObject<mEmcGeometryModule>(parNode,"mEmcGeometry");
  
  mEmcClusterNewModule* mc = 
    new mEmcClusterNewModule(mEmcGeometry);
  
  // Setup clustering module.
  mc->SetTowerThresholdPbSc(fgPbScTowerThreshold);
  mc->SetTowerThresholdPbGl(fgPbGlTowerThreshold);
  mc->SetMinClusterEnergyPbSc(fgPbScMinClusterEnergy);
  mc->SetMinClusterEnergyPbGl(fgPbGlMinClusterEnergy);

  return mc;
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear1::setupEvaluation(PHCompositeNode* topNode)
{
  PHCompositeNode* evaNode = emcNodeHelper::findCompositeNode(topNode,"EVA");
  assert(evaNode!=0);

  emcNodeHelper::addTable<dEmcGeaTrackClusterWrapper>
    (evaNode,"dEmcGeaTrackCluster",7500);
  emcNodeHelper::addTable<dEmcGeaClusterTrackWrapper>
    (evaNode,"dEmcGeaClusterTrack",7500);
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear1::setupResponse(dEmcRespParWrapper* respPar)
{
  respPar->SetRowCount(1);
  respPar->set_anyset(0,1);
  respPar->set_sim_timing(0,0);  // reset to LED timing  May 23, 2001
  respPar->set_pbgl_response(0,3);
}
