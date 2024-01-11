#include "mEmcRecoModuleSimulationYear2.h"
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
#include "mEmcPutDCMLongModule.h"
#include "mEmcRawToLongModulev1.h"
#include "mEmcPRDFToRawModule.h"
#include "mEmcToolsModule.h"



#include "PHFlag.h"

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <memory>

using namespace std;


const float mEmcRecoModuleSimulationYear2::fgPbScTowerThreshold = 0.010; // 10 MeV  
const float mEmcRecoModuleSimulationYear2::fgPbGlTowerThreshold = 0.014; // 14 MeV  
							      
const float mEmcRecoModuleSimulationYear2::fgPbScMinClusterEnergy = 0.015; // 15 MeV
const float mEmcRecoModuleSimulationYear2::fgPbGlMinClusterEnergy = 0.060; // 60 MeV

//_____________________________________________________________________________
mEmcRecoModuleSimulationYear2::mEmcRecoModuleSimulationYear2
(const PHFlag& flags)
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

  if ( fSimulationFlag == 3 && fEvaluation ) 
    {
      cerr << "mEmcRecoModuleSimulationYear2::ctor : "
	   << "<INFO> Cannot do evaluation in PISA to PRDF mode. (you set"
	   << " EVALUATIONFLAG to 1 and asked for SIMULATIONFLAG=3, which has"
	   << " no sense). Thus I will *not* try to do EMCAL evaluation."
	   << endl;
      fEvaluation=false;
    }

  if ( flags.get_IntFlag("PPFLAG") ) 
    {
      fPP = true;
    }
  else
    {
      fPP = false;
    }

  // Insure we use the same time stamp as the one blessed by the framework
  fTimeStamp = rc->get_TimeStamp();

  PHTimeStamp y1999(1999,1,1,0,0,0);

  assert( fTimeStamp > y1999 );
}

//_____________________________________________________________________________
int
mEmcRecoModuleSimulationYear2::ana(PHCompositeNode* topNode)
{
  return fModules.event(topNode);
}

//_____________________________________________________________________________
int
mEmcRecoModuleSimulationYear2::end(PHCompositeNode* topNode)
{
  return 0;
}

//_____________________________________________________________________________
int
mEmcRecoModuleSimulationYear2::setup(PHCompositeNode* topNode)
{
  //---- First thing is to check that we do have all our nodes there.

  emcNodeHelper nodeHelper;

  if ( !nodeHelper.makeDSTnodes(topNode) ) 
    {      
      cerr << __FILE__ << ":" << __LINE__ 
	   << " Something went wrong while making EMC related nodes."
	   << endl;
      return -1;
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

  // Collect Q&A information to be applied to simulated data

  setupBadModules(topNode);

  // Then go to setup the modules needed.

  setupAna(topNode);

  topNode->print();

  return 0;
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear2::setupAna(PHCompositeNode* topNode)
{
  if ( fSimulationFlag == 1 ) 
    {
      // PRDF to DST
      fModules.add(new mEmcPRDFToRawModule);
      fModules.add(new mEmcDefGeomModule);
      fModules.add(new mEmcGeaEventModule);
      fModules.add(new mEmcCalibTowerModule);
      mEmcToolsModule* tools = mEmcToolsModule::instance();
      fModules.add(tools);
      fModules.add(setupClustering(topNode));
    }
  else if ( fSimulationFlag == 2 )
    {
      // PISA to DST
      fModules.add(new mEmcGeaParamsModule,emcModuleHelper::kFirstEventOnly); 
      fModules.add(new mEmcGeaEventModule);
      fModules.add(new mEmcGeaMakeRawModule);
      fModules.add(new mEmcGeaTrackModule);

      // LVL2 Simulations requires this
      // Level 2 needs DCM tables to be filled
      // fModules.add(new mEmcRawToLongModule);
      // fModules.add(new mEmcPutDCMLongModule("dEmcRawDataReCal","dEmcDCMLongDataReCal"));
      // fModules.add(new EmcSimuRawDataRecal);
      // fModules.add(new mEmcPutDCMLongReCalModule);

      fModules.add(new mEmcCalibTowerModule);
      fModules.add(new mEmcApplyQAToSimu);
      fModules.add(setupClustering(topNode));
    }
  else if ( fSimulationFlag == 3 ) 
    {
      // PISA to PRDF
      fModules.add(new mEmcGeaParamsModule,emcModuleHelper::kFirstEventOnly); 
      fModules.add(new mEmcGeaMakeRawModule);
      fModules.add(new mEmcGeaTrackModule);
      fModules.add(new mEmcRawToLongModulev1);
      fModules.add(new mEmcPutDCMLongModule);
    }

  if ( fEvaluation ) 
    {
      fModules.add(new mEmcGeaClusterEvalModule);
    }

  cout << "mEmcRecoModuleSimulationYear2::setupAna : module list : "
       << endl;

  fModules.print();
}

//_____________________________________________________________________________
EMCModule*
mEmcRecoModuleSimulationYear2::setupClustering(PHCompositeNode* topNode)
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
mEmcRecoModuleSimulationYear2::setupEvaluation(PHCompositeNode* topNode)
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
mEmcRecoModuleSimulationYear2::setupResponse(dEmcRespParWrapper* respPar)
{
  respPar->SetRowCount(1);
  respPar->set_anyset(0,1);
  respPar->set_sim_timing(0,0);  // reset to LED timing  May 23, 2001
  respPar->set_pbgl_response(0,3);
}
