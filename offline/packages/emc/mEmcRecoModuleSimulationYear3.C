#include <Fun4AllReturnCodes.h>
#include "mEmcRecoModuleSimulationYear3.h"

#include "PHCompositeNode.h"
#include "PHFlag.h"

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
#include "emcparWrapper.h"

// Modules
#include "mEmcApplyQAToSimu.h"
#include "mEmcClusterizerv0.h"
#include "mEmcDCMToRawModule.h"  
#include "mEmcDefGeomModule.h"    
#include "mEmcFEMToDCMModule.h"
#include "mEmcGeaEventModule.h"  
#include "mEmcGeaGetHits.h"
#include "mEmcGeaMakeCalibTower.h"
#include "mEmcGeaMakeClusterEvaluation.h"
#include "mEmcGeaParamsModule.h"  
#include "mEmcGeaMakeRawModule.h"
#include "mEmcGeaPatchSimulatedCluster.h"
#include "mEmcGeaTrackModule.h"  
#include "mEmcGeometryModule.h"
#include "mEmcGetDCMModule.h"
#include "mEmcPRDFToRawModule.h"
#include "mEmcPutDCMLongModule.h"
#include "mEmcRawToFEMModule.h"
#include "mEmcToolsModule.h"
#include "mEmcMaskDeadTowers.h"

// Somehow related to database(s)
#include "emcBadModulesv1.h"
#include "emcDataManager.h"
#include "emcTimeStamp.h"

#include "emcNodeHelper.h"
#include "emcClusterContainerv1S.h"
#include "emcTowerContainerv1S.h"
#include "recoConsts.h"
#include "emcDBMS.h"

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <memory>

using namespace std;

const float mEmcRecoModuleSimulationYear3::fgTowerThresholdPbSc = 0.010;
const float mEmcRecoModuleSimulationYear3::fgTowerThresholdPbGl = 0.014;  
const float mEmcRecoModuleSimulationYear3::fgMinClusterEnergyPbSc = 0.015;
const float mEmcRecoModuleSimulationYear3::fgMinClusterEnergyPbGl = 0.060;

//_____________________________________________________________________________
mEmcRecoModuleSimulationYear3::mEmcRecoModuleSimulationYear3
(const PHFlag& flags):
  SubsysRecoStack("mEmcRecoModuleSimulationYear3")
{ 
  const recoConsts* rc = dynamic_cast<const recoConsts*>(&flags);
  assert(rc!=0);

  if ( flags.FlagExist("EVALUATIONFLAG") && 
       flags.get_IntFlag("EVALUATIONFLAG") ) 
    {
      fEvaluation = true;
    }
  else
    {
      fEvaluation = false;
    }

  fSimulationFlag = flags.get_IntFlag("SIMULATIONFLAG");
  
  assert(fSimulationFlag==2);

  int runnumber = 0;

  if ( flags.FlagExist("RUNNUMBER") )
    {
      runnumber = flags.get_IntFlag("RUNNUMBER");
    }

  fDataSource = emcDBMS::get();

  if ( flags.FlagExist("EMCDATASOURCE") )
    {
      emcManageable::EStorage ds = 
	emcManageable::GetStorage(flags.get_CharFlag("EMCDATASOURCE"));
      if ( ds == emcManageable::kNone )
	{
	  cerr << "mEmcRecoModuleSimulationYear3::"
	       << "mEmcRecoModuleSimulationYear3 "
	       << "Flag EMCDATASOURCE=" << flags.get_CharFlag("EMCDATASOURCE")
	       << " is not valid. Using default=" 
	       << emcManageable::GetStorageName(fDataSource)
	       << endl;
	}
      else
	{
	  fDataSource = ds;
	}
    }      

  emcTimeStamp ets;
  ets.SetSource(fDataSource);

  emcDataManager* dm = emcDataManager::GetInstance();

  bool ok = dm->Read(ets,runnumber);

  assert(ok==true);

  fTimeStamp = ets.getTimeStamp();

  std::cout << "<I> mEmcRecoModuleSimulationYear3 : Using TimeStamp: ";
  fTimeStamp.print();
  std::cout << std::endl;

  PHTimeStamp y1999(1999,1,1,0,0,0);

  assert( fTimeStamp > y1999 );
}

//_____________________________________________________________________________
int
mEmcRecoModuleSimulationYear3::InitRun(PHCompositeNode* topNode)
{
  //---- First thing is to check that we do have all our nodes there.

  const int N = 1;

  const char* nodes[] = { "EMC" } ; //{"DST","PAR","DCM","EVA","GEA","EMC" };

  for ( int i = 0; i < N; ++i )
    {
      if ( !emcNodeHelper::makeCompositeNode(topNode,nodes[i],"-p") )
	{
	  return i+1;
	}
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

  // Output objects (see also setupClustering)

  emcNodeHelper::addObject<emcTowerContainerv1S>(dstNode,"emcTowerContainer");

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
mEmcRecoModuleSimulationYear3::setupAna(PHCompositeNode* topNode)
{
  assert(fSimulationFlag==2);

  // PISA to DST
  push_back(new mEmcGeaGetHits);
  push_back(new mEmcGeaParamsModule);
  push_back(new mEmcGeaEventModule);
  push_back(new mEmcGeaMakeRawModule);
  push_back(new mEmcGeaTrackModule);
  
  // LVL2 Simulations requires this
  // Level 2 needs DCM tables to be filled
  // push_back(new mEmcRawToLongModule);
  // push_back(new mEmcPutDCMLongModule("dEmcRawDataReCal","dEmcDCMLongDataReCal"));
  // push_back(new EmcSimuRawDataRecal);
  // push_back(new mEmcPutDCMLongReCalModule);
  
  // The "fake" calibrator module
  push_back(new mEmcGeaMakeCalibTower);

  // Apply the real (corresponding to RUNNUMBER) deadmap to simulated towers.
  push_back(new mEmcApplyQAToSimu);

  // Apply additional deadmap from file to simulated towers if called by the macro. Cesar
  recoConsts *rc = recoConsts::instance();
  if (rc->FlagExist("EMCDEADRECALDATASOURCE"))
    push_back(new mEmcMaskDeadTowers);

  setupClustering(topNode);

  // Little module to patch the clusters, e.g. to indicate that SimFrac=1
  // (i.e. 100%), or any other simulation specific thing you'd like
  // to do
  push_back(new mEmcGeaPatchSimulatedCluster);

  if ( fEvaluation ) 
    {
      push_back(new mEmcGeaMakeClusterEvaluation);
    }

  cout << "mEmcRecoModuleSimulationYear3::setupAna : module list : "
       << endl;

  Print();
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear3::setupBadModules(PHCompositeNode* topNode)
{
  bool init = true; // init all bad modules now.
  emcBadModules::EInformationOrigin origin = emcBadModules::kAll; // read both online QA & offline rejectlist.

  emcBadModules* bm = new emcBadModulesv1(fTimeStamp,origin,
					  fDataSource,init);

  PHCompositeNode* emcNode = 
    emcNodeHelper::findCompositeNode(topNode,"EMC");  
  
  assert(emcNode!=0);
  emcNodeHelper::insertObject<emcBadModules>(emcNode,bm,"emcBadModules");
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear3::setupClustering(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  PHCompositeNode* dstNode = nh.findCompositeNode(topNode,"DST");

  nh.addObject<emcClusterContainerv1S>(dstNode,"emcClusterContainer");

  PHCompositeNode* parNode = nh.findCompositeNode(topNode,"PAR");

  mEmcGeometryModule* geometryModule = 
    new mEmcGeometryModule(mEmcGeometryModule::kPISA);
  PHIODataNode<TObject>* geometryNode 
    = new PHIODataNode<TObject>(geometryModule, "mEmcGeometry");
  parNode->addNode(geometryNode);

  mEmcClusterizerv0* clusterizer = new mEmcClusterizerv0(geometryModule);
  
  clusterizer->SetTowerThresholdPbSc(fgTowerThresholdPbSc);
  clusterizer->SetTowerThresholdPbGl(fgTowerThresholdPbGl);
  clusterizer->SetMinClusterEnergyPbSc(fgMinClusterEnergyPbSc);
  clusterizer->SetMinClusterEnergyPbGl(fgMinClusterEnergyPbGl);

  push_back(clusterizer);
}

//_____________________________________________________________________________
void
mEmcRecoModuleSimulationYear3::setupEvaluation(PHCompositeNode* topNode)
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
mEmcRecoModuleSimulationYear3::setupResponse(dEmcRespParWrapper* respPar)
{
  respPar->SetRowCount(1);
  respPar->set_anyset(0,1);
  respPar->set_sim_timing(0,0);  // reset to LED timing  May 23, 2001
  respPar->set_pbgl_response(0,3);
}
