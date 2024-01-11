//////////////////////////////////////////////////////////////////////////////////
//
// simulation only reco module for new emcal simulation code. 
// based on mEmcRecoModuleSimulationYear3
//
//////////////////////////////////////////////////////////////////////////////////


#include <phool.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>

#include <emcGeaRawDataContainer.h>
#include <emcGeaDepositContainer.h>
#include <emcGeaTrackContainervA.h>
#include <emcGeaTowerContainervA.h>
#include <emcGeaClusterContainervA.h>

#include <emcNodeHelper.h>
#include <mEmcGeometryModule.h>
#include <EmcGeaParamsSimMaker.h>
#include <EmcGeaPisa2GeaEdep.h>
#include <EmcGeaRawDataSimMaker.h>
#include <EmcGeaTrackSimMaker.h>
#include <EmcGeaTowerSimMaker.h>
#include <EmcApplyQA.h>
#include <mEmcGeometryModule.h>
#include <mEmcClusterizerv0.h>

#include <EmcRecoModuleSimulationYear8.h>



ClassImp(EmcRecoModuleSimulationYear8);



namespace {

  const static float fgTowerThresholdPbSc = 0.010;
  const static float fgTowerThresholdPbGl = 0.014;  
  const static float fgMinClusterEnergyPbSc = 0.015;
  const static float fgMinClusterEnergyPbGl = 0.060;

}







EmcRecoModuleSimulationYear8::EmcRecoModuleSimulationYear8(): SubsysRecoStack("EmcRecoModuleSimulationYear8"){
  std::string ss = ""; // to make PHMESSAGE()'ing easier


  // get config data
  recoConsts *rc = recoConsts::instance();
  assert( rc != 0 );


  // for compatibility reasons
  if ( rc->FlagExist("EVALUATIONFLAG") && rc->get_IntFlag("EVALUATIONFLAG") )
    PHMESSAGE("EVALUATIONFLAG set: evaluaton is done at runtime in emc simulation v2.");


  // for compatibility reasons
  assert( rc->get_IntFlag("SIMULATIONFLAG") == 2 );



  // fModules->add(new mEmcGeaParamsModule,emcModuleHelper::kFirstEventOnly); 
  push_back( new EmcGeaParamsSimMaker() ); 

  mEmcGeometryModule * geometryModule = new mEmcGeometryModule( mEmcGeometryModule::kPISA );
  //  std::cout << "geometry @ " << geometry << std::endl;
  //  std::cout << "geometry   " << geometry->Name() << std::endl;
  push_back( geometryModule );

  // fModules->add(new mEmcGeaGetHits);
  // fModules->add(new mEmcGeaEventModule);
  push_back( new EmcGeaPisa2GeaEdep() );

  // fModules->add(new mEmcGeaMakeRawModule);
  EmcGeaRawDataSimMaker * rawdatamaker = new EmcGeaRawDataSimMaker();
  rawdatamaker->SetPbScTiming( EmcGeaRawDataSimMaker::PBSC_TIMING_LED );   // reset to LED timing  May 23, 2001
  rawdatamaker->SetPbGlTiming( EmcGeaRawDataSimMaker::PBGL_TIMING_CHERENKOV ); // cherenkov photons, no attenuation
  push_back( rawdatamaker );

  // fModules->add(new mEmcGeaTrackModule);
  push_back( new EmcGeaTrackSimMaker() );

  // The "fake" calibrator module
  // fModules->add(new mEmcGeaMakeCalibTower);
  float lowgain = rc->get_FloatFlag("EMCTOWERLOWGAIN", 0.001);
  float highgain = rc->get_FloatFlag("EMCTOWERHIGHGAIN", 0.008);
  push_back( new EmcGeaTowerSimMaker(lowgain, highgain) );

  // Apply the real (corresponding to RUNNUMBER) deadmap to simulated towers.
  int runno = rc->FlagExist("RUNNUMBER") ? rc->get_IntFlag("RUNNUMBER") : 0;
  if( !rc->FlagExist("EMCSIMULATIONV2NOQA") )
    push_back( new EmcApplyQA(EmcApplyQA::TOWER, runno) );

  // set up clusterizer module
  mEmcClusterizerv0* clusterizer = new mEmcClusterizerv0( geometryModule );
  clusterizer->SetTowerThresholdPbSc(fgTowerThresholdPbSc);
  clusterizer->SetTowerThresholdPbGl(fgTowerThresholdPbGl);
  clusterizer->SetMinClusterEnergyPbSc(fgMinClusterEnergyPbSc);
  clusterizer->SetMinClusterEnergyPbGl(fgMinClusterEnergyPbGl);
  push_back(clusterizer);



  PHMESSAGE("module list:");
  Print();
}



EmcRecoModuleSimulationYear8::~EmcRecoModuleSimulationYear8(){
}



int EmcRecoModuleSimulationYear8::Init(PHCompositeNode * root){


  //---- First thing is to check that we do have all our nodes there.

  if ( !emcNodeHelper::makeCompositeNode(root,"EMC","-p") )
    return ABORTRUN;

  //---- Then make our tables/objects

  PHCompositeNode* emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode* dcmnode = emcNodeHelper::findCompositeNode(root, "DCM"); EMCNODEASSERT( dcmnode );
  PHCompositeNode* evanode = emcNodeHelper::findCompositeNode(root, "EVA"); EMCNODEASSERT( evanode );
  PHCompositeNode* parnode = emcNodeHelper::findCompositeNode(root, "PAR"); EMCNODEASSERT( parnode );
  PHCompositeNode* geanode = emcNodeHelper::findCompositeNode(root, "GEA"); EMCNODEASSERT( geanode );
  PHCompositeNode* dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );


  // Output objects

  emcGeaTrackContainer * tracks = emcGeaTrackContainer::createdef();
  emcNodeHelper::insertObject< emcGeaTrackContainer >(dstnode, tracks, "emcGeaTrackContainer");

  emcGeaTowerContainer * towers = emcGeaTowerContainer::createdef();
  emcNodeHelper::insertObject< emcTowerContainer >(dstnode, towers, "emcTowerContainer");

  emcGeaClusterContainer * clusters = emcGeaClusterContainer::createdef();
  emcNodeHelper::insertObject< emcClusterContainer >(dstnode, clusters, "emcClusterContainer");

  tracks->SetTowers( towers ); tracks->SetClusters( clusters );
  towers->SetTracks( tracks ); towers->SetClusters( clusters );
  clusters->SetTracks( tracks ); clusters->SetTowers( towers );



  // EmcRespParams->SetAnySet(1);

  //  root->print();

  return SubsysRecoStack::Init(root);
}
