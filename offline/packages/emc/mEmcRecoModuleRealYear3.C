#include <Fun4AllReturnCodes.h>
#include "mEmcRecoModuleRealYear3.h"
#include "emcPatch.h"
#include "emcTowerContainerv1.h"
#include "emcNodeHelper.h"
#include "PHFlag.h"
#include "mEmcCalibratorModule.h"
#include "emcCalibratorFactory.h"
#include "emcCalibrator.h"
#include "PHTimeStamp.h"
#include "PHCompositeNode.h"
#include "dEmcCalibTowerWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"
#include "dEmcClusterLocalWrapper.h"
#include "dEmcEventWrapper.h"
#include "emcCalibratedDataObject.h"
#include "mEmcGeometryModule.h"
#include "mEmcClusterNewModule.h"
#include "recoConsts.h"
#include <cassert>
#include <memory>
#include <iostream>

using namespace std;


const float mEmcRecoModuleRealYear3::fgPbScTowerThreshold = 0.010; // 10 MeV  
const float mEmcRecoModuleRealYear3::fgPbGlTowerThreshold = 0.014; // 14 MeV  
							      
const float mEmcRecoModuleRealYear3::fgPbScMinClusterEnergy = 0.015; // 15 MeV
const float mEmcRecoModuleRealYear3::fgPbGlMinClusterEnergy = 0.060; // 60 MeV

//_____________________________________________________________________________

mEmcRecoModuleRealYear3::mEmcRecoModuleRealYear3(const PHFlag& flags):
  SubsysRecoStack("mEmcRecoModuleRealYear3")
{
  const recoConsts* rc = dynamic_cast<const recoConsts*>(&flags);
  assert(rc!=0);

  assert(flags.get_IntFlag("SIMULATIONFLAG")==0);
  


  
 // Insure we use the same time stamp as the one blessed by the framework.
  fTimeStamp = new PHTimeStamp(rc->get_TimeStamp());

  PHTimeStamp y1999(1999,1,1,0,0,0);

  assert( *fTimeStamp > y1999 );

}

//_____________________________________________________________________________
mEmcRecoModuleRealYear3::~mEmcRecoModuleRealYear3()
{
  delete fTimeStamp;
}

//_____________________________________________________________________________
int
mEmcRecoModuleRealYear3::InitRun(PHCompositeNode* topNode)
{
  //---- Then make our tables/objects

  PHCompositeNode* dstNode = emcNodeHelper::findCompositeNode(topNode,"DST");

  if (!dstNode) 
    {
      cerr << "<E>" << getName() << "::setup"
	   << " no DST node found !"
	   << endl;
      return ABORTRUN;
    }

  bool ok = emcNodeHelper::makeCompositeNode(topNode,"EMC","-p");
  assert(ok==true);

  ok = emcNodeHelper::makeCompositeNode(topNode,"EMC2","-p");
  assert(ok==true);

  ok = emcNodeHelper::makeCompositeNode(topNode,"PAR","-p");
  assert(ok==true);

  // Create the data objects we'll need

  PHCompositeNode* emc2Node = emcNodeHelper::findCompositeNode(topNode,"EMC2");
  PHCompositeNode* emcNode = emcNodeHelper::findCompositeNode(topNode,"EMC");
  PHCompositeNode* parNode = emcNodeHelper::findCompositeNode(topNode,"PAR");

  emcNodeHelper::addObject<emcTowerContainerv1>(dstNode,"emcTowerContainer");

  emcNodeHelper::addTable<dEmcCalibTowerWrapper>(dstNode,"dEmcCalibTower",
						 25000);
  emcNodeHelper::addTable<dEmcClusterLocalExtWrapper>(dstNode,
						      "dEmcClusterLocalExt",
						      25000);
  emcNodeHelper::addTable<dEmcClusterLocalWrapper>(emcNode,
						      "dEmcClusterLocal",
						      25000);
  emcNodeHelper::addTable<dEmcEventWrapper>(emcNode,"dEmcEvent",1);

  emcNodeHelper::addObject<emcCalibratedDataObject>(emc2Node,"EmcCdo");

  // Create the module we'll need

  mEmcCalibratorModule* calib =
    new mEmcCalibratorModule(*fTimeStamp,"emcRawDataCalibratorV2");
  
  push_back(calib);
  
  calib->UseTimeStamp(*fTimeStamp) ;
  
  emcCalibrator* rdc = emcCalibratorFactory::GetCalibrator() ;

  rdc->SetExtraRejectListFilename
    ("/afs/rhic/phenix/users/maximv/emc/emc_extra_reject_Run3dAu.list");
  rdc->Print();

  mEmcGeometryModule* geometryModule = 
    new mEmcGeometryModule(mEmcGeometryModule::kReal);

  PHIODataNode<TObject>* geometryNode 
    = new PHIODataNode<TObject>(geometryModule, "mEmcGeometry");
  parNode->addNode(geometryNode);

  push_back(new emcPatch);

  mEmcClusterNewModule* mc = 
    new mEmcClusterNewModule(geometryModule);
  
  // Setup clustering module.
  mc->SetTowerThresholdPbSc(fgPbScTowerThreshold);
  mc->SetTowerThresholdPbGl(fgPbGlTowerThreshold);
  mc->SetMinClusterEnergyPbSc(fgPbScMinClusterEnergy);
  mc->SetMinClusterEnergyPbGl(fgPbGlMinClusterEnergy);

  push_back(mc);

  cout << "mEmcRecoModuleRealYear3::setupAna : module list : "
       << endl;

  Print();
  topNode->print();

  return 0;
}


