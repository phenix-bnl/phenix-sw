#include <Fun4AllReturnCodes.h>
#include "mEmcRecoModuleRealYear2.h"
#include "emcNodeHelper.h"
#include "PHFlag.h"
#include "mEmcCalibratorModule.h"
#include "emcCalibratorFactory.h"
#include "emcCalibrator.h"
#include "PHTimeStamp.h"
#include "PHCompositeNode.h"
#include "dEmcCalibTowerWrapper.h"
#include "emcCalibratedDataObject.h"

#include <cassert>
#include <iostream>

using namespace std;

const float mEmcRecoModuleRealYear2::fgPbScTowerThreshold = 0.010; // 10 MeV  
const float mEmcRecoModuleRealYear2::fgPbGlTowerThreshold = 0.014; // 14 MeV  
							      
const float mEmcRecoModuleRealYear2::fgPbScMinClusterEnergy = 0.015; // 15 MeV;
const float mEmcRecoModuleRealYear2::fgPbGlMinClusterEnergy = 0.060; // 60 MeV

//_____________________________________________________________________________

mEmcRecoModuleRealYear2::mEmcRecoModuleRealYear2(const PHFlag& flags):
  SubsysRecoStack("mEmcRecoModuleRealYear2")
{
  assert(flags.get_IntFlag("SIMULATIONFLAG")==0);

  
  if ( flags.get_IntFlag("PPFLAG") ) 
    {
      fPP = true;
    }
  else
    {
      fPP = false;
    }

  fTimeStamp = new PHTimeStamp(0);
}

//_____________________________________________________________________________
int
mEmcRecoModuleRealYear2::End(PHCompositeNode*)
{
  // should call SubsysRecoStack::End()
  return EVENT_OK;
}

//_____________________________________________________________________________
int
mEmcRecoModuleRealYear2::InitRun(PHCompositeNode* topNode)
{
  //---- Then make our tables/objects

  bool ok = emcNodeHelper::makeCompositeNode(topNode,"EMC2","-p");
  if ( !ok ) 
    {
      std::cerr << "mEmcRecoModuleRealYear2::setup : "
		<< "Cannot create EMC2 node ?!" << std::endl;
      topNode->print();
      return ABORTRUN;
    }

  PHCompositeNode* emc2Node = emcNodeHelper::findCompositeNode(topNode,"EMC2");
  PHCompositeNode* dstNode = emcNodeHelper::findCompositeNode(topNode,"DST");

   assert(emc2Node!=0);
   assert(dstNode!=0);

   emcNodeHelper::addTable<dEmcCalibTowerWrapper>(dstNode,"dEmcCalibTower",
						  25000);

     //  emcNodeHelper::addObject<EmcCalibTowerv1>(emcNode,"EmcCalibTower");

//   emcCalibratedDataObject * EmcCdo = new emcCalibratedDataObject() ;
//   PHIODataNode<TObject>* EmcCdoNode 
//     = new PHIODataNode<TObject>(EmcCdo, "EmcCdo");

//   emc2Node->addNode(EmcCdoNode) ;

  emcNodeHelper::addObject<emcCalibratedDataObject>(emc2Node,"EmcCdo");

  // Then go to setup the modules needed
  setupAna(topNode);

  topNode->print();

  return 0;
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear2::setupAna(PHCompositeNode* topNode)
{
  mEmcCalibratorModule* calib =
    new mEmcCalibratorModule(*fTimeStamp,"emcRawDataCalibratorV1");

  push_back( calib );

  calib->UseTimeStamp(*fTimeStamp) ;

  emcCalibrator* rdc = emcCalibratorFactory::GetCalibrator() ;

  if ( fPP ) 
    {
      rdc->SetExtraRejectListFilename
	("/afs/rhic/phenix/phnxemc/DATA/emc_extra_reject_Run2pp.list");
    }
  else
    {
      rdc->SetExtraRejectListFilename
	("/afs/rhic/phenix/phnxemc/DATA/emc_extra_reject_Run2pp.list");      
    }

  rdc->Print();

  //  push_back( setupClustering(topNode) );

  cout << "mEmcRecoModuleRealYear2::setupAna : module list : "
       << endl;

  Print();
}

//_____________________________________________________________________________
SubsysReco*
mEmcRecoModuleRealYear2::setupClustering(PHCompositeNode* topNode)
{
//   mEmcClusterNewModule* mc = 
//     new mEmcClusterNewModule(mEmcGeometry);
  
//   // Setup clustering module.
//   mc->SetTowerThresholdPbSc(fgPbScTowerThreshold);
//   mc->SetTowerThresholdPbGl(fgPbGlTowerThreshold);
//   mc->SetMinClusterEnergyPbSc(fgPbScMinClusterEnergy);
//   mc->SetMinClusterEnergyPbGl(fgPbGlMinClusterEnergy);

//   return mc;
  return 0;
}
