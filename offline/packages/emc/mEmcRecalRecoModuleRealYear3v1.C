#include <Fun4AllReturnCodes.h>
#include "mEmcRecalRecoModuleRealYear3v1.h"
#include "emcTowerContainerv3.h"
#include "emcClusterContainerv3.h"
#include "emcClusterContainerv4.h"
#include "emcNodeHelper.h"
#include "mEmcReCalibratorModulev1.h"
#include "PHIODataNode.h"
#include "PHTimeStamp.h"
#include "mEmcGeometryModule.h"
#include "mEmcClusterizerv0.h"
#include "recoConsts.h"
#include <mEmcTOFCorr5Module.h>

#include <memory>
#include <iostream>
#include <string>
#include <cassert>

using namespace std;

static mEmcTOFCorr5Module* mEmcTOFCorr5 = NULL;
const float mEmcRecalRecoModuleRealYear3v1::fgTowerThresholdPbSc = 0.010;
const float mEmcRecalRecoModuleRealYear3v1::fgTowerThresholdPbGl = 0.014;  
const float mEmcRecalRecoModuleRealYear3v1::fgMinClusterEnergyPbSc = 0.015;
const float mEmcRecalRecoModuleRealYear3v1::fgMinClusterEnergyPbGl = 0.060;

//_____________________________________________________________________________

mEmcRecalRecoModuleRealYear3v1::mEmcRecalRecoModuleRealYear3v1(const PHFlag& flags):
  SubsysRecoStack("mEmcRecalRecoModuleRealYear3v1")

{
  const recoConsts* rc = dynamic_cast<const recoConsts*>(&flags);

  assert(rc!=0);
  
  if ( flags.FlagExist("SIMULATIONFLAG") )
    {
      assert(flags.get_IntFlag("SIMULATIONFLAG")==0);
    }
  
  // Insure we use the same time stamp as the one blessed by the framework.
  fTimeStamp = new PHTimeStamp(rc->get_TimeStamp());

  PHTimeStamp y1999(1999,1,1,0,0,0);

  assert( *fTimeStamp > y1999 );

  std::cout << "mEmcRecalRecoModuleRealYear3v1::mEmcRecalRecoModuleRealYear3v1 : timestamp="
	    << *fTimeStamp << std::endl;
  
  fConstantGains = false;

  if ( flags.FlagExist("EMCCONSTANTGAINS") )
    {
      if ( flags.get_IntFlag("EMCCONSTANTGAINS") == 1 ) 
	{
	  fConstantGains = true;
	  std::cout << "mEmcRecalRecoModuleRealYear3v1::mEmcRecalRecoModuleRealYear3v1 : "
	       << " Will not use variability of PbSc gains"
	       << std::endl;
	}
    }

  fRunNumber = flags.get_IntFlag("RUNNUMBER");

  fDstNodeName = "DST";
}

//_____________________________________________________________________________
mEmcRecalRecoModuleRealYear3v1::~mEmcRecalRecoModuleRealYear3v1()
{
  delete fTimeStamp;
}

//_____________________________________________________________________________
void
mEmcRecalRecoModuleRealYear3v1::createNodeTree(PHCompositeNode* topNode)
{
  vector<string> nodes;

  nodes.push_back("EMC");
  nodes.push_back("PAR");

  emcNodeHelper nh;

  for ( size_t i = 0; i < nodes.size(); ++i )
    {
      bool ok = nh.makeCompositeNode(topNode,nodes[i].c_str(),"-p");
      assert(ok==true);
    }
}

//_____________________________________________________________________________
void
mEmcRecalRecoModuleRealYear3v1::identify(ostream& os) const
{
  os << "mEmcRecalRecoModuleRealYear3v1::identify" << endl;
}

//_____________________________________________________________________________
int 
mEmcRecalRecoModuleRealYear3v1::InitRun(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  PHCompositeNode* dstNode = 
    nh.findCompositeNode(topNode,fDstNodeName.c_str());

  if (!dstNode) 
    {
      // Try microDST then.
      fDstNodeName = "UDST";
      dstNode = nh.findCompositeNode(topNode,fDstNodeName.c_str());
      if ( !dstNode ) 
	{
	  cerr << "<E> mEmcRecalRecoModuleRealYear3v1::setup : no DST or "
	       << " uDST node found !"
	       << endl;
	  return ABORTRUN;
	}
    }

  createNodeTree(topNode);

  // Calibrator
  setup_calibrator(topNode);

  // mEmcTOFCorr5 to apply tower corrections and globalT0
  mEmcTOFCorr5 = mEmcTOFCorr5Module::instance();
  mEmcTOFCorr5->readDataFromDB(fRunNumber);

  push_back(mEmcTOFCorr5);

  // Clustering  
  setup_clustering(topNode);
	
  std::cout << "mEmcRecalRecoModuleRealYear3v1::setup : list of modules = "
	    << std::endl;

  Print();

  return EVENT_OK;
}

//_____________________________________________________________________________
void
mEmcRecalRecoModuleRealYear3v1::setup_calibrator(PHCompositeNode* topNode)
{
  // The data we need first.
  // Check on the existence of various nodes has been done elsewhere.

  emcNodeHelper nh;

  PHCompositeNode* dstNode = nh.findCompositeNode(topNode,"DST");
  
  nh.addObject<emcTowerContainerv3>(dstNode,"emcTowerContainer");

  assert(fTimeStamp!=0);

  mEmcReCalibratorModulev1* calib = 
    new mEmcReCalibratorModulev1(fRunNumber,*fTimeStamp,fConstantGains);

  push_back( calib );
}

//_____________________________________________________________________________
void
mEmcRecalRecoModuleRealYear3v1::setup_clustering(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  PHCompositeNode* dstNode = 
    nh.findCompositeNode(topNode,fDstNodeName.c_str());

  //  Changed by TKH SM STJL...8-4-2003
  //nh.addObject<emcClusterContainerv3>(dstNode,"emcClusterContainer");
  nh.addObject<emcClusterContainerv4>(dstNode,"emcClusterContainer");

  PHCompositeNode* parNode = nh.findCompositeNode(topNode,"PAR");

  mEmcGeometryModule* geometryModule = 
    new mEmcGeometryModule(mEmcGeometryModule::kReal);
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






