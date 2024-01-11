#include <Fun4AllReturnCodes.h>
#include "mEmcRecoModuleRealYear4.h"

#include "emcDataStorageMap.h"
#include "emcDBMS.h"
#include "emcNodeHelper.h"
#include "mEmcGeometryModule.h"
#include "PHIODataNode.h"
#include "PHTimeStamp.h"
#include "recoConsts.h"

#include "emcClusterContainerv6.h"
#include "emcTowerContainerv3.h"
#include "emcTowerContainerDST.h"
#include "emcClusterAuxInfoContainerV1.h"
#include "mEmcCalibratorModulev2.h"
#include "mEmcClusterizerv0.h"
#include "mEmcClusterizerv1.h"

#include <iostream>
#include <memory>
#include <vector>
#include <cassert>

using namespace std;

const float mEmcRecoModuleRealYear4::fgTowerThresholdPbSc = 0.010;  
const float mEmcRecoModuleRealYear4::fgTowerThresholdPbGl = 0.014;  
const float mEmcRecoModuleRealYear4::fgMinClusterEnergyPbSc = 0.015;
const float mEmcRecoModuleRealYear4::fgMinClusterEnergyPbGl = 0.060;

//_____________________________________________________________________________
mEmcRecoModuleRealYear4::mEmcRecoModuleRealYear4(const PHFlag& flags):
  SubsysRecoStack("mEmcRecoModuleRealYear4")
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

  fStartupMessage << "TimeStamp=" << *fTimeStamp << std::endl;

  //  cout << "mEmcRecoModuleRealYear4::mEmcRecoModuleRealYear4 : timestamp="
  //       << *fTimeStamp << endl;
  
  fConstantGains = false;
  fSectors = "emcal";
  fDataStorageMap = 0;

  if ( flags.FlagExist("EMCCONSTANTGAINS") )
    {
      if ( flags.get_IntFlag("EMCCONSTANTGAINS") == 1 ) 
	{
	  fConstantGains = true;
	  //	  cout << "mEmcRecoModuleRealYear4::mEmcRecoModuleRealYear4 : "
	  fStartupMessage 
	       << "Will not use variability of PbSc gains"
	       << endl;
	}
    }

  if ( flags.FlagExist("EMCSECTORS") )
    {
      fSectors = flags.get_CharFlag("EMCSECTORS");
      //      cout << "mEmcRecoModuleRealYear4::mEmcRecoModuleRealYear4 : will use "
      fStartupMessage 
	   << "Considering emcal sectors : "
	   << fSectors
	   << endl;
    }

  if ( flags.FlagExist("EMCDATASOURCE") && 
       flags.FlagExist("EMCEXPERTDATASOURCES"))
    {
      std::cout << PHWHERE << "<WARNING> Both EMCDATASOURCE and "
		<< "EMCEXPERTDATASOURCES flags are present. "
		<< "Second one will superceede first one."
		<< std::endl;
    }

  if ( flags.FlagExist("EMCDATASOURCE") && 
       !flags.FlagExist("EMCEXPERTDATASOURCES"))
    {
      emcManageable::EStorage ds = 
	emcManageable::GetStorage(flags.get_CharFlag("EMCDATASOURCE"));
      if ( ds == emcManageable::kNone )
	{
	  cerr << "mEmcRecoModuleRealYear4::mEmcRecoModuleRealYear4 : "
	       << "Flag EMCDATASOURCE=" << flags.get_CharFlag("EMCDATASOURCE")
	       << " is not valid. Using default=" 
	       << emcManageable::GetStorageName(emcDBMS::get())
	       << endl;
	}
      else
	{
	  fDataStorageMap = new emcDataStorageMap(ds);
	}
      
      fStartupMessage << "Using "
		      << emcManageable::GetStorageName(fDataStorageMap->storage())
		      << " as calibration data source"
		      << endl;				      
    }

  if (!fDataStorageMap)
    {
      fDataStorageMap = new emcDataStorageMap(emcDBMS::get());
    }

  if ( flags.FlagExist("EMCEXPERTDATASOURCES") )
    {
      fDataStorageMap->parse(flags.get_CharFlag("EMCEXPERTDATASOURCES"));
      fStartupMessage << "Calibration data will be read from " 
		      << emcManageable::GetStorageName(fDataStorageMap->storage())
		      << " except the following ones : "
		      << endl
		      << (*fDataStorageMap)
		      << endl;
    }


  fRunNumber = flags.get_IntFlag("RUNNUMBER");

  fDstNodeName = "DST";
}

//_____________________________________________________________________________
mEmcRecoModuleRealYear4::~mEmcRecoModuleRealYear4()
{
  delete fTimeStamp;
  delete fDataStorageMap;
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear4::createNodeTree(PHCompositeNode* topNode)
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
mEmcRecoModuleRealYear4::identify(ostream& os) const
{
  os << "mEmcRecoModuleRealYear4::identify" << endl;
}

//_____________________________________________________________________________
int 
mEmcRecoModuleRealYear4::InitRun(PHCompositeNode* topNode)
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
	  cerr << "<E> mEmcRecoModuleRealYear4::setup : no DST or "
	       << " uDST node found !"
	       << endl;
	  return ABORTRUN;
	}
    }

  createNodeTree(topNode);

  // Calibrator
  setup_calibrator(topNode);

  // Clustering  
  setup_clustering(topNode);

  std::cout << std::string(80,'*') << "\n"
	    << "EMCAL Reconstruction is using mEmcRecoModuleRealYear4\n" 
	    << fStartupMessage.str() 
	    << "Sequence of modules used :"
	    << std::endl;

  Print();

  std::cout << std::string(80,'*') << std::endl;
  return 0;
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear4::setup_calibrator(PHCompositeNode* topNode)
{
  // The data we need first.
  // Check on the existence of various nodes has been done elsewhere.

  emcNodeHelper nh;

  PHCompositeNode* dstNode = nh.findCompositeNode(topNode,"DST");
  
  nh.addObject<emcTowerContainerv3>(dstNode,"emcTowerContainer");

  assert(fTimeStamp!=0);

  mEmcCalibratorModulev2* calib = 
    new mEmcCalibratorModulev2(fRunNumber,*fTimeStamp,fConstantGains,
			       *fDataStorageMap,fSectors.c_str());

  push_back(calib);
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear4::setup_clustering(PHCompositeNode* topNode)
{
  emcNodeHelper nh;

  PHCompositeNode* dstNode = 
    nh.findCompositeNode(topNode,fDstNodeName.c_str());

  nh.addObject<emcClusterContainerv6>(dstNode,"emcClusterContainer");
  nh.addObject<emcClusterAuxInfoContainerV1>(dstNode,"emcClusterAuxInfo");  
  nh.addObject<emcTowerContainerDST>(dstNode,"emcHitContainer");

  PHCompositeNode* parNode = nh.findCompositeNode(topNode,"PAR");

  mEmcGeometryModule* geometryModule = 
    new mEmcGeometryModule(mEmcGeometryModule::kReal);
  PHIODataNode<TObject>* geometryNode 
    = new PHIODataNode<TObject>(geometryModule, "mEmcGeometry");
  parNode->addNode(geometryNode);

  //  Allow external disabling of clustering (default = do the clustering) .
  recoConsts *rc = recoConsts::instance();

  mEmcClusterizerv0* clusterizer;  

  if ( rc->FlagExist("OLDEMCCLUSTERFORMAT") )
    {
      clusterizer = new mEmcClusterizerv0(geometryModule);
      cout << PHWHERE << " Using old cluster format" << endl;
     }
  else
    {
      clusterizer = new mEmcClusterizerv1(geometryModule);  // no mistake, v1 inherits from v0
      cout << PHWHERE << " Using lean cluster format" << endl;
    }
  
  clusterizer->SetTowerThresholdPbSc(fgTowerThresholdPbSc);
  clusterizer->SetTowerThresholdPbGl(fgTowerThresholdPbGl);
  clusterizer->SetMinClusterEnergyPbSc(fgMinClusterEnergyPbSc);
  clusterizer->SetMinClusterEnergyPbGl(fgMinClusterEnergyPbGl);

  //  Allow external disabling of clustering (default = do the clustering) .

  if (rc->get_IntFlag("RUNEMCCLUSTERING",1)==1) 
    {
      push_back(clusterizer);
    }
  else
    {
      cout << PHWHERE << " WARNING::User has disabled EMC clustering!!!" << endl;
    }
}






