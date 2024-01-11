#include <Fun4AllReturnCodes.h>
#include "mEmcRecoModuleRealYear3v1.h"
#include "emcTowerContainerv3.h"
#include "emcClusterContainerv3.h"
#include "emcClusterContainerv4.h"
#include "emcNodeHelper.h"
#include "mEmcCalibratorModulev1.h"
#include "PHIODataNode.h"
#include "PHTimeStamp.h"
#include "mEmcGeometryModule.h"
#include "mEmcClusterizerv0.h"
#include "recoConsts.h"
#include "mEmcTOFCorr5Module.h"
#include "emcDBMS.h"
#include <memory>
#include <iostream>
#include <cassert>

using namespace std;


const float mEmcRecoModuleRealYear3v1::fgTowerThresholdPbSc = 0.010;  
const float mEmcRecoModuleRealYear3v1::fgTowerThresholdPbGl = 0.014;  
const float mEmcRecoModuleRealYear3v1::fgMinClusterEnergyPbSc = 0.015;
const float mEmcRecoModuleRealYear3v1::fgMinClusterEnergyPbGl = 0.060;

static mEmcTOFCorr5Module* mEmcTOFCorr5 = NULL;

//_____________________________________________________________________________
mEmcRecoModuleRealYear3v1::mEmcRecoModuleRealYear3v1(const PHFlag& flags):
  SubsysRecoStack("mEmcRecoModuleRealYear3v1")
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

  fConstantGains = false;
  fSectors = "emcal";
  fDataSource = emcDBMS::get();

  if ( flags.FlagExist("EMCCONSTANTGAINS") )
    {
      if ( flags.get_IntFlag("EMCCONSTANTGAINS") == 1 ) 
	{
	  fConstantGains = true;
	  fStartupMessage 
	    << "Will not use variability of PbSc gains"
	    << endl;
	}
    }

  if ( flags.FlagExist("EMCSECTORS") )
    {
      fSectors = flags.get_CharFlag("EMCSECTORS");
      fStartupMessage 
	   << "Considering emcal sectors : "
	   << fSectors
	   << endl;
    }

  if ( flags.FlagExist("EMCDATASOURCE") )
    {
      emcManageable::EStorage ds = 
	emcManageable::GetStorage(flags.get_CharFlag("EMCDATASOURCE"));
      if ( ds == emcManageable::kNone )
	{
	  cerr << "mEmcRecoModuleRealYear3v1::mEmcRecoModuleRealYear3v1 : "
	       << "Flag EMCDATASOURCE=" << flags.get_CharFlag("EMCDATASOURCE")
	       << " is not valid. Using default=" 
	       << emcManageable::GetStorageName(fDataSource)
	       << endl;
	}
      else
	{
	  fDataSource = ds;
	}
      fStartupMessage << "Using "
		      << emcManageable::GetStorageName(fDataSource)
		      << " as calibration data source"
		      << endl;		
    }
      
  fRunNumber = flags.get_IntFlag("RUNNUMBER");

  fDstNodeName = "DST";
}

//_____________________________________________________________________________
mEmcRecoModuleRealYear3v1::~mEmcRecoModuleRealYear3v1()
{
  delete fTimeStamp;
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear3v1::createNodeTree(PHCompositeNode* topNode)
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
mEmcRecoModuleRealYear3v1::identify(ostream& os) const
{
  os << "mEmcRecoModuleRealYear3v1::identify" << endl;
}

//_____________________________________________________________________________
int 
mEmcRecoModuleRealYear3v1::InitRun(PHCompositeNode* topNode)
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
	  cerr << "<E> mEmcRecoModuleRealYear3v1::setup : no DST or "
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
	
  std::cout << std::string(80,'*') << "\n"
	    << "EMCAL Reconstruction is using mEmcRecoModuleRealYear3v1\n" 
	    << fStartupMessage.str() 
	    << "Sequence of modules used :"
	    << std::endl;

 Print(); 

  return 0;
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear3v1::setup_calibrator(PHCompositeNode* topNode)
{
  // The data we need first.
  // Check on the existence of various nodes has been done elsewhere.

  emcNodeHelper nh;

  PHCompositeNode* dstNode = nh.findCompositeNode(topNode,"DST");
  
  nh.addObject<emcTowerContainerv3>(dstNode,"emcTowerContainer");

  assert(fTimeStamp!=0);

  mEmcCalibratorModulev1* calib = 
    new mEmcCalibratorModulev1(fRunNumber,*fTimeStamp,fConstantGains,
			       fDataSource,fSectors.c_str());

  push_back(calib);
}

//_____________________________________________________________________________
void
mEmcRecoModuleRealYear3v1::setup_clustering(PHCompositeNode* topNode)
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






