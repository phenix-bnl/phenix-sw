#include "RecalEMCalTOF.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>

#include "TFile.h"
#include "TF1.h"
#include "THmulf.h"
#include "THnSparse.h"
#include "TrigLvl1.h"
#include "RunHeader.h"
#include "utiCentrality.h"
#include "TriggerHelper.h"
#include "PHGlobal.h"
#include "recoConsts.h"

#include "Fun4AllServer.h"
#include "Fun4AllReturnCodes.h"

#include "mEmcGeometryModule.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "getClass.h"
#include "TOAD.h"

#include "emcDataError.h"
#include "emcDataManager.h"
#include "emcDCProcessorv3.h"
#include "emcCalibrationData.h"
#include "emcCalibrationDataHelper.h"
#include "emcDataStorageMap.h"
#include "emcTofT0FEM.h"
#include "T0Out.h"
#include "emcTracedValue.h"
#include "EmcIndexer.h"

#include "Run16WMap.h" // Run16 warmap

using namespace std;
using namespace findNode;

const double DEG_PER_RAD = 180.0 / M_PI;

const double RecalEMCalTOF::pi = M_PI;
const emcCalFEM* LC;

RecalEMCalTOF::RecalEMCalTOF(int input_flag, int debug_flag) : 
  SubsysReco("PI04ALL Run16 dAu Warmnap recalibrator")
// o1  _phGlobalNodeName("PHGlobal"),
{
  rc = recoConsts::instance();
  debug = rc->get_IntFlag("EMCNEW_DEBUG", 0);
  if(debug>0) std::cout <<"RecalEMCalTOF constructor"<<endl; 

  runnumber = 0;

  toad_time = NULL;
  NEW_histo = NULL;
  fCDH = NULL;
  _phglobal_ptr = NULL;
  _emcClusterContainer_ptr = NULL;
  _emcTowerContainer_ptr = NULL;

  se = NULL;

  for(int itow = 0; itow < 24768; itow++){
	  Walk[itow] = 0;
	  Walk2[itow] = 0;
	  T0Offset[itow] = 0;	  
	  T0OffsetSigma[itow] = 0;	  
  }
  for(int isec = 0; isec < 8; isec++){
	  SectorOffset[isec] = 0;
  }
  fafter = new TF1("f","[0]*exp([1]/x)*pow(x,[2])",0.2, 20);
  fafter->SetParameters(-8.25403, -5.4072, -0.33457);
 
  return;
}

RecalEMCalTOF::~RecalEMCalTOF() {
  if(debug>0) std::cout <<"RecalEMCalTOF destructor"<<endl; 
}

int RecalEMCalTOF::process_event(PHCompositeNode *topNode) {
  // cout<<"\n PE in RecalEMCalTOF....";
  bool nodeLookup = getNodes(topNode);
  if ( nodeLookup == false ) {
    std::cout << PHWHERE << "WARNING: Failed to get one or more critical Nodes!" << std::endl;
  }

  float fVtx = _phglobal_ptr->getBbcZVertex();
  
  size_t nclusters = _emcClusterContainer_ptr->size(); 
  size_t ntowers = _emcTowerContainer_ptr->size(); 
  for(size_t i=0; i < nclusters; i++) {
    emcClusterContent *cluster = _emcClusterContainer_ptr->getCluster(i);

	int clustercent = cluster->towerid(0);
	emcTowerContent* tower = NULL;
	for (size_t itow = 0; itow < ntowers; itow++){
		emcTowerContent* towertemp = _emcTowerContainer_ptr->getTower(itow);
		if(towertemp->towerid() == clustercent){
			tower = towertemp;
		}
	}
	if(tower == NULL) continue;

	int ifem, channel, isec;
	EmcIndexer::PXPXSM144CH(clustercent, ifem, channel);
	LC = fCDH->getCalibration(ifem,"LCTofs");
	float lc = LC->getValueFast(channel,0);
	lc = ((lc>25.&&lc<65.)? lc : 40.0)/1000.;  

	int TDC = tower->TDC();
	int ADC = tower->ADC();
	//int towerID = tower->towerid();
	//cout<<towerID<<endl;
	double x = cluster->x();
	double y = cluster->y();
	double z = cluster->z()-fVtx;
  	if(clustercent < 15552){
		isec = clustercent/(72*36);
	} else{
		isec = 6 + (clustercent-6*72*36)/(96*48);
	}

	double d = sqrt( x*x + y*y + z*z );
	double c = 29.979245829979; //[cm/ns]
	double t_flash = d/c;
	double t0_offset = T0Offset[clustercent];
	double sec_offset = SectorOffset[isec];
	double walk = Walk[clustercent]/ADC + Walk2[clustercent]/(ADC*ADC);
	double fTime = - lc * (TDC - walk) - t0_offset - sec_offset - t_flash;
	if(TDC < 0) fTime = -9999;
  // Afterburner
//  fTime = fTime - fafter->Eval( cluster->ecent() );
  if(isec<6) fTime = fTime - fafter->Eval( cluster->ecent() );
  //if(fTime>-40&&fTime<80)
  //cout<<"\n In RecalEMCalTOF "<<fTime;
    cluster->set_tofcorr(fTime);
  }
 
  return EVENT_OK;
}
  
int RecalEMCalTOF::InitRun(PHCompositeNode *topNode) {

 
  std::cout << __FILE__ << ":" << __LINE__  << " in InitRun" << std::endl;

  runnumber = 0;
  
  RunHeader *runheader = getClass<RunHeader>(topNode, "RunHeader");
  if ( !runheader ) {
    std::cout << PHWHERE << "Failed to find RunHeader Node" << std::endl;
  }
  runnumber = runheader->get_RunNumber();
  std::cout << "RecalEMCalTOF::InitRun: Run Number = " << runnumber <<std::endl;
  fCDH = new emcCalibrationDataHelper(runnumber, false);
 
  char dummy[100];
  int run;
  int sector;
  int itowerid;
  double max = 0, peak = 0, sigma = 0;
  double walkconst = 0, walkconst2 = 0, woffset = 0;
  
   
  string file_location0 = toad_time->location("WalkCorrection.txt");
  ifstream file_walk(file_location0.c_str());
  string file_location1 = toad_time->location("TowerByTower.txt");
  ifstream file_tower(file_location1.c_str());
  string file_location2 = toad_time->location("SectorBySector.txt");
  ifstream file_sector(file_location2.c_str());  

  if (file_walk.is_open()) {
     cout << "Recal Open '" << file_location0.c_str() << endl;
  } else {
      cout << "File " << file_location0.c_str() << " doesn't exist" << endl;
     exit(0);
  }
  if (file_tower.is_open()) {
     cout << "Recal Open '" << file_location1.c_str() << endl;
  } else {
      cout << "File " << file_location1.c_str() << " doesn't exist" << endl;
     exit(0);
  }
  if (file_sector.is_open()) {
     cout << "Recal Open '" << file_location2.c_str() << endl;
  } else {
      cout << "File " << file_location2.c_str() << " doesn't exist" << endl;
     exit(0);
  }
  double chiSq_walk;
  while(file_walk>>itowerid>>chiSq_walk>>walkconst>>walkconst2>>woffset){
	   Walk[itowerid] = walkconst;
	   Walk2[itowerid] = walkconst2;
  }
  file_walk.close(); 
  
  while(file_tower>>dummy>>itowerid>>max>>peak>>sigma){
	  T0Offset[itowerid] = peak;	
	  T0OffsetSigma[itowerid] = sigma;	
  }
  file_tower.close();
  
  while(file_sector>>run>>sector>>max>>peak>>sigma){
	   if(run == runnumber){
		   SectorOffset[sector] = peak;
	   }
  }
  file_sector.close();
   
  return EVENT_OK;
 
}

int RecalEMCalTOF::Init(PHCompositeNode *topNode) {
 
  toad_time = new TOAD("Run16dAuPi0Photon");

  se = Fun4AllServer::instance();
  return EVENT_OK;
}

int RecalEMCalTOF::End(PHCompositeNode *topNode) {
 
  delete toad_time;
  delete fCDH;
  return EVENT_OK;
}

bool RecalEMCalTOF::getNodes(PHCompositeNode* topNode) {
 
  bool retVal = true; // set to false if we fail to get a needed node

  _phglobal_ptr = getClass<PHGlobal>(topNode, "PHGlobal");
  if ( !_phglobal_ptr ) {
      cout << PHWHERE << "Could not find PHGlobal Node" << endl;
      retVal = false;
  }
  
  _emcTowerContainer_ptr = getClass<emcTowerContainer>(topNode, "emcHitContainer");
  if ( !_emcTowerContainer_ptr ) {
    std::cout << PHWHERE << "Could not find emcTowerContainer Node" << std::endl;
    retVal = false;
  }

  _emcClusterContainer_ptr = getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  if ( !_emcClusterContainer_ptr ) {
    std::cout << PHWHERE << "Could not find emcClusterContainer Node" << std::endl;
    retVal = false;
  }
 
  return retVal;
}
