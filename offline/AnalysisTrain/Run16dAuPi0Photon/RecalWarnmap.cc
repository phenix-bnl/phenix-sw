#include "RecalWarnmap.h"
#include <iostream>
#include <fstream>
#include <cmath>

#include "TFile.h"
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
#include "getClass.h"
#include "TOAD.h"

#include "Run16WMap.h" // Run16 warmap

using namespace std;
using namespace findNode;

const double DEG_PER_RAD = 180.0 / M_PI;

const double RecalWarnmap::pi = M_PI;

RecalWarnmap::RecalWarnmap(int input_flag, int debug_flag) : 
  SubsysReco("PI04ALL Run16 dAu Warmnap recalibrator"), 
// o1  _phGlobalNodeName("PHGlobal"),
  _emcClusterContainerNodeName("emcClusterContainer")   {
  rc = recoConsts::instance();
  debug = rc->get_IntFlag("EMCNEW_DEBUG", 0);
  if(debug>0) std::cout <<"RecalWarnmap constructor"<<endl; 
  TwrVeto=NULL;

  runnumber = 0;
  _emcClusterContainer_ptr = NULL;
  se = NULL;

  return;
}

RecalWarnmap::~RecalWarnmap() {
  if(debug>0) std::cout <<"RecalWarnmap destructor"<<endl; 
  if(TwrVeto) delete TwrVeto;
}


void RecalWarnmap::updateCluster(emcClusterContent * c) {
/// set map for the cluster
  int arm = c->arm();
  int sector = c->sector();
  int mysector = 4*arm + sector;                 // Run16 warnmap
  int y = c->iypos();
  int z = c->izpos();
  unsigned int deadmap = 0;
  if(TwrVeto->IsFlagged(mysector, y, z)) { /// deadmap filled only for dead towers
    //std::cout << mysector << "  " << y << "  " << z << std::endl; 
    deadmap = TwrVeto->GetFlag(mysector, y, z);
  }
  unsigned int warnmap = TwrVeto->GetFlag(mysector, y, z);
  c->set_maps(deadmap,warnmap);
  if(debug>0) std::cout <<"RecalWarnmap cluster as,y,z "<< mysector<<" "<<y<<" "<<z
      <<"  dead/warnmap flag "<< deadmap<<" "<<warnmap << endl; 
}

int RecalWarnmap::process_event(PHCompositeNode *topNode) {
  bool nodeLookup = getNodes(topNode);
  if ( nodeLookup == false ) {
    std::cout << PHWHERE << "WARNING: Failed to get one or more critical Nodes!" << std::endl;
  }
// o1  if ( !_phGlobal_ptr ) return -1;
  if ( ! _emcClusterContainer_ptr ) { 
    std::cout << "<RecalWarnmap::process_event>  no _emcClusterContainer_ptr" <<endl;
    return -1;
  }
  
  size_t nclusters = _emcClusterContainer_ptr->size(); 
  for(size_t i=0; i < nclusters; i++) {
    emcClusterContent *c = _emcClusterContainer_ptr->getCluster(i);
    if(c==NULL) std::cerr<< "RecalWarnmap::process_event error: NULL cluster "<<i<<endl;
    else updateCluster(c);
  }
//cout << "<RecalWarnmap::process_event> EVENT OK" <<endl;
  return EVENT_OK;
}
  
int RecalWarnmap::InitRun(PHCompositeNode *topNode) {
  std::cout << __FILE__ << ":" << __LINE__  << " in InitRun" << std::endl;

  runnumber = 0;
  RunHeader *runheader = getClass<RunHeader>(topNode, "RunHeader");
  if ( !runheader ) {
    std::cout << PHWHERE << "Failed to find RunHeader Node" << std::endl;
  }
  runnumber = runheader->get_RunNumber();
  std::cout << "RecalWarnmap::InitRun: Run Number = " << runnumber <<std::endl;
 
  return EVENT_OK;
}

int RecalWarnmap::Init(PHCompositeNode *topNode) {
  se = Fun4AllServer::instance();
  if(debug>0) cout <<"        Initializing warnmap"<<endl; 
//  TwrVeto = new Run16WMap("/direct/phenix+u/ondrejch/ThermalPhotonRun8/src/map/map.kensuke.Run8.txt");     // Run8 d Au  - preliminary one from Kensuke

  TOAD toad_loader("Run16dAuPi0Photon");
  string file_location1 = toad_loader.location("DeadWarnRun16.txt");
  TwrVeto = new Run16WMap(file_location1.c_str());     // Run 12
  if(debug>0) cout <<"        Warnap initialized " << file_location1.c_str() <<endl; 

  return EVENT_OK;
}

int RecalWarnmap::End(PHCompositeNode *topNode) {
  return EVENT_OK;
}

bool RecalWarnmap::getNodes(PHCompositeNode* topNode) {
  bool retVal = true; // set to false if we fail to get a needed node

  /* o1 _phGlobal_ptr = getClass<PHGlobal>(topNode, _phGlobalNodeName.c_str());
  if ( !_phGlobal_ptr )     {
      std::cout << PHWHERE << "Could not find " << _phGlobalNodeName << " Node" << std::endl;
      retVal = false;
    }*/

  _emcClusterContainer_ptr = getClass<emcClusterContainer>(topNode, _emcClusterContainerNodeName.c_str());
  if ( !_emcClusterContainer_ptr ) {
    std::cout << PHWHERE << "Could not find " << _emcClusterContainerNodeName << " Node" << std::endl;
    retVal = false;
  }
  return retVal;
}
