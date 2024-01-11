#include <iostream>

#include <TH2.h>

#include <phool.h>
#include <PHTimeStamp.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>
#include <RunHeader.h>

#include <emcManageable.h>
#include <emcDBMS.h>
#include <emcBadModulesv1.h>
#include <emcTimeStamp.h>
#include <emcDataManager.h>
#include <EmcIndexer.h>
#include <emcNodeHelper.h>

#include <emcTowerContent.h>
#include <emcGeaTowerContent.h>
#include <emcTowerContainer.h>
#include <emcClusterContent.h>
#include <emcClusterContainer.h>

#include <EmcApplyQA.h>

//#define DEBUG(x...) printf(x)
#define DEBUG(x...) if( verbosity ) printf(x)
//#define DEBUG(x...) {}


ClassImp(EmcApplyQA);



using namespace std;






EmcApplyQA::EmcApplyQA(EmcApplyQA::target_t target, char * filename): SubsysReco("EmcApplyQA"){
  this->target = target;
  this->usehisto = false;
  fBadModules = NULL;
  this->filename = filename; runno = -1; flags = NULL;
  origin = emcBadModules::kPhysics; // read offline only, not emcBadModules::kAll;
}





EmcApplyQA::EmcApplyQA(EmcApplyQA::target_t target, int runno): SubsysReco("EmcApplyQA"){
  this->target = target;
  this->usehisto = false;
  fBadModules = NULL;
  filename = NULL; this->runno = runno; flags = NULL;
  origin = emcBadModules::kPhysics; // read offline only, not emcBadModules::kAll;
}





EmcApplyQA::EmcApplyQA(EmcApplyQA::target_t target, PHFlag * flags): SubsysReco("EmcApplyQA"){
  this->target = target;
  this->usehisto = false;
  fBadModules = NULL;
  filename = NULL; runno = -1; this->flags = flags;
  origin = emcBadModules::kPhysics; // read offline only, not emcBadModules::kAll;
}





EmcApplyQA::~EmcApplyQA(){
  delete fBadModules; fBadModules = NULL;
}





void EmcApplyQA::fillhistos(const char * prefix){
  usehisto = true; histoprefix = prefix;


  Fun4AllServer* se = Fun4AllServer::instance();

  for ( size_t is = 0; is < 8; is++ ){
    int nx = (is <= 5) ? 72 : 96; // pbsc : pbgl
    int ny = (is <= 5) ? 36 : 48; // pbsc : pbgl

    if( target & TOWER ){
      const char * name0 = histoname(TOWER, is, "_all");
      TH1* h0 = new TH2F(name0, name0, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name0, h0);
      
      const char * name1 = histoname(TOWER, is, "_before");
      TH1* h1 = new TH2F(name1, name1, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name1, h1);
      
      const char * name2 = histoname(TOWER, is, "_after");
      TH1* h2 = new TH2F(name2, name2, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name2, h2);
      
      const char * name3 = histoname(TOWER, is, "_dead");
      TH1* h3 = new TH2F(name3, name3, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name3, h3);
    }
       
    if( target & CLUSTER ){
      const char * name0 = histoname(CLUSTER, is, "_all");
      TH1* h0 = new TH2F(name0, name0, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name0, h0);
      
      const char * name1 = histoname(CLUSTER, is, "_before");
      TH1* h1 = new TH2F(name1, name1, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name1, h1);
      
      const char * name2 = histoname(CLUSTER, is, "_after");
      TH1* h2 = new TH2F(name2, name2, nx, -0.5, nx-0.5, ny, -0.5, ny-0.5);
      se->registerHisto(name2, h2);
    }
  }
  
}





const char * EmcApplyQA::histoname(target_t target, int num, const char * suffix){
  static char buff[1024];

  assert( 0 <= num  &&  num <= 9 ); 

  snprintf(buff, sizeof(buff), "%s%s%c%s",
	   histoprefix.c_str(),
	   (target == TOWER) ? "tower" : "cluster",
	   '0' + num, 
	   suffix
	   );

  return buff;
}





TH2 * EmcApplyQA::gethisto(target_t target, int num, const char * suffix){
  Fun4AllServer* se = Fun4AllServer::instance();
  TH2 * h = dynamic_cast< TH2 * >( se->getHisto( histoname(target, num, suffix) ) );
  assert( h != 0 );  
  return h;
}





int EmcApplyQA::setsource(const char * filename){
  delete fBadModules; fBadModules = NULL;
  
  emcManageable::EStorage ds = emcManageable::kFile_ASCII;
  //emcManageable::EStorage ds = emcManageable::GetStorage( filename );
  //assert( ds == emcManageable::kFile_ASCII );
  
  //emcDataManager* dm = emcDataManager::GetInstance();
  //const string dir = dm->GetSourceDir();

  const char * name = emcManageable::GetStorageName(ds);
  cout << __PRETTY_FUNCTION__  << ": using " << name << " (" << filename << ") as datasource." << endl;

  fBadModules = new emcBadModulesv1(filename, emcBadModules::kPhysics, true, "emcal");

  return EVENT_OK;
}





int EmcApplyQA::setsource(PHTimeStamp t){
  delete fBadModules; fBadModules = NULL;

  emcManageable::EStorage ds = emcManageable::kDB_Pg; //emcDBMS::get();

  const char * name = emcManageable::GetStorageName(ds);
  const char * date = t.formatTimeString();
  cout << __PRETTY_FUNCTION__  << ": using " << name << " (" << date <<  ") as datasource." << endl;

  fBadModules = new emcBadModulesv1(t, origin, ds, true, "emcal");

  for(size_t i = 0; i < 24768; i++){
    int iarm, is, iz, iy, towerid = i;
    EmcIndexer::TowerLocation(towerid, iarm, is, iz, iy);
    is = iarm ? (7 - is) : is;
    DEBUG("=== clb   %6d   %1d %2d %2d   %08x %08x   ", towerid, is, iy, iz, 0U, 0U);
    DEBUG("%08x %08x\n", fBadModules->DeadmapFast(towerid), fBadModules->WarnmapFast(towerid));
  }

  return EVENT_OK;
}





int EmcApplyQA::setsource(int runnumber){
  delete fBadModules; fBadModules = NULL;

  emcTimeStamp ets; ets.SetSource( emcDBMS::get() );
  emcDataManager* dm = emcDataManager::GetInstance();
  if( dm->Read(ets, runnumber) == false ){
    cerr << __PRETTY_FUNCTION__ << ": failed to read runnumber data." << endl;
    return ABORTRUN;
  }

  const char * date = ets.getTimeStamp().formatTimeString();
  cout << __PRETTY_FUNCTION__ << ": converted runno " << runnumber << " into date: " << date << endl;

  return setsource( ets.getTimeStamp() );
}





int EmcApplyQA::setsource(PHFlag * flags){
  delete fBadModules; fBadModules = NULL;

  if( ! flags->FlagExist("RUNNUMBER") ){
    cerr << __PRETTY_FUNCTION__ << ": RUNNUMBER flag does not exist!" << endl;
    return ABORTRUN;
  }

  int thisrunno = flags->get_IntFlag("RUNNUMBER");
  cout << __PRETTY_FUNCTION__ << ": read runno " << thisrunno << " from " << flags << endl;

  return setsource( thisrunno );
}





int EmcApplyQA::setsource(PHCompositeNode * root){
  delete fBadModules; fBadModules = NULL;

  PHCompositeNode* run = emcNodeHelper::findCompositeNode(root, "RUN"); 
  EMCNODEASSERT( run );

  //  PHFlag * thisflags = emcNodeHelper::getObject< PHFlag >("Flags", run);
  //  EMCNODEASSERT( thisflags );
  //  cout << __PRETTY_FUNCTION__ << ": found RUN/Flags at " << thisflags << endl;
  //
  //  return setsource( thisflags );

  RunHeader * header = emcNodeHelper::getObject< RunHeader >("RunHeader", run);
  EMCNODEASSERT( header );
  int thisrunno = header->get_RunNumber();
  cout << __PRETTY_FUNCTION__ << ": read runno " << thisrunno << " from RUN/RunHeader at " << header << endl;

  return setsource( thisrunno );
}





int EmcApplyQA::Init(PHCompositeNode* root){
  if( filename ) return setsource( filename );
  else if( runno != -1 ) return setsource( runno );
  else return EVENT_OK;
}





int EmcApplyQA::InitRun(PHCompositeNode* root){
  // check input nodes
  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(root, "DST"); 
  EMCNODEASSERT( dst );

  if( target & TOWER ){
    emcTowerContainer * towers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", dst);
    EMCNODEASSERT( towers );
  }

  if( target & CLUSTER ){
    emcClusterContainer * clusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", dst);
    EMCNODEASSERT( clusters );
  }

  if( filename == NULL  &&  runno == -1 ){
    int rc = flags ? setsource( flags ) : setsource( root );
    if( rc ) return rc;
  }

  return EVENT_OK;
}





int EmcApplyQA::process_event(PHCompositeNode* root){

  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(root, "DST"); 
  EMCNODEASSERT( dst );



  // process towers
  if( target & TOWER ){
    emcTowerContainer * towers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", dst);
    EMCNODEASSERT( towers );

    for(size_t i = 0; i < towers->size(); i++){
      emcTowerContent * t = towers->getTower( i );
      int iarm, is, iz, iy, towerid = t->towerid();
      EmcIndexer::TowerLocation(towerid, iarm, is, iz, iy);
      is = iarm ? (7 - is) : is;

      unsigned int cutmap = (is <= 5) ? 0xffe1ce70 : 0x1ce70; // pbsc : pbgl

      unsigned int tdead = t->ErrorNeighbours(), twarn = t->WarnNeighbours();

      if( usehisto )
	gethisto(TOWER, is, "_all")->Fill(iz, iy);

      if( usehisto  &&  (tdead & cutmap) == 0  &&  (twarn & cutmap) == 0 )
	gethisto(TOWER, is, "_before")->Fill(iz, iy);

      DEBUG("=== twr   %6d   %1d %2d %2d   %08x %08x   ", towerid, is, iy, iz, tdead, twarn);
      tdead |= fBadModules->DeadmapFast(towerid);
      twarn |= fBadModules->WarnmapFast(towerid);
      t->SetNeighbours(tdead, twarn);
      DEBUG("%08x %08x\n", tdead, twarn);

      if( usehisto  &&  (tdead & cutmap) == 0  &&  (twarn & cutmap) == 0 )
	gethisto(TOWER, is, "_after")->Fill(iz, iy);

      if( tdead & 0x400 ){
	emcGeaTowerContent * gt = dynamic_cast< emcGeaTowerContent * >( t );

	if( gt ){
	  gt->scale_edep( 0. );
	} else {
	  t->SetCalibrated(0.0, 9999.9);
	}
	if( usehisto ) gethisto(TOWER, is, "_dead")->Fill(iz, iy);
      }
    }
  }



  // process clusters
  if( target & CLUSTER ){
    emcClusterContainer * clusters = emcNodeHelper::getObject< emcClusterContainer >("emcClusterContainer", dst);
    EMCNODEASSERT( clusters );

    for(size_t i = 0; i < clusters->size(); i++){
      emcClusterContent * c = clusters->getCluster( i );
      int iarm, is, iz, iy, towerid = c->towerid( 0 );
      EmcIndexer::TowerLocation(towerid, iarm, is, iz, iy);
      is = iarm ? (7 - is) : is;

      unsigned int cutmap = (is <= 5) ? 0xffe1ce70 : 0x1ce70; // pbsc : pbgl

      unsigned int cdead = c->deadmap(), cwarn = c->warnmap(); 

      if( usehisto )
	gethisto(CLUSTER, is, "_all")->Fill(iz, iy);

      if( usehisto  &&  (cdead & cutmap) == 0  &&  (cwarn & cutmap) == 0 )
	gethisto(CLUSTER, is, "_before")->Fill(iz, iy);

      DEBUG("=== clr   %6d   %1d %2d %2d   %08x %08x   ", towerid, is, iy, iz, cdead, cwarn);
      cdead |= fBadModules->DeadmapFast(towerid);
      cwarn |= fBadModules->WarnmapFast(towerid);
      c->set_maps(cdead, cwarn);
      DEBUG("%08x %08x\n", cdead, cwarn);

      if( usehisto  &&  (cdead & cutmap) == 0  &&  (cwarn & cutmap) == 0 )
	gethisto(CLUSTER, is, "_after")->Fill(iz, iy);
    }
  }



  return EVENT_OK;
}


