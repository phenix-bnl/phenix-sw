//////////////////////////////////////////////////////////////////////////////////
//
// reincarnation of mEmcApplyQAToSimu and mEmcMaskDeadTowers:
//
// Applys Q&A information to be applied to simulated data.
// Additional Module to update dead/warn status of simulated towers from file.
//
//////////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <cassert>

#include <phool.h>
#include <PHTimeStamp.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>

#include <emcManageable.h>
#include <emcDBMS.h>
#include <emcBadModulesv1.h>
#include <emcTimeStamp.h>
#include <emcDataManager.h>
#include <emcNodeHelper.h>

#include <emcGeaTowerContent.h>
#include <emcGeaTowerContainer.h>

#include <EmcApplyQAToSimu.h>


namespace {

  const static size_t NFEMS = 172;
  const static size_t NCHANNELS = NFEMS*144;

}




//_____________________________________________________________________________
EmcApplyQAToSimu::EmcApplyQAToSimu(): SubsysReco("EmcApplyQAToSimu")
{
  emcdeadrecal = false;
  errorraw.resize(NCHANNELS, 0);
  warnraw.resize(NCHANNELS, 0);
}

//_____________________________________________________________________________
EmcApplyQAToSimu::~EmcApplyQAToSimu()
{
}

//_____________________________________________________________________________
int
EmcApplyQAToSimu::Init(PHCompositeNode * root)
{

  // check flag
  recoConsts *rc = recoConsts::instance();
  if( !rc->FlagExist("EMCDEADRECALDATASOURCE") ) return EVENT_OK;


  // open file
  const char * filename = rc->get_CharFlag("EMCDEADRECALDATASOURCE");
  std::ifstream fin( filename );
  if( fin.fail() ){ 
    std::cerr << __PRETTY_FUNCTION__ << ": error opening " << filename << std::endl; 
    return ABORTRUN;
  }

  // read data
  unsigned int towerid, err, warn, count = 0;
  while (fin >> towerid >> err >> warn){
    errorraw[towerid] = err;
    warnraw[towerid] = warn;
    count++;
  }
  
  // cleanup
  fin.close();
  std::cout << __PRETTY_FUNCTION__ << ": read " << count << " entries from " << filename << std::endl;
  emcdeadrecal = true;
  

  return EVENT_OK;
}

  



//_____________________________________________________________________________
int
EmcApplyQAToSimu::InitRun(PHCompositeNode * root)
{

  // check input
  PHCompositeNode* dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  std::string ss = ""; // to make PHMESSAGE()'ing easier


  // get config data
  recoConsts *rc = recoConsts::instance();
  assert( rc != 0 );



  // get runnumber
  int runnumber = rc->FlagExist("RUNNUMBER") ? rc->get_IntFlag("RUNNUMBER") : 0;



  // select datasource
  emcManageable::EStorage fDataSource = emcDBMS::get();

  if ( rc->FlagExist("EMCDATASOURCE") ){
    const char * charflag = rc->get_CharFlag("EMCDATASOURCE");
    emcManageable::EStorage ds = emcManageable::GetStorage(charflag);

    if ( ds == emcManageable::kNone )
      PHMESSAGE(ss + "Flag EMCDATASOURCE=" + charflag + "is not valid. Using default.");
    else 
      fDataSource = ds;
  }
  
  PHMESSAGE(ss + "using datasource: " + emcManageable::GetStorageName(fDataSource));



  // get calibration data
  emcTimeStamp ets; ets.SetSource(fDataSource);
  emcDataManager* dm = emcDataManager::GetInstance();
  if( dm->Read(ets, runnumber) == false ){
    std::cerr << __PRETTY_FUNCTION__ << ": failed to read calibration data." << std::endl;
    return ABORTRUN;
  }


  // get timestamp
  PHTimeStamp fTimeStamp = ets.getTimeStamp();
  PHMESSAGE(ss + "Using TimeStamp: " + fTimeStamp.formatTimeString());
  PHTimeStamp y1999(1999,1,1,0,0,0);
  assert( fTimeStamp > y1999 );



  // create and fill container for bad data info

  bool init = true; // init all bad modules now.
  emcBadModules::EInformationOrigin origin = emcBadModules::kAll; // read both online QA & offline rejectlist.
  emcBadModules* bm = new emcBadModulesv1(fTimeStamp, origin, fDataSource, init);

  PHCompositeNode* emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  emcNodeHelper::insertObject<emcBadModules>(emcnode, bm, "emcBadModules");



  return EVENT_OK;
}




int
EmcApplyQAToSimu::process_event(PHCompositeNode* root)
{
  size_t deadcount = 0;

  PHCompositeNode* dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );
  PHCompositeNode* emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );

  emcGeaTowerContainer * towers = getGeaObject( emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstnode);
  EMCNODEASSERT( towers );

  emcBadModules* badmodules = emcNodeHelper::getObject<emcBadModules>("emcBadModules", emcnode);
  EMCNODEASSERT( badmodules );

  
  // fill data like deadmap, warnmap, etc. (but not raw adc values!)
  for(size_t i = 0; i < towers->size(); i++){
    emcGeaTowerContent * tower = towers->get(i);

    unsigned int dead = badmodules->Deadmap( tower->get_towerid() );
    unsigned int warn = badmodules->Warnmap( tower->get_towerid() );

    if( emcdeadrecal ){
      dead |= errorraw[ tower->get_towerid() ];
      warn |= warnraw[ tower->get_towerid() ];
    }

    tower->SetNeighbours(dead, warn);


    // from mEmcApplyQAToSimu.C:
    //
    // set ecal and tof to zero 
    // In the case of real data (in MDO -> RDO):
    // a) in compressed mode: the tower is simply dropped 
    // b) in non-compressed mode: ADC and TDC are set to zero
    // We adopt b) here.  When merging with a real event,
    // the tower will be there (i.e. observable by the evaluator) but empty
    if( dead & 0x400 ){

      tower->scale_edep(0.0);
      tower->set_tof(0.0); // should be set to something huge instead..

      deadcount++;
    }
  }


  if( deadcount ) 
    std::cout << __PRETTY_FUNCTION__ << ": Total num. of EMCal dead channels: " << deadcount << std::endl ;


  return EVENT_OK;
}
