#include <Fun4AllReturnCodes.h>

#include "EmcRecalReco.h"
#include <iostream>
#include "mEmcRecalRecoModuleRealYear3v1.h"
#include "emcDataManager.h"
#include "recoConsts.h"




//_____________________________________________________________________________
EmcRecalReco::EmcRecalReco(const std::string &name)
  : SubsysRecoStack(name.c_str())
{
}


//_____________________________________________________________________________
EmcRecalReco::~EmcRecalReco()
{
  if( !empty() ){ delete back(); pop_back(); }

  // Instruct all our plugins to reset themselves.
  // This is not stricly needed, but it will free up
  // some caching memory, and thus won't appear as "possibly leak"
  // in e.g. valgrind outputs.
  emcDataManager* dm = emcDataManager::GetInstance();
  dm->Reset();
}

//_____________________________________________________________________________
int
EmcRecalReco::InitRun(PHCompositeNode* topNode)
{
  Reset(topNode);

  // create worker
  recoConsts* recoconsts = recoConsts::instance();
  SubsysReco * recomodule = new mEmcRecalRecoModuleRealYear3v1(*recoconsts);
  if( recomodule == NULL ){
    std::cerr << __FILE__ << ":" << __LINE__ 
	      << Name() << "::InitRun : did not find a suitable worker."
	      << std::endl;
    return ABORTRUN;
  }
  
  std::cout << Name() << "::InitRun : I selected " 
	    << recomodule->Name()
	    << " as reconstruction module for EMCAL." << std::endl;

  push_back( recomodule );
  

  // run Init()
  int rc = SubsysRecoStack::Init(topNode);
  if( rc != EVENT_OK ) return rc;


  // run InitRun()
  return SubsysRecoStack::InitRun(topNode);
}


//_____________________________________________________________________________
int
EmcRecalReco::Reset(PHCompositeNode* topNode)
{
  // run Reset()
  int rc = SubsysRecoStack::Reset(topNode);
  if( rc != EVENT_OK ) return rc;


  // remove worker
  if( !empty() ){ delete back(); pop_back(); }


  return EVENT_OK;
}

