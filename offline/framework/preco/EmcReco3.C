#include <Fun4AllReturnCodes.h>

#include "EmcReco3.h"

#include "emcRecoModuleFactory.h"
#include "emcDataManager.h"
#include "recoConsts.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"

#include <iostream>

using namespace std;


//_____________________________________________________________________________
EmcReco3::EmcReco3(const string &name) : SubsysRecoStack(name.c_str())
{
}


//_____________________________________________________________________________
EmcReco3::~EmcReco3()
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
EmcReco3::InitRun(PHCompositeNode* topNode)
{
  Reset(topNode);


  // create worker
  recoConsts* recoconsts = recoConsts::instance();
  SubsysReco * recomodule = emcRecoModuleFactory::create(*recoconsts);

  if( recomodule == NULL ){
    cerr << __FILE__ << ":" << __LINE__ 
	 << Name() << "::InitRun : did not find a suitable worker."
	 << endl;
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
  rc = SubsysRecoStack::InitRun(topNode);
  if( rc != EVENT_OK ) return rc;


  return EVENT_OK;
}


//_____________________________________________________________________________
int
EmcReco3::Reset(PHCompositeNode* topNode)
{
  // run Reset()
  int rc = SubsysRecoStack::Reset(topNode);
  if( rc != EVENT_OK ) return rc;


  // remove worker
  if( !empty() ){ delete back(); pop_back(); }


  return EVENT_OK;
}


//_____________________________________________________________________________
int
EmcReco3::ResetEvent(PHCompositeNode* topNode)
{  
  // run ResetEvent()
  int rc = SubsysRecoStack::ResetEvent(topNode);
  if( rc != EVENT_OK ) return rc;


  // clean up nodes
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("EMC")) mainIter.forEach(reset);


  return EVENT_OK;
}

