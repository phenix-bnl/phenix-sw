// ===============
// FILE: RpParManager.C
// ===============

// ******************************************************
//
// Class: RpParManager implementation
//
// Author:  Takashi Hachiya (hachiya@rcf.rhic.bnl.gov)
// 
// Revisions: July 2012 - initial version
//
// ***************************************************************************

#include "RpParManager.h"

#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <getClass.h>

#include <RpConst.h>
#include <ReactionPlaneCalibv1.h>

#include <RunToTime.hh>

//#include <cstdlib>
#include <iostream>
#include <iomanip>
//#include <cstdio>

using namespace std;

int RpParManager::m_nmodule = 0;

//----------------------------------------------------------------------------------------------------

RpParManager::RpParManager(const string &name): 
  Recalibrator(name),
  m_rpcalibv1(NULL),
  m_rpCalibFromDB(true),
  m_rpCalibFileName(""),
  m_nth(0)
{
  baseclasses.insert("RpSumXYObject");
}

RpParManager::~RpParManager()
{
//--  if(m_rpcalibv1!=NULL) delete m_rpcalibv1; // this is deleted in NodeTree
}

int
RpParManager::isValidRun(const int runno) const
{
  // this module can be applied after run11
  if (330693 <= runno) // Run11 p+p (physics) started
  {
    return 1;
  }
  return 0;
}



//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int RpParManager::Init(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  if(rc->FlagExist("RPCALIB_VERBOSE"))
    Verbosity(rc->get_IntFlag("RPCALIB_VERBOSE"));


  m_nmodule++;
  m_nth = m_nmodule;
  if(verbosity>0) { cout<<"RpParManager::Init Nmodule="<<m_nth<<" in N="<<m_nmodule<<endl; }


  if(verbosity>0) cout << "RpParManager::Init() Execution completed." << endl;
  return EVENT_OK;
}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int RpParManager::InitRun(PHCompositeNode *topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "RpParManager::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  if(verbosity>0) cout << "RpParManager::InitRun() Execution started.." << " "<< Name()<<endl;

  CreateNodeTree(topNode);

  recoConsts *rc = recoConsts::instance();
  int runNumber = rc->get_IntFlag("RUNNUMBER");
  // initialize run dependence of the detector setup

  m_rpcalibv1->SetEnableDetByRunNumber(runNumber);
  m_rpcalibv1->PrintDet(true);


  // get value from reconConst
  if(rc->FlagExist("RPCALIB_READFROMDB")){
    m_rpCalibFromDB = rc->get_IntFlag("RPCALIB_READFROMDB");
  }
  if(rc->FlagExist("RPCALIB_CALIBFILENAME")){
    m_rpCalibFileName = rc->get_CharFlag("RPCALIB_CALIBFILENAME");
  }





  cout<<"RpParManager::FlagStatus "<<m_rpCalibFromDB<<" "<<m_rpCalibFileName<<endl;

  if(m_rpCalibFromDB)
    {
      cout<<"RpParMangager calib data fetch from DB"<<endl;
    
      if (m_rpcalibv1->Fetch(runNumber)) // not 0 means failure to load calibs
        {
          cout << PHWHERE << "No Reaction Plane calibration for run " << runNumber 
               << ", no Reaction Plane for this run" << endl;
        }
    }
  else
    {
      cout << "RpParManager::InitRun(): Fetch RP parameters from " << m_rpCalibFileName << endl;
      if(m_rpCalibFileName.length()>0)
        {
          m_rpcalibv1->Fetch(m_rpCalibFileName.c_str());
        }
      else 
        {
          cout << "RpParManager::InitRun(). Default parameters are used" << endl;
        }
    }

  if(verbosity>0) cout << "RpParManager::InitRun() Node tree created." << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int RpParManager::process_event(PHCompositeNode *topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "RpParManager::process_event() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  if(verbosity>0) cout << "RpParManager::process_event() Execution started..." <<endl;
  if(verbosity>0) {cout << "RpParManager::process_event() Event processed." <<endl;}
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

// Create the data
int RpParManager::CreateNodeTree(PHCompositeNode *topNode) 
{

  if(verbosity>0) cout << "RpParManager::CreateNodeTree() Execution started." << endl;

  PHNodeIterator iter(topNode);


  // Look for the PAR node
  PHCompositeNode *parNode;
  parNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "PAR"));
  if (!parNode) { 
    cerr << PHWHERE << "PAR node missing, doing nothing." << endl; 
    return EVENT_OK; 
  }

  PHDataNode<ReactionPlaneCalibv1>* RPCalibNode = NULL;
  RPCalibNode = (PHIODataNode<ReactionPlaneCalibv1>*)iter.findFirst("PHDataNode", "ReactionPlaneCalibv1");
  if (!RPCalibNode)
    {
      if(verbosity>0) cout << "RpParManager::CreateNodeTree() Create new ReactionPlaneCalibv1 ." << " " << Name()<<endl;

      m_rpcalibv1  = new ReactionPlaneCalibv1();
      RPCalibNode = new PHDataNode<ReactionPlaneCalibv1>(m_rpcalibv1, "ReactionPlaneCalibv1");
      parNode->addNode(RPCalibNode);
    }
  else {
      if(verbosity>0) cout << "RpParManager::CreateNodeTree() ReactionPlaneCalibv1 exist." << " " << Name()<<endl;
      m_rpcalibv1 = dynamic_cast<ReactionPlaneCalibv1*>(RPCalibNode->getData());
    }
  
  if(verbosity>0) {
    parNode->print();
  }

  return EVENT_OK;
}

//--------------------------------------------------------------------------------------------

int RpParManager::End(PHCompositeNode *topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "RpParManager::End() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  return EVENT_OK;
}

