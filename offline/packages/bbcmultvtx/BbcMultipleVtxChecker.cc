/*
  
  BbcMultipleVtxChecker.cc
  
  Created: 2011/09/09
  Last Update: 2011/09/09
  Author: Hideyuki Oide
  
  
  Description:
  
  A simple checker to print BbcMultipleVtx DST node.
  
 */



#include "BbcMultipleVtxChecker.hh"

// General
#include <getClass.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>

// DST
#include <RunHeader.h>
#include <EventHeader.h>
#include <TrigLvl1.h>
#include "BbcMultipleVtx.hh"

// BbcMultipleVtx
#include "BbcMultipleVtxList.hh"
#include "BbcMultipleVtxCluster.hh"
#include "BbcMultipleVtxPoint.hh"

#include <TString.h> // for form

using namespace std;


//____________________________________________________________________________________________________
BbcMultipleVtxChecker::BbcMultipleVtxChecker(const string &name): SubsysReco(name)
{}


//____________________________________________________________________________________________________
int BbcMultipleVtxChecker::Init(PHCompositeNode *topNode){
  return 0;
}


//____________________________________________________________________________________________________
int BbcMultipleVtxChecker::InitRun(PHCompositeNode *topNode){
  if(verbosity>0) cout << "<BbcMultipleVtxChecker::Init()>:: start" << endl;
  
  topNode->print();

  fRunHeader   = findNode::getClass<RunHeader>(topNode, "RunHeader");
  fEventHeader = findNode::getClass<EventHeader>(topNode,"EventHeader");
  fTrigLvl1    = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
  fBbcMvtx     = findNode::getClass<BbcMultipleVtx>(topNode, "BbcMultipleVtx");
  
  //fRunNumber = fRunHeader->get_RunNumber();
  
  if(verbosity>0) cout << "<BbcMultipleVtxChecker::Init()>:: end" << endl;
  return 0;
}

//____________________________________________________________________________________________________

int BbcMultipleVtxChecker::End(PHCompositeNode *topNode){
  
  return 0;
}

//____________________________________________________________________________________________________
int BbcMultipleVtxChecker::process_event(PHCompositeNode *top_node){
  
  const Int_t evenum  = fEventHeader->get_EvtSequence();
  cout << Form("\n\nEvent %d\n", evenum) << endl;
  
  if( fBbcMvtx ) {
    fBbcMvtx->print();
  } else {
    cout << "BbcMultipleVtx node is NULL!" << endl;
  }
  
  return EVENT_OK;
}

