#include "GlobalReco.h"

#include "PHGlobalv10.h"
#include "PHGlobalv11.h"
#include "PHGlobal_Centralv1.h"
#include "PHGlobal_Muonv1.h"

#include "Bbc.hh"
#include "BbcOut.h"
#include "CrkHit.h"
#include "DchTrack.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "PadCluster.h"
#include "SmdOut.h"
#include "TecOut.hh"
#include "TofOut.h"
#include "Zdc.hh"
#include "ZdcOut.h"

// new muon framework maps
#include "TMuiHitMapO.h"
#include "TMutHitMap.h"

#include "Fun4AllReturnCodes.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"

#include <iostream>

using namespace std;

typedef PHIODataNode <PHObject> PHObjectNode_t;

//________________________________________________________________
GlobalReco::GlobalReco(const int v, const string &name ):
  SubsysReco(name), version(v)
{
  TimingSN = False;
  
  switch(version)
  {
    case 0: // Run 5 Cu+Cu, Run6 p+p
    TimingSN = False;
    break;
    
    case 1: // Run7 Au+Au
    TimingSN = True;
    break;
    
    default:
    cout << PHWHERE << "WARNING::GlobalReco...Unknown Version Requested: " << version <<endl;
    cout << PHWHERE << "WARNING::GlobalReco...You will receive version 10 PHGlboal instead." << endl;
    TimingSN = False;
    break;
  }
  
  return ;
}

//________________________________________________________________
int GlobalReco::InitRun(PHCompositeNode *topNode)
{
  
  // look for existing PHGlobal from DST node. Create one otherwise
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  PHGlobal *global = findNode::getClass<PHGlobal>(dstNode, "PHGlobal");
  if ( !global )
  {
    
    switch(version)
    {
      case 0: // Run 5 Cu+Cu
      global = new PHGlobalv10();
      break;
      
      case 1: // Run7 Au+Au
      global = new PHGlobalv11();
      break;
      
      default:
      cout << PHWHERE << "WARNING::GlobalReco...Unknown Version Requested: " << version <<endl;
      cout << PHWHERE << "WARNING::GlobalReco...You will receive version 10 PHGlboal instead." << endl;
      global = new PHGlobalv10();
      break;
    }
    
    PHObjectNode_t *PHGlobalNode = new PHObjectNode_t(global, "PHGlobal", "PHObject");
    dstNode->addNode(PHGlobalNode);
  }
  
  return 0;
}

//________________________________________________________________
int GlobalReco::process_event(PHCompositeNode *topNode)
{
  // Fill common variables
  fillCommon(topNode);
  
  //  Fill any and all additional blocks that you need.
  if( TimingSN ) fillTimingSN(topNode);
  
  return EVENT_OK;
}

//________________________________________________________________
int GlobalReco::fillCommon(PHCompositeNode *topNode)
{
  
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  
  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout)
  {
    global->setBbcZVertex(bbcout->get_VertexPoint());
    global->setBbcZVertexError(bbcout->get_dVertexPoint());
    global->setBbcTimeZero(bbcout->get_TimeZero());
    global->setBbcMultNS(bbcout->get_nPmt(Bbc::North), bbcout->get_nPmt(Bbc::South));
    global->setBbcChargeNS(bbcout->get_ChargeSum(Bbc::North), bbcout->get_ChargeSum(Bbc::South));
  }
  
  SmdOut *smd = findNode::getClass<SmdOut>(topNode, "SmdOut");
  if (smd)
  {
    global->set_SmdXN(smd->get_Xpos(1));
    global->set_SmdYN(smd->get_Ypos(1));
    global->set_SmdXS(smd->get_Xpos(0));
    global->set_SmdYS(smd->get_Ypos(0));
    global->set_SmdEN(smd->get_Energy(1));
    global->set_SmdES(smd->get_Energy(0));
  }
  
  ZdcOut *zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");
  if (zdcout)
  {
    global->setZdcZVertex(zdcout->get_Zvertex());
    global->setZdcZVertexError(zdcout->get_ZvertexError());
    global->setZdcTimeZero(zdcout->get_TimeZero());
    global->setZdcEnergyNS(zdcout->get_Energy(Zdc::North), zdcout->get_Energy(Zdc::South));
  }
  
  return EVENT_OK;
}

//________________________________________________________________
int GlobalReco::fillTimingSN(PHCompositeNode *topNode)
{
  PHGlobal *global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if (bbcout)
  {
    global->setBbcTimeS( bbcout->get_Timing(Bbc::South) );
    global->setBbcTimeN( bbcout->get_Timing(Bbc::North) );
  }
  
  ZdcOut *zdcout = findNode::getClass<ZdcOut>(topNode, "ZdcOut");
  if (zdcout)
  {
    global->setZdcTimeS( zdcout->get_Timing(Zdc::South) );
    global->setZdcTimeN( zdcout->get_Timing(Zdc::North) );
  }
  
  return EVENT_OK;
}


