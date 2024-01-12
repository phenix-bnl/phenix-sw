#include "SvxReactionPlaneMergeReco.h"

// PHOOL & Fun4All
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <getClass.h>

// ReactionPlane
#include <RpConst.h>
#include <ReactionPlaneCalibv1.h>

// DST Data
#include <PHGlobal.h>
#include <RpSumXYObject.h>
#include <RpSumXYObjectv3.h>
#include <RpSnglSumXY.h>


#include <TString.h>

// C++ lib
#include <iostream>

using namespace findNode;
using namespace std;

int SvxReactionPlaneMergeReco::m_nmodule = 0;

SvxReactionPlaneMergeReco::SvxReactionPlaneMergeReco(): 
  
  Recalibrator("SvxReactionPlaneMergeReco"),
  rpcalibv1(NULL),
  m_RunNumber(0),
  m_event(0),
  m_skip(0),
  m_timer(PHTimeServer::get()->insert_new("SvxReactionPlaneMergeReco")),
  m_nth(0)
{
  baseclasses.insert("RpSumXYObject");
}

int SvxReactionPlaneMergeReco::isValidRun(const int runno) const
{
  // this module can be applied after run11
  if (BEGIN_OF_RUN11 <= runno) // Run11 p+p (physics) started
    {
      return 1;
    } 
  return 0;
}

int SvxReactionPlaneMergeReco::Init(PHCompositeNode* topNode)
{
  recoConsts *rc = recoConsts::instance();
  
  if(rc->FlagExist("RPCALIB_VERBOSE"))
    Verbosity(rc->get_IntFlag("RPCALIB_VERBOSE"));

  if(verbosity>0)
    cout<<"SvxReactionPlaneMergeReco::Init"<<" "<<Name()<<endl;
 

  m_nmodule++;
  m_nth = m_nmodule;
  if(verbosity>0) { cout<<"SvxReactionPlaneMergeReco::Init Nmodule="<<m_nth<<" in N="<<m_nmodule<<endl; }

  ////////////////////////////
  return EVENT_OK;
}

int SvxReactionPlaneMergeReco::InitRun(PHCompositeNode* topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "SvxReactionPlaneMergeReco::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  m_event = 0;
  
  recoConsts *rc = recoConsts::instance();
  m_RunNumber = rc->get_IntFlag("RUNNUMBER");
  
  if(rc->FlagExist("RPCALIB_VERBOSE"))
    Verbosity(rc->get_IntFlag("RPCALIB_VERBOSE"));
  
  ///////////////////////
  rpcalibv1 = getClass<ReactionPlaneCalibv1>(topNode, "ReactionPlaneCalibv1");
  if(rpcalibv1==NULL){
    cerr<<"SvxReactionPlaneReco::InitRun can not find ReactionPlaneCalibv1 object in the node tree"<<endl;
    cerr<<"SvxReactionPlaneReco::InitRun !!!!  SHOULD STOP !!!!"<<endl;
    return EVENT_OK;   
  }
  if(! rpcalibv1->isCalibrationOK()){
    cerr<<"ReactionPlaneCalibv1 is not calibrated."<<endl;
  }
  
  CreateNodeTree(topNode);
  
  ////////////////////////////
  return EVENT_OK;
}

int SvxReactionPlaneMergeReco::CreateNodeTree(PHCompositeNode* topNode)
{
  if(verbosity>0)
    cout<<"SvxReactionPlaneMergeReco::CreateNodeTree"<<" "<<Name()<<endl;
 
  //////////////////////
  // Find the DST node so we can put objects there
  PHNodeIterator iter(topNode);
  
  PHCompositeNode* dstNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
  if (!dstNode) { cerr << PHWHERE << "DST node missing, doing nothing." << endl; return ABORTEVENT; }
  
  // Find/Create SvxRpSumXY node
  PHIODataNode<PHObject>* SvxRpSumXYNode 
    = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "RpSumXYObject");
  
  if(SvxRpSumXYNode==NULL){
    if(verbosity>0)
      cout<<"SvxReactionPlaneMergeReco::CreateNodeTree"<<" No RpSumXYObjectNode. Create."<<Name()<<endl;

    RpSumXYObject* rpsumxy = new RpSumXYObjectv3();
    
    SvxRpSumXYNode = new PHIODataNode<PHObject>(rpsumxy, "RpSumXYObject", "PHObject");
    
    dstNode->addNode(SvxRpSumXYNode);
  }

  return EVENT_OK;
}

int SvxReactionPlaneMergeReco::process_event(PHCompositeNode* topNode)
{  
  if(m_nth>1) {
    if(verbosity>0) cout << "SvxReactionPlaneMergeReco::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  m_timer.get()->restart();

//  static int initialize = 0;
//  if(initialize==0)
//  {
//    CreateNodeTree(topNode);
//    initialize = 1;
//  }
  
  //VTX,BBC,MPC,
  //----------------------------------
  //  PHGlobal       *global  = getClass<PHGlobal>(topNode, "PHGlobal");
  RpSumXYObject  *sumxy     = getClass<RpSumXYObject>(topNode, "RpSumXYObject");
  if(!sumxy)    { if(verbosity>0) cerr<<PHWHERE<<"No RpSumXYObject object"   <<endl; return EVENT_OK;}
  m_event++;
  
  
  TString s_rpname[4] = {"CntRpSumXYObject", "SvxRpSumXYObject", "MuonRpSumXYObject", "FvtxRpSumXYObject"};
  RpSumXYObject* sumxy_dummy[4];
  for(int irp=0; irp<4; irp++)
    {
    sumxy_dummy[irp] = getClass<RpSumXYObject>(topNode, s_rpname[irp].Data() );
    }
  
  if(verbosity>0&&m_event==1)
    cout<<"SvxReactionPlaneMergeReco::process_event  merging Qvectors "<<m_event<<endl;
  //////////////////////////////
  for(int irp=0; irp<4; irp++)
  {
    if(sumxy_dummy[irp] !=NULL) mergeRpSumXY(sumxy, sumxy_dummy[irp]);
    else {
      if(verbosity>0&&m_event==1)
        cout<<"RpSumXYObject is NULL. skip this : "<<s_rpname[irp].Data()<<endl;
    }
  }

  //////////////////
  m_timer.get()->stop();
  
  return EVENT_OK;
}


int SvxReactionPlaneMergeReco::mergeRpSumXY(RpSumXYObject* sumxy, RpSumXYObject  *cntsumxy)
{
  if(sumxy==NULL  || cntsumxy==NULL){
      return -1;
  }
  
  ////////////////////
  
  for(unsigned int idetect=0; idetect<rpcalibv1->GetNDet(); idetect++){ 
    int detid = rpcalibv1->getDetId(idetect);
    
    
    for(unsigned int ikind=0; ikind<rpcalibv1->GetDetNkind(detid); ikind++){
      
      for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++){
	
        int detkind  = ikind;	
	int idcode   = RP::calcIdCode(detid, detkind, ihar);
	
        RpSnglSumXY *qvect  = sumxy->getRpSumXY(idcode);
	RpSnglSumXY *cqvect = cntsumxy->getRpSumXY(idcode);
	
        if( cqvect == NULL) continue;
	
	if(qvect == 0){
	  
	  sumxy->AddRpSumXY(cqvect->Name(), idcode, cqvect->QVector(0) ,cqvect->QVector(1), cqvect->Weight());
	  
	}
	
      }}}
  return 0;
  
}
