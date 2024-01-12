#include "SvxReactionPlaneReco.h"

// PHOOL & FUN4ALL
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <getClass.h>

//reactionplane
#include <ReactionPlaneCalibv1.h>

// DST Data
#include <PHGlobal.h>
#include <RpSumXYObject.h>
#include <RpSnglSumXY.h>

#include <ReactionPlaneObjectv4.h>

// ROOT
#include <TString.h>

// STL & std libraries
#include <iostream>
#include <cmath>


//using namespace RP;
using namespace findNode;
using namespace std;


typedef PHIODataNode <ReactionPlaneObject> ReactionPlaneObjectNode_t;

int SvxReactionPlaneReco::m_nmodule = 0;

SvxReactionPlaneReco::SvxReactionPlaneReco() : 
  Recalibrator("SvxReactionPlaneReco"),
  d_rp(NULL),
  rpcalibv1(NULL),
  m_RunNumber(-1),
  m_event(0),
  m_skip(0),
  m_nth(0)
{
  baseclasses.insert("RpSumXYObject");

  //cout<<"My SvxReactionPlaneReco"<<endl;
}

SvxReactionPlaneReco::~SvxReactionPlaneReco()
{
}

int
SvxReactionPlaneReco::isValidRun(const int runno) const
{
  // this module can be applied after run11
  if (BEGIN_OF_RUN11 <= runno) // Run11 p+p (physics) started
  {
    return 1;
  }
  return 0;
}

int
SvxReactionPlaneReco::Init(PHCompositeNode* topNode) 
{
  if(verbosity>0) cout<<"SvxReactionPlaneReco::InitRun"<<endl;

  recoConsts *rc = recoConsts::instance();
  if(rc->FlagExist("RPCALIB_VERBOSE"))
    Verbosity(rc->get_IntFlag("RPCALIB_VERBOSE"));


  m_nmodule++;
  m_nth = m_nmodule;
  if(verbosity>0) { cout<<"SvxReactionPlaneReco::Init Nmodule="<<m_nth<<" in N="<<m_nmodule<<endl; }


  return EVENT_OK;
}

int SvxReactionPlaneReco::InitRun(PHCompositeNode* topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "SvxReactionPlaneReco::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }


  m_event = 0;

  if(verbosity>0) cout<<"SvxReactionPlaneReco::InitRun"<<endl;
  CreateNodeTree(topNode);

  recoConsts *rc = recoConsts::instance();
  m_RunNumber    = rc->get_IntFlag("RUNNUMBER");

  if(rc->FlagExist("RP_SKIP_FLATTENING")){
    m_skip = rc->get_IntFlag("RP_SKIP_FLATTENING");
    cout<<"Skip Flattening = "<<m_skip<<" "<<(m_skip==0?"not skip":"do skip")<<endl;
  }

  rpcalibv1 = getClass<ReactionPlaneCalibv1>(topNode, "ReactionPlaneCalibv1");
  if(rpcalibv1==NULL){
    cerr<<"SvxReactionPlaneReco::InitRun can not find ReactionPlaneCalibv1 object in the node tree"<<endl;
    cerr<<"SvxReactionPlaneReco::InitRun !!!!  SHOULD STOP !!!!"<<endl;
    return EVENT_OK;  
  } 

  if (!rpcalibv1->isCalibrationOK()) {
    cout<<"ReactionPlaneCalib is not calibrated. "<<endl;
  }

  return 0;
}

int SvxReactionPlaneReco::process_event(PHCompositeNode* topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "SvxReactionPlaneReco::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }


  m_event++;

  if(m_skip) {
    if(verbosity>0) cout<<"SvxReactionPlaneReco::process_event skip flattening"<<endl;
    return EVENT_OK;
  }

  if (rpcalibv1==NULL || !rpcalibv1->isCalibrationOK()) { return EVENT_OK; }
  

  PHGlobal      *global = getClass<PHGlobal>(topNode, "PHGlobal");
  RpSumXYObject *sumxy  = getClass<RpSumXYObject>(topNode, "RpSumXYObject");
  
  if(!global){ if(verbosity>0) cerr<<PHWHERE<<"No PHGlobal object"     <<endl; return EVENT_OK; }
  if(!sumxy) { if(verbosity>0) cerr<<PHWHERE<<"No RpSumXYObject object"<<endl; return EVENT_OK; }


  //////////////////
  // Z_Vertex Cut & Bin
  float vertex=-9999;
  int izps = rpcalibv1->GetZVertexBin(topNode, vertex);
  if (izps<0) {
    if(verbosity>0) cout <<"no Z Vertex available ,skip the event vertex=" << vertex<<endl;
    return EVENT_OK;
  }

  int izsvx = rpcalibv1->GetZVertexBinSvx(vertex);



  //////////////////
  // Centrality Bin
  int icenttmp = (int)global->getCentrality();
  int imul = rpcalibv1->GetCentralityBin(icenttmp);
  if( imul < 0 )  return EVENT_OK;                                                                                                                          
  
  if(verbosity>0||m_event==1)
    cout<<"SvxReactionPlaneReco::process_event  doFlattening : "<<m_event<<endl;

  ///////////////////////////////////////////
  // Get calibrated Q-vector and add plane //
  {
    doFlattening(RP::ID_SVX, imul, izsvx, sumxy);
    doFlattening(RP::ID_SEG, imul, izsvx, sumxy);
    doFlattening(RP::ID_MPC, imul, izps,  sumxy);
    doFlattening(RP::ID_BBC, imul, izps,  sumxy);
    doFlattening(RP::ID_SMD, imul, izps,  sumxy);
    doFlattening(RP::ID_CNT, imul, izps,  sumxy);
    doFlattening(RP::ID_FVT, imul, izps,  sumxy);
  } 

  
  return EVENT_OK;
}

bool SvxReactionPlaneReco::CreateNodeTree(PHCompositeNode* topNode)
{
  //Find the DST node
  PHNodeIterator iter(topNode);

  PHCompositeNode* dstNode = 
    static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));
  
  
  //Find/Create SvxReactionPlane node
  ReactionPlaneObjectNode_t* rpNode = 
    static_cast<ReactionPlaneObjectNode_t*>(iter.findFirst("PHIODataNode", "ReactionPlaneObject"));
  if(!rpNode) {
    d_rp = new ReactionPlaneObjectv4;
    rpNode = new PHIODataNode<ReactionPlaneObject>(d_rp, "ReactionPlaneObject", "PHObject");
    dstNode->addNode(rpNode);
  }
  
  return true;
}

bool SvxReactionPlaneReco::doFlattening(const int detid, const int imul, const int iz, RpSumXYObject* sumxy)
{
  if(verbosity>0 && m_event==1){
    cout<<"SvxReactionPlaneReco::"<<__FUNCTION__<<" "<<rpcalibv1->GetDetName(detid)<<endl;
  }

  if(!rpcalibv1->GetEnableDet(detid)) return false;

  if(iz<0) return false;

  bool status = true;
  for(unsigned int ikind=0; ikind<rpcalibv1->GetDetNkind(detid); ikind++)
    {
      if( !rpcalibv1->isUsedQvectorForAnalysis(detid, ikind) ) continue;
  
      for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++)
        {
          status &= applyFlattening(detid, ikind, ihar, imul, iz, sumxy);
        } // ihar loop
    } // ikind loop 

  return status;
}

bool SvxReactionPlaneReco::applyFlattening(int detid, int ikind, int ihar, int imul, int iz, RpSumXYObject *sumxy)
{
  if(sumxy==NULL) { 
    if(verbosity>0) cerr<<PHWHERE<<"No RpSumXYObject"<<endl;
    return false; 
  }

  int idcode = RP::calcIdCode(detid, ikind, ihar);
  RpSnglSumXY *qvect = sumxy->getRpSumXY(idcode);
  if(qvect==NULL) {
    if(verbosity>0) cerr<<PHWHERE<<"No Qvector :"<<endl;
    return false;
  }
  
  float qxw = qvect->QVector(0); // After re-centering
  float qyw = qvect->QVector(1); // After re-centering
  float qw  = qvect->Weight();   // After re-centering
  
  // flattening
  if(qw>0.&&qxw>-9990.&&qyw>-9999.){
    float psiRaw = atan2(qyw, qxw) / (ihar+1.0);
    float psi    = rpcalibv1->Flattening(detid, ikind, ihar, imul, iz, psiRaw); // must change, split idet to detid + ikind
  
    // save to DST
    TString sname = qvect->Name();
    sname.ReplaceAll("sum", "rp");
    d_rp->AddReactionPlane(sname, idcode, psi);
  }
  else {
    if(verbosity>0) 
      cerr<<" invalid Qvec qx:qy:qw = "<<qxw<<" "<<qyw<<" "<<qw<<" "
          <<rpcalibv1->GetDetName(detid)<<" "<<ikind<<" "<<ihar<<endl;
  }

  return true;
}
