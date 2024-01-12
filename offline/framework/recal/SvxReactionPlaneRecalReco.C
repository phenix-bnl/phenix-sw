#include "SvxReactionPlaneRecalReco.h"

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
#include <RpSnglSumXY.h>

#include <SvxSegmentList.h>
#include <SvxSegment.h>

// C++ lib
#include <iostream>

using namespace findNode;
using namespace std;

int SvxReactionPlaneRecalReco::m_nmodule = 0;

SvxReactionPlaneRecalReco::SvxReactionPlaneRecalReco(): 
  Recalibrator("SvxReactionPlaneRecalReco"),
  rpcalibv1(NULL),
  m_RunNumber(0),
  m_event(0),
  m_skip(0),
  m_correctForPro98(false),
  m_nth(0)
{
  baseclasses.insert("RpSumXYObject");

  //cout<<"My SvxReactionPlaneRecalReco"<<endl;
}

int SvxReactionPlaneRecalReco::isValidRun(const int runno) const
{
  // this module can be applied after run11
  if (BEGIN_OF_RUN11 <= runno) // Run11 p+p (physics) started
  {
    return 1;
  } 
  return 0;
}

int SvxReactionPlaneRecalReco::Init(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  if(rc->FlagExist("RPCALIB_VERBOSE"))
    Verbosity(rc->get_IntFlag("RPCALIB_VERBOSE"));


  m_nmodule++;
  m_nth = m_nmodule;
  if(verbosity>0) { cout<<"SvxReactionPlaneRecalReco::Init Nmodule="<<m_nth<<" in N="<<m_nmodule<<endl; }

  if(verbosity>0) cout << "SvxReactionPlaneRecalReco::Init() Execution completed." << endl;
  return EVENT_OK;
}

int SvxReactionPlaneRecalReco::InitRun(PHCompositeNode* topNode)
{
  if(m_nth>1) {
    if(verbosity>0) cout << "SvxReactionPlaneRecalReco::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  m_event = 0;

  //cout << "svx recal reco init run" << endl;
  recoConsts *rc = recoConsts::instance();
  m_RunNumber = rc->get_IntFlag("RUNNUMBER");

  if(rc->FlagExist("RP_SKIP_RECENTERING")){
    m_skip = rc->get_IntFlag("RP_SKIP_RECENTERING");
    cout<<"Skip Recentering = "<<m_skip<<" "<<(m_skip==0?"not skip":"do skip")<<endl;
  }

  if(rc->FlagExist("RP_CORRECT_PRO98")){
    m_correctForPro98 = (rc->get_IntFlag("RP_CORRECT_PRO98")!=0);
    cout<<"  CorrectForPro98 functions = "<<(m_correctForPro98 ? "enable":"disable")<<endl;
  }

  if(rc->FlagExist("RPCALIB_VERBOSE"))
    Verbosity(rc->get_IntFlag("RPCALIB_VERBOSE"));


  rpcalibv1 = getClass<ReactionPlaneCalibv1>(topNode, "ReactionPlaneCalibv1");
  if(rpcalibv1==NULL){
    cerr<<"SvxReactionPlaneReco::InitRun can not find ReactionPlaneCalibv1 object in the node tree"<<endl;
    cerr<<"SvxReactionPlaneReco::InitRun !!!!  SHOULD STOP !!!!"<<endl;
    return EVENT_OK;   
  }
  if(! rpcalibv1->isCalibrationOK()){
    cerr<<"ReactionPlaneCalibv1 is not calibrated."<<endl;
  }
  
  //cout << " SvxReactionPlane RecalReco " << name << endl;
  return EVENT_OK;
}

int SvxReactionPlaneRecalReco::process_event(PHCompositeNode* topNode)
{  
  if(m_nth>1) {
    if(verbosity>0) cout << "SvxReactionPlaneRecalReco::InitRun() nth module="<<m_nth<<". skip." << " "<< Name()<<endl;
    return EVENT_OK;
  }

  //VTX,BBC,MPC,
  //----------------------------------
  PHGlobal       *global  = getClass<PHGlobal>(topNode, "PHGlobal");
  RpSumXYObject  *sumxy   = getClass<RpSumXYObject>(topNode, "RpSumXYObject");
  SvxSegmentList *seglist = getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  
  if(!global){ if(verbosity>0) cerr<<PHWHERE<<"No PHGlobal object"     <<endl; return EVENT_OK;}
  if(!sumxy) { if(verbosity>0) cerr<<PHWHERE<<"No RpSumXYObject object"<<endl; return EVENT_OK;}
  
  m_event++;
  
  if(verbosity>0||m_event==1)
    cout<<"SvxReactionPlaneRecalReco::process_event  combining Qvectors using eta slices "<<m_event<<endl;

  // apply bugcorrection for pro98 library
  if(m_correctForPro98)
  {
    correctSvxPro98(sumxy); // correction for cluster Qvector
    correctSegPro98(sumxy); // correction for segment Qvector
  }

  // Put additional Qvectors into DST node
  {
    AddSvxRpSumXY( sumxy );
    AddSegRpSumXY( sumxy, seglist );
    AddFvtxRpSumXY(sumxy );
  }

  
  //////////////////
  // Z_Vertex Cut & Bin
  float vertex=-9999;
  int izps = rpcalibv1->GetZVertexBin(topNode, vertex);
  if (izps<0) {
    if(verbosity>0) cout <<"no Z Vertex available ,skip the event  vertex=" << vertex<< endl;
    return EVENT_OK;
  }
  int izsvx = rpcalibv1->GetZVertexBinSvx(vertex);

  /////////////////
  // Centrality Bin
  const int icenttmp = (int)global->getCentrality();
  int imul = rpcalibv1->GetCentralityBin(icenttmp);
  if( imul < 0 )  return EVENT_OK;


  ////////////////////////////////////////////////////////
  // recentering 
  if(m_skip){
    if(verbosity>0) cout<<"SvxReactionPlaneRecalReco::process_event  skip recentering"<<endl;
    return EVENT_OK;
  }


  // if calib isnot initialized, return!
  if( rpcalibv1==NULL || !rpcalibv1->isCalibrationOK() ){ return EVENT_OK; }
  

  if(verbosity>0||m_event==1)
    cout<<"SvxReactionPlaneRecalReco::process_event  doRecentering : "<<m_event<<endl;
  ///////////////////////////////////////
  { // recentering
    doRecentering(RP::ID_SVX, imul, izsvx, sumxy);
    doRecentering(RP::ID_SEG, imul, izsvx, sumxy);
    doRecentering(RP::ID_MPC, imul, izps,  sumxy);
    doRecentering(RP::ID_BBC, imul, izps,  sumxy);
    doRecentering(RP::ID_SMD, imul, izps,  sumxy);
    doRecentering(RP::ID_CNT, imul, izps,  sumxy);
    doRecentering(RP::ID_FVT, imul, izps,  sumxy);
  }
    
  return EVENT_OK;
}

int SvxReactionPlaneRecalReco::AddSvxRpSumXY( RpSumXYObject  *sumxy )
{
  if(sumxy==NULL){ return -1; }

  //---------------------------------------------------------------------------
  //eta  -3.0  -2.5  -2.0  -1.5  -1.0  -0.5  0.0  0.5  1.0  1.5  2.0  2.5  3.0
  //etabin   36    37    38    39    40    41   42   43   44   45   46   47
  //ebin      0     1     2     3     4     5    6    7    8    9   10   11
  //---------------------------------------------------------------------------

  //Add SVX Qvector S / N
  static const int DETID    = RP::ID_SVX;
  static const int NSVX     = 53;
  static const int NQ_SLICE = 12;
  static const int NQ_ADD   = 12;
  static const int NHAR     = RP::NHAR4;


  if(!rpcalibv1->GetEnableDet(DETID)) return -1;

  float  Qadd[NQ_ADD][NHAR][3];
  memset(Qadd, 0, sizeof(Qadd));

  for(int ikind=0; ikind<NQ_SLICE; ikind++){
    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++){
      int detkind  = ikind+36; // detid: 36-47
      int idcode = RP::calcIdCode(DETID, detkind, ihar);

      RpSnglSumXY *qvect = sumxy->getRpSumXY(idcode);
      if(qvect==NULL) continue;

      float Q[3] = {qvect->QVector(0), qvect->QVector(1), qvect->Weight()};
      if(Q[2]<=0.) continue;

      for(int ico=0; ico<3; ico++){
        //SVX South
        if(ikind<8){ Qadd[ 0][ihar][ico] += Q[ico]; } //SVX eta< 1.0 
        if(ikind<7){ Qadd[ 2][ihar][ico] += Q[ico]; } //SVX eta< 0.5 
        if(ikind<6){ Qadd[ 4][ihar][ico] += Q[ico]; } //SVX eta< 0
        if(ikind<5){ Qadd[ 6][ihar][ico] += Q[ico]; } //SVX eta<-0.5
        if(ikind<4){ Qadd[ 9][ihar][ico] += Q[ico]; } //SVX eta<-1

        // SVX North
        if(3<ikind){ Qadd[ 1][ihar][ico] += Q[ico]; } //SVX eta>-1.0
        if(4<ikind){ Qadd[ 3][ihar][ico] += Q[ico]; } //SVX eta>-0.5 
        if(5<ikind){ Qadd[ 5][ihar][ico] += Q[ico]; } //SVX eta>0.0
        if(6<ikind){ Qadd[ 7][ihar][ico] += Q[ico]; } //SVX eta>0.5
        if(7<ikind){ Qadd[10][ihar][ico] += Q[ico]; } //SVX eta>1.0

        // SVX South+North
        if(ikind<5||6<ikind){ Qadd[ 8][ihar][ico] += Q[ico]; } //SVX eta_gap=1
        if(ikind<4||7<ikind){ Qadd[11][ihar][ico] += Q[ico]; } //SVX eta_gap=2
      } // loop ico
    } // loop ihar
  } // loop idet
  
  char name[100];
  for( int ikind=0; ikind<NQ_ADD; ikind++ ){
    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++ ){
      if(Qadd[ikind][ihar][2]<=0.) continue;

      int detkind  = NSVX + ikind;
      int idcode = RP::calcIdCode(DETID, detkind, ihar); 
      sprintf(name, "SVXsum%02d%u", detkind, ihar);
      sumxy->AddRpSumXY(name, idcode, Qadd[ikind][ihar][0], Qadd[ikind][ihar][1], Qadd[ikind][ihar][2]);
    }
  }
  
  return 0;
}

int SvxReactionPlaneRecalReco::AddSegRpSumXY( RpSumXYObject  *sumxy, SvxSegmentList *seglist )
{
  if(sumxy==NULL){ return -1; }

  //---------------------------------------------------------------
  //eta   -2.0  -1.5  -1.0  -0.5  0.0  0.5  1.0  1.5  2.0  all
  //ebin      0     1     2     3    4    5    6    7       8
 
  //---------------------------------------------------------------

  //Add SVX Qvector S / N
  static const int DETID    = RP::ID_SEG;
  static const int NSEG     = 9;
  static const int NQ_SLICE = 8;
  static const int NQ_ADD   = 2; // d_eta=1, d_eta=2
  static const int NHAR     = RP::NHAR4;


  if(!rpcalibv1->GetEnableDet(DETID)) return -1;


  { // combine q-vectors
    float  Qadd[NQ_ADD][NHAR][3];
    memset(Qadd, 0, sizeof(Qadd));

    for(int ikind=0; ikind<NQ_SLICE; ikind++){
      for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++){
        int detkind  = ikind;
        int idcode = RP::calcIdCode(DETID, detkind, ihar);

        RpSnglSumXY *qvect = sumxy->getRpSumXY(idcode);
        if(qvect==NULL) continue;

        float Q[3] = {qvect->QVector(0), qvect->QVector(1), qvect->Weight()};
        if(Q[2]<=0.) continue;

        for(int ico=0; ico<3; ico++){
          // SVX South+North
          if(ikind<3||4<ikind){ Qadd[0][ihar][ico] += Q[ico]; } //SVX eta_gap=1
          if(ikind<2||6<ikind){ Qadd[1][ihar][ico] += Q[ico]; } //SVX eta_gap=2
        } // loop ico
      } // loop ihar
    } // loop idet
    

    char name[100];
    for( int ikind=0; ikind<NQ_ADD; ikind++ ){
      for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++ ){
        if(Qadd[ikind][ihar][2]<=0.) continue;

        int detkind  = NSEG + ikind;
        int idcode = RP::calcIdCode(DETID, detkind, ihar); 
        RpSnglSumXY *obj = sumxy->getRpSumXY(idcode);
        if(obj!=NULL){ // this is due to bug in pro.98
          obj->QVector(0, Qadd[ikind][ihar][0]);
          obj->QVector(1, Qadd[ikind][ihar][1]);
          obj->Weight(    Qadd[ikind][ihar][2]);
        }
        else {
          sprintf(name, "SVXsum%02d%u", detkind, ihar);
          sumxy->AddRpSumXY(name, idcode, Qadd[ikind][ihar][0], Qadd[ikind][ihar][1], Qadd[ikind][ihar][2]);
        }
      }
    }
  }

  ///////////////////////////////////////////
  // calculate segment Q vector with pT weight
  if(seglist==NULL) return -1;

  { // pT weighted Qvector
    static const int NQ_ADD1 = 3; // d_eta=0, d_eta=1, d_eta=2
    float  Qadd1[NQ_ADD1][NHAR][3];
    memset(Qadd1, 0, sizeof(Qadd1));

    for(int iseg=0; iseg<seglist->get_nSegments(); iseg++)
    {
      SvxSegment *seg = seglist->get_segment(iseg);
      if(seg==NULL) continue;

      float px = seg->get3MomentumAtPrimaryVertex(0);
      float py = seg->get3MomentumAtPrimaryVertex(1);
      float pz = seg->get3MomentumAtPrimaryVertex(2);
      
      float phi = atan2(py, px);
      float pt  = sqrt( px*px + py*py );
      float the = atan2(pt, pz);
      float eta = -log( tan(0.5*the) );

      if(eta<=-2.0||2.0<eta) continue;

      bool etaOK[2] = {(eta<=-0.5||0.5<eta), 
                       (eta<=-1.0||1.0<eta)};

      float w = (0.2<=pt&&pt<2) ? pt : 0; // pT weight for Qvector
      for(int ihar=0; ihar<NHAR; ihar++){
        float qx =cos( phi*(ihar+1.0) );
        float qy =sin( phi*(ihar+1.0) );

        Qadd1[0][ihar][0]+=(w*qx);
        Qadd1[0][ihar][1]+=(w*qy);
        Qadd1[0][ihar][2]+= w;

        for(int iOK=0; iOK<2; iOK++){
          if( etaOK[iOK] ){
            Qadd1[iOK+1][ihar][0]+=(w*qx);
            Qadd1[iOK+1][ihar][1]+=(w*qy);
            Qadd1[iOK+1][ihar][2]+= w;
          }
        }
      } // ihar loop
    } // iseg loop

    char name[100];
    for( int ikind=0; ikind<NQ_ADD1; ikind++ ){
      for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++ ){
        if(Qadd1[ikind][ihar][2]<=0.) continue;

        int detkind  = NSEG + NQ_ADD + ikind;
        int idcode = RP::calcIdCode(DETID, detkind, ihar); 
        RpSnglSumXY *obj = sumxy->getRpSumXY(idcode);
        if(obj!=NULL){ // this is due to bug in pro.98
          obj->QVector(0, Qadd1[ikind][ihar][0]);
          obj->QVector(1, Qadd1[ikind][ihar][1]);
          obj->Weight(    Qadd1[ikind][ihar][2]);
        }
        else {
          sprintf(name, "SVXsum%02d%u", detkind, ihar);
          sumxy->AddRpSumXY(name, idcode, Qadd1[ikind][ihar][0], Qadd1[ikind][ihar][1], Qadd1[ikind][ihar][2]);
        }
      }
    }
  }
  
  return 0;
}

int SvxReactionPlaneRecalReco::AddFvtxRpSumXY(RpSumXYObject* sumxy )
{
  if(sumxy==NULL){
    return -1;
  }

  ////////////////////
  //  additional q-vector classe = 5*3*2 = 30
  //  layer(all, 0,1,2,3) * arm(both, south, north) * eta(>1.0, >1.5)

  static const int DETID = RP::ID_FVT;
  static const unsigned int NADD = 30;
  static const unsigned int NHAR = RP::NHAR4;

  if(!rpcalibv1->GetEnableDet(DETID)) return -1;


  float Qadd[NADD][NHAR][3];
  memset(Qadd, 0, sizeof(Qadd));

  for(unsigned int ikind=0; ikind<40; ikind++){//
    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++){
      int   iarm      = ikind/20; //0,1
      int   ikind_arm = ikind%20; //0-19
      int   istation  = ikind_arm/5;//0,1,2,3
      int   igap      = ikind_arm%5;//0,1,2,3,4

      int   id       = RP::ID_FVT;
      int   idcode   = RP::calcIdCode(id, ikind, ihar);
      RpSnglSumXY *qvect = sumxy->getRpSumXY(idcode);
      if(qvect==NULL) continue;

      float q[3] = {qvect->QVector(0), qvect->QVector(1), qvect->Weight()};

      bool good_qv  = q[2]>0.0;
      bool good_qv2 = q[0]>-9000. && q[1]>-9000. && q[2]>-9000;

      if(!good_qv2) {
        cout<<PHWHERE<<" fail : "<<q[0]<<" "<<q[1]<<" "<<q[2]<<endl;
        continue;
      }

      if(!good_qv) continue;

      for(int iq=0; iq<3; iq++){
        Qadd[iarm+0               ][ihar][iq] += q[iq]; //South + North 10 ,11, 12, 13
        Qadd[     2               ][ihar][iq] += q[iq]; //South + North 14

        Qadd[iarm+0+6*(istation+1)][ihar][iq] += q[iq]; //South + North 10 ,11, 12, 13
        Qadd[     2+6*(istation+1)][ihar][iq] += q[iq]; //South + North 14

        if(igap>0){
          //---Eta>1.1------
          Qadd[iarm+3+0               ][ihar][iq] += q[iq]; //South + North 10 ,11, 12, 13
          Qadd[     3+2               ][ihar][iq] += q[iq]; //South + North 14

          Qadd[iarm+3+0+6*(istation+1)][ihar][iq] += q[iq]; //South + North 10 ,11, 12, 13
          Qadd[     3+2+6*(istation+1)][ihar][iq] += q[iq]; //South + North 14

        }
      }

/*
      for(int iq=0; iq<3; iq++){
        Qadd[iarm*5 + istation][ihar][iq] += q[iq]; //South 0,1,2,3 / North 5, 6, 7, 8 
        Qadd[iarm*5 +    4    ][ihar][iq] += q[iq]; //South 4 / North 9
        Qadd[   2*5 + istation][ihar][iq] += q[iq]; //South + North 10 ,11, 12, 13
        Qadd[   2*5 +    4    ][ihar][iq] += q[iq]; //South + North 14
        if(igap>0){
          //---Eta>1.1------
          Qadd[iarm*5 + istation + 15][ihar][iq] += q[iq]; //South 15,16,17,18 / North 20,21,22,23
          Qadd[iarm*5 +    4     + 15][ihar][iq] += q[iq]; //South 19 / North 24 
          Qadd[   2*5 + istation + 15][ihar][iq] += q[iq]; //South + North 25, 26, 27, 28
          Qadd[   2*5 +    4     + 15][ihar][iq] += q[iq]; //South + North 29
        }
      }
*/
    }
  }

  char name[100];
  for(unsigned int ikind=0; ikind<NADD; ikind++){
    for(unsigned int ihar = 0; ihar < rpcalibv1->GetDetNhar(DETID); ihar++ ){
      if(Qadd[ikind][ihar][2]<=0.) continue;

      int detkind = ikind + RP::NFVT;
      int idcode  = RP::calcIdCode(DETID, detkind, ihar); 
      sprintf(name, "FVTsum%02d%u", detkind, ihar);
      sumxy->AddRpSumXY(name, idcode, Qadd[ikind][ihar][0], Qadd[ikind][ihar][1], Qadd[ikind][ihar][2]);
    }
  }

  return 0;
}

bool SvxReactionPlaneRecalReco::doRecentering(
  const int detid, const int imul, const int iz, RpSumXYObject *sumxy)
{
  if(verbosity>0 && m_event==1){
    cout<<"SvxReactionPlaneRecalReco::"<<__FUNCTION__<<" "<<rpcalibv1->GetDetName(detid)<<endl;
  }

  if(!rpcalibv1->GetEnableDet(detid)) return false;

  if(iz<0) return false;

  bool status=true;

  for(unsigned int ikind=0; ikind<rpcalibv1->GetDetNkind(detid); ikind++)
    {
      if( !rpcalibv1->isUsedQvectorForAnalysis(detid, ikind) ) continue;

      for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++)
        {
          status &= applyRecentering(detid, ikind, ihar, imul, iz, sumxy);
        }
    }

  return status;
}

bool SvxReactionPlaneRecalReco::applyRecentering(
      int detid,// DetectorID
      unsigned int ikind,// kindID
      unsigned int ihar, // HarmonicsID
      int icent,// icentrality bin
      int iz,   // izvertex bin
      RpSumXYObject *sumxy)
{
  if(sumxy==NULL){
    if(verbosity>0) {cerr<<"No RpSumXYObject :"<<detid<<" "<<ikind<<" "<<ihar<<endl;}
    return false;
  }

  int idcode = RP::calcIdCode(detid, ikind, ihar);
  RpSnglSumXY *qvect = sumxy->getRpSumXY(idcode);
  if(qvect==NULL)
    {
      if(verbosity>0) {cerr<<"No Qvector :"<<detid<<" "<<ikind<<" "<<ihar<<endl;}
      return false;
    }
  
  float qx = qvect->QVector(0);
  float qy = qvect->QVector(1);
  float qw = qvect->Weight();
  
  float qx_re = -9999.;
  float qy_re = -9999.;
  if(qw>0.0){
    //float qxw = qx/qw;
    //float qyw = qy/qw;
    qx_re = rpcalibv1->Recentering(detid, ikind, ihar, icent, iz, qx, qw, 0);
    qy_re = rpcalibv1->Recentering(detid, ikind, ihar, icent, iz, qy, qw, 1);
  }
  
  // set recentered Q vector
  qvect->QVector(0, qx_re);
  qvect->QVector(1, qy_re);

  // debug out
  //-if(detid==RP::ID_SVX&&ikind==52&&ihar==0&&icent==0&&iz==0){
  //-  float xmean = rpcalibv1->GetSumXmean( detid, ikind, ihar, icent, iz);
  //-  float xsig  = rpcalibv1->GetSumXsigma(detid, ikind, ihar, icent, iz);
  //-  float ymean = rpcalibv1->GetSumYmean( detid, ikind, ihar, icent, iz);
  //-  float ysig  = rpcalibv1->GetSumYsigma(detid, ikind, ihar, icent, iz);
  //-  cout<<qx<<" "<<qy<<" "<<qw<<" : "<<qx_re<<" "<<qy_re<<" : "<<xmean<<" "<<xsig<<" "<<ymean<<" "<<ysig<<endl;
  //-}

  return true;
}

int SvxReactionPlaneRecalReco::correctSvxPro98(RpSumXYObject* sumxy ) // correction for cluster Qvector
{
//  cout<<__FUNCTION__<<endl;

  if(sumxy==NULL){ return -1; }

  //---------------------------------------------------------------------------
  //eta  -3.0  -2.5  -2.0  -1.5  -1.0  -0.5  0.0  0.5  1.0  1.5  2.0  2.5  3.0
  //etabin   36    37    38    39    40    41   42   43   44   45   46   47
  //ebin      0     1     2     3     4     5    6    7    8    9   10   11
  //---------------------------------------------------------------------------

  //Add SVX Qvector S / N
  static const int DETID    = RP::ID_SVX;
  static const int NQ_ADD   = 4;
  static const int NHAR     = RP::NHAR4;


  if(!rpcalibv1->GetEnableDet(DETID)) return -1;

  float  Qadd[NQ_ADD][NHAR][3];
  memset(Qadd, 0, sizeof(Qadd));

  for(int ikind=0; ikind<36; ikind++){
    int layer=0;
    if(      0<=ikind&&ikind<12) layer=0;
    else if(12<=ikind&&ikind<22) layer=1;
    else if(22<=ikind&&ikind<30) layer=2;
    else                         layer=3;

    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++){
      int detkind  = ikind;
      int idcode = RP::calcIdCode(DETID, detkind, ihar);

      RpSnglSumXY *qvect = sumxy->getRpSumXY(idcode);
      if(qvect==NULL) continue;

      float Q[3] = {qvect->QVector(0), qvect->QVector(1), qvect->Weight()};
      if(Q[2]<=0.) continue;

      for(int ico=0; ico<3; ico++){
        Qadd[layer][ihar][ico] += Q[ico];
      } // loop ico

    } // loop ihar
  } // loop idet
  
  char name[100];
  for( int ikind=0; ikind<NQ_ADD; ikind++ ){
    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++ ){
      if(Qadd[ikind][ihar][2]<=0.) continue;

      int detkind  = 48 + ikind;
      int idcode = RP::calcIdCode(DETID, detkind, ihar); 
      sprintf(name, "SVXsum%02d%u", detkind, ihar);

      RpSnglSumXY *object = sumxy->getRpSumXY(idcode);
      if(object==NULL) {
        //cerr<<"No RpSngl(dest) : "<<detkind<<" skip"<<endl;
        sprintf(name, "SVXsum%02d%u", detkind, ihar);
        sumxy->AddRpSumXY(name, idcode, Qadd[ikind][ihar][0], Qadd[ikind][ihar][1], Qadd[ikind][ihar][2]);
      }
      else {
        object->QVector(0, Qadd[ikind][ihar][0]);
        object->QVector(1, Qadd[ikind][ihar][1]);
        object->Weight(    Qadd[ikind][ihar][2]);
      }
    }
  }
  
  return 0;
}

int SvxReactionPlaneRecalReco::correctSegPro98(RpSumXYObject* sumxy ) // correction for segment Qvector
{ 
//  cout<<__FUNCTION__<<endl;

  if(sumxy==NULL) return -1;

  // all the segment Q vector were filled for index 53 - 61.
  // but this is bug in pro.98. 
  // To handling the bug, all the segment Q vector is copied to 0-8
  //
  static const int DETID=RP::ID_SEG;
  static const int NKIND=9;

  if(!rpcalibv1->GetEnableDet(DETID)) return -1;

  char name[100];
  for( int ikind=0; ikind<NKIND; ikind++ ){
    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(DETID); ihar++ ){
      int detkind_org = ikind + 53;
      int idcode_org  = RP::calcIdCode(DETID, detkind_org, ihar); 
      RpSnglSumXY *objorg  = sumxy->getRpSumXY(idcode_org);
      if(objorg==NULL) { /*cerr<<"No RpSngl(org)  : "<<detkind_org <<" skip"<<endl;*/ continue; }

      int detkind_dest = ikind;
      int idcode_dest  = RP::calcIdCode(DETID, detkind_dest, ihar); 
      RpSnglSumXY *objdest = sumxy->getRpSumXY(idcode_dest);
      if(objdest==NULL) {
        //cerr<<"No RpSngl(dest) : "<<detkind_dest<<" skip"<<endl;
        sprintf(name, "SVXsum%02d%u", detkind_dest, ihar);
        sumxy->AddRpSumXY(name, idcode_dest, objorg->QVector(0), objorg->QVector(1), objorg->Weight() );
      }
      else {
        objdest->QVector(0, objorg->QVector(0));
        objdest->QVector(1, objorg->QVector(1));
        objdest->Weight(    objorg->Weight( ) );
      }
    }
  }

  return 0;
}
