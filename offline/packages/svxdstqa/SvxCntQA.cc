#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include "gsl/gsl_rng.h"
#include <math.h>

#include "phool.h"
#include "PHTypedNodeIterator.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "Fun4AllReturnCodes.h"

#include "SvxCntQA.h"

#include "PHCentralTrack.h"
#include "PHTrackOut.h"
#include "PHGlobal.h"
#include "emcClusterContainer.h"
#include "emcClusterContent.h"
#include "TriggerHelper.h"
#include "VtxOut.h"
#include "ErtOut.h"
#include "PadCluster.h"
#include "CglTrack.h"
#include "DchTrack.h"
#include "CrkRing.h"
#include "RunHeader.h"
#include "EventHeader.h"
#include "TrigLvl1.h"
#include "TrigRunLvl1.h"
#include "Bbc.hh"
#include "BbcOut.h"

#include "McEvalSingleList.h"

#include "RpConst.h"
#include "RpSnglSumXY.h"
#include "RpSumXYObject.h"

#include "SvxRawhitList.h"
#include "SvxRawhit.h"
#include "SvxClusterList.h"
#include "SvxSegmentList.h"
#include "SvxRawhitClusterList.h"
#include "SvxRawhitCluster.h"
#include "SvxClusterList.h"
#include "SvxCluster.h"
#include "SvxSegment.h"
#include "SvxTracker.h"
#include "SvxCentralTrackList.h"
#include "SvxCentralTrack.h"
#include "SvxClusterInfo.h"
#include "SvxResidualInfo.h"
#include "SvxBeamCenterPar.h"

#include "SvxClusterContainer.h"

#include "svxAddress.hh"
#include "svxDetectorGeo.hh"
#include "SvxSensor.h"

#include "TFile.h"
#include "TNtuple.h"
#include "TTree.h"
#include "TF1.h"
#include "TH1.h"
#include "TH2.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "TRandom2.h"
#include "TMath.h"


#include "getClass.h"

using namespace std;
using namespace findNode;

const float SvxCntQA::GOODZVTX      = 10.0; // +-8cm
const float SvxCntQA::GOODEMCDPHI   = 0.03; // +-0.02 rad in emcdphi
const float SvxCntQA::GOODEMCDZ     = 12.0; // +-8.0 cm in emcdz
const int   SvxCntQA::GOODDCQUALITY = 4;    // require dcqual>=4
const float SvxCntQA::PTTHRESHOLD   = 1.0;  // pt threshold 1GeV/c
const float SvxCntQA::PTUPPERLIMIT  = 6.0;  // pt upper limit


union val_convert {
  int ival;
  float fval;
};

class residual_data{
  public:
    residual_data(float dp_val, float dz_val): dphi(dp_val), dz(dz_val){};
    float dphi;
    float dz;

    static bool compare(const residual_data& d1, const residual_data& d2){
      // d1 has smaller absolute value, then return "TRUE"
      return (fabs(d1.dphi) < fabs(d2.dphi));
    }

    void print(){ cout<<dphi<<" "<<dz<<endl; }
};

void print_resdata(residual_data& d){ d.print(); };

//==============================================================

SvxCntQA::SvxCntQA() : SubsysReco("SVXCNTQA")
{
  //ThisName = "SvxCntQA";
  init_ana=0;
  EventNumber=0;
  OutputFileName="svxanalysis.root";

  m_svxAdrObj = NULL;
  m_svxGeo    = NULL;

  m_simmode=0;
  m_goodOnlyFlag=false;
  m_lowpt_ctr[0]=0;
  m_lowpt_ctr[1]=0;
  m_midpt_ctr[0]=0;
  m_midpt_ctr[1]=0;

  m_beam_x = 0.0;
  m_beam_y = 0.0;

  //////////////////////
  random = NULL;
  OutputNtupleFile = NULL;
  ntpsvxkalfit    = NULL;
  ntpsvxrawhit    = NULL;
  ntpsvxcluster   = NULL;
  ntpsvxsegment   = NULL;
  ntprawclus      = NULL;
  ntp_trk         = NULL;
  ntp_cnt         = NULL;
  ntp_cnt_clus    = NULL;
  ntp_cnt_clus1   = NULL;
  ntp_evt_pxlchip = NULL;

/*
  hdphi           = NULL;
  hdphi2          = NULL;
  hdphi3          = NULL;
  hdphi3a         = NULL;
  hdphi3b         = NULL;
  hdthe           = NULL;
  hdthe2          = NULL;
  hdphidthe       = NULL;
  hsvxsegmentmult = NULL;
  hsvxclustermult = NULL;
  hsvxrawhitmult  = NULL;
  hadc2X          = NULL;
  hpix_chip_mult  = NULL;
  hzvtx           = NULL;
  h2pixel         = NULL;
  h2pixel2        = NULL; //nhit<30
  h2pixel3        = NULL; //nchip<5
*/
  
  ntpevt          = NULL;
  m_ntp_cnttrk    = NULL;
  m_ntp_cnttrk_bg = NULL;
}

//==============================================================

SvxCntQA::SvxCntQA(string filename) : OutputFileName(filename)
{
  ThisName = "SvxCntQA";
  init_ana=0;
  EventNumber=0;

  m_svxAdrObj = NULL;
  m_svxGeo    = NULL;

  m_simmode=0;
  m_goodOnlyFlag=false;
  m_lowpt_ctr[0]=0;
  m_lowpt_ctr[1]=0;
  m_midpt_ctr[0]=0;
  m_midpt_ctr[1]=0;

  m_beam_x = 0.0;
  m_beam_y = 0.0;

  m_fieldScale=1;

//   m_svxAdrObj = NULL;
//   // hotchannel 
//   for(int imod=0; imod<NMODULE; imod++){
//     for(int ichip=0; ichip<NCHIP; ichip++){
//       for(int icol=0; icol<NCOLUMN; icol++){
//         for(int irow=0; irow<NROW; irow++){
//           m_hotchannel[imod][ichip][icol][irow]=false;
//         }
//       }
//     }
//   }
  //cluscont = new SvxClusterContainer();

  /////////////////////////
  random = NULL;
  OutputNtupleFile = NULL;
  ntpsvxkalfit    = NULL;
  ntpsvxrawhit    = NULL;
  ntpsvxcluster   = NULL;
  ntpsvxsegment   = NULL;
  ntprawclus      = NULL;
  ntp_trk         = NULL;
  ntp_cnt         = NULL;
  ntp_cnt_clus    = NULL;
  ntp_cnt_clus1   = NULL;
  ntp_evt_pxlchip = NULL;

/*
  hdphi           = NULL;
  hdphi2          = NULL;
  hdphi3          = NULL;
  hdphi3a         = NULL;
  hdphi3b         = NULL;
  hdthe           = NULL;
  hdthe2          = NULL;
  hdphidthe       = NULL;
  hsvxsegmentmult = NULL;
  hsvxclustermult = NULL;
  hsvxrawhitmult  = NULL;
  hadc2X          = NULL;
  hpix_chip_mult  = NULL;
  hzvtx           = NULL;
  h2pixel         = NULL;
  h2pixel2        = NULL; //nhit<30
  h2pixel3        = NULL; //nchip<5
*/
  
  ntpevt          = NULL;
  m_ntp_cnttrk    = NULL;
  m_ntp_cnttrk_bg = NULL;

}

//==============================================================

SvxCntQA::~SvxCntQA() {
//  if(m_svxAdrObj!=NULL) delete m_svxAdrObj;
  if(random          == NULL) {delete random          ; }
  if(OutputNtupleFile== NULL) {delete OutputNtupleFile; }
/*
  if(ntpsvxkalfit    == NULL) {delete ntpsvxkalfit    ; }
  if(ntpsvxrawhit    == NULL) {delete ntpsvxrawhit    ; }
  if(ntpsvxcluster   == NULL) {delete ntpsvxcluster   ; }
  if(ntpsvxsegment   == NULL) {delete ntpsvxsegment   ; }
  if(ntprawclus      == NULL) {delete ntprawclus      ; }
  if(ntp_trk         == NULL) {delete ntp_trk         ; }
  if(ntp_cnt         == NULL) {delete ntp_cnt         ; }
  if(ntp_cnt_clus    == NULL) {delete ntp_cnt_clus    ; }
  if(ntp_cnt_clus1   == NULL) {delete ntp_cnt_clus1   ; }
  if(ntp_evt_pxlchip == NULL) {delete ntp_evt_pxlchip ; }
                      
  if(hdphi           == NULL) {delete hdphi           ; }
  if(hdphi2          == NULL) {delete hdphi2          ; }
  if(hdphi3          == NULL) {delete hdphi3          ; }
  if(hdphi3a         == NULL) {delete hdphi3a         ; }
  if(hdphi3b         == NULL) {delete hdphi3b         ; }
  if(hdthe           == NULL) {delete hdthe           ; }
  if(hdthe2          == NULL) {delete hdthe2          ; }
  if(hdphidthe       == NULL) {delete hdphidthe       ; }
  if(hsvxsegmentmult == NULL) {delete hsvxsegmentmult ; }
  if(hsvxclustermult == NULL) {delete hsvxclustermult ; }
  if(hsvxrawhitmult  == NULL) {delete hsvxrawhitmult  ; }
  if(hadc2X          == NULL) {delete hadc2X          ; }
  if(hpix_chip_mult  == NULL) {delete hpix_chip_mult  ; }
  if(hzvtx           == NULL) {delete hzvtx           ; }
  if(h2pixel         == NULL) {delete h2pixel         ; }
  if(h2pixel2        == NULL) {delete h2pixel2        ; }
  if(h2pixel3        == NULL) {delete h2pixel3        ; }
                      
  if(ntpevt          == NULL) {delete ntpevt          ; }
  if(m_ntp_cnttrk    == NULL) {delete m_ntp_cnttrk    ; }
  if(m_ntp_cnttrk_bg == NULL) {delete m_ntp_cnttrk_bg ; }
*/

}

//==============================================================

int SvxCntQA::Init(PHCompositeNode *topNode) {

  if(verbosity>0) cout << "SvxCntQA::Init started..." << endl;

  OutputNtupleFile = new TFile(OutputFileName.c_str(),"RECREATE");

  if(verbosity>0) cout << "SvxCntQA::Init: output file " << OutputFileName << " opened." << endl;

  random = new TRandom2();


  ntpsvxrawhit = new  TNtuple("ntpsvxrawhit","","event:hitID:section:layer:ladder:sensor:SS:readout:type:channel:adc:pixModule:pixROC:status");
  ntpsvxcluster = new  TNtuple("ntpsvxcluster","","event:x:y:z:adc1:adc2:layer:hitID:section:ladder:sensor:type:edgeflag:lx:ly:lz:phi:size:xsize:zsize");
  ntpsvxsegment = new  TNtuple("ntpsvxsegment","","event:mom:px:py:pz:L0phi:L1phi:L2phi:L3phi:charge:nhit0:nhit1:nhit2:nhit3:quality:dedx1:dedx2:dca:dca2d:nseg:bbcz:vtxz:vtxx:vtxy:phi0:phi0b:vtxxc:vtxyc:dca0:bend:L0ladder:L1ladder:L0sensor:L1sensor:dproj2:bend2:dproj3:bend3:dproj32:bend32:L2ladder:L3ladder:x0:y0:z0:x1:y1:z1:x2:y2:z2:x3:y3:z3:dEdx1:dEdx2");

  ntprawclus = new TNtuple("ntprawclus","","event:rid:rrid:rlayer:rladder:rsensor:rsection:rreadout:rchannel:pixROC:pixMod:cid:clayer:cladder:csensor:lx:lz:x:y:z:ix:iz:ixs:izs:rlx:rlz:size:xsize:zsize:radc:cadc");

//  ntpevt = new TNtuple("ntpevt","","event:strig:n0:n1:n2:n3:zvtx:zzdc:nseg:ntrk:bbcq:xvtxs:yvtxs:zvtxs:xvtxp:yvtxp:zvtxp");
  initEvtTree();
  initCntTree();


  ntp_trk = new TNtuple("ntp_trk","","mom_cnt:phi0_cnt:mom:phi0:phi00:the0_cnt:the0:dphi0:n0:emcdphi:emcdz:ecore:dca0:nhit:qual:dcqual");
  ntp_cnt = new TNtuple("ntp_cnt","","mom:emcdphi:emcdz:the0:phi0:zed:zvtx");
  ntp_cnt_clus = new TNtuple("ntp_cnt_clus","","mom:emcdphi:emcdz:the0:phi0:zed:layer:zproj:dproj:bend:zv:phiv:zvtx");
  ntp_cnt_clus1 = new TNtuple("ntp_cnt_clus1","","mom:emcdphi:emcdz:the0:phi0:zed:zproj3:dproj3:zv3:zvtx:layer1:dproj1:bend1:zproj1:zv1");

  ntp_evt_pxlchip = new TNtuple("ntp_evt_pxlchip","","event:bbcq:zvtx:layer:ladder:sensor:mod:roc:ncls:nlcls:size:lsize:nrhit");


/*
  hdphi = new TH1F("hdphi","dphi(svx-central)",400,-4,4);
  hdphi2 = new TH1F("hdphi2","dphi(svx-central)",400,-4,4);
  hdphi3 = new TH1F("hdphi3","dphi(svx-central)",400,-4,4);
  hdphi3a = new TH1F("hdphi3a","dphi(svx-central)",400,-4,4);
  hdphi3b = new TH1F("hdphi3b","dphi(svx-central)",400,-4,4);
  hdthe = new TH1F("hdthe","dtheta(svx-central)",100,-0.3,0.3);
  hdthe2 = new TH1F("hdthe2","dtheta(svx-central)",100,-0.3,0.3);
  hdphidthe = new TH2F("hdphidthe","dthe(svx-central) : dphi",400,-4,4,100,-0.3,0.3);

  hsvxsegmentmult = new TH1F("hsvxsegmentmult","",1000,0.,1000.);
  hsvxclustermult = new TH1F("hsvxclustermult","",1000,0.,1000.);
  hsvxrawhitmult = new TH1F("hsvxrawhitmult","",1000,0.,1000.);
  hadc2X = new TH1F("hadc2X","",200,0.,200);

  hzvtx = new TH1F("hzvtx","bbc vertex",100,-50,50);

  hpix_chip_mult = new TH1F("hpix_chip_mult","pix chip multiplicity",1000,0.,1000.);

  h2pixel  = new TH2F("h2pixel","",18,-1.,17.0,32,-1.0,31.0);
  h2pixel2 = new TH2F("h2pixel2","nhit<30",18,-1.,17.0,32,-1.0,31.0);
  h2pixel3 = new TH2F("h2pixel3","nchip<5",18,-1.,17.0,32,-1.0,31.0);
*/

//  m_svxAdrObj = new svxAddress();
//  m_svxAdrObj->set_usedatabase(0);
//  m_svxAdrObj->Initialize();

  if(verbosity>0) cout << "SvxCntQA::Init ended." << endl;

  return 0;
}

//==============================================================
  
int SvxCntQA::InitRun(PHCompositeNode *topNode) {
  if(verbosity>0) cout << "SvxCntQA::InitRun started..." << endl;

  svxAddress* address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( address == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  m_svxAdrObj = address;

  svxDetectorGeo* detgeo = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if ( detgeo == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxDetectorGeo. " << endl; }
    return ABORTRUN;
  }
  m_svxGeo = detgeo;

  // check magnet current 
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader==NULL) {
    cout << PHWHERE<< "Can't find runheader. " << endl;
    return ABORTRUN;
  }
  if(runheader->get_currentCentral()>0){
   m_fieldScale = 1.0;
  } else {
   m_fieldScale = -1.0;
  }
  cout<<"SvxCntQA::InitRun  fieldScale="<<m_fieldScale<<endl;

  // get beam center from BeamCenter object
  SvxBeamCenterPar* beamcenter = findNode::getClass<SvxBeamCenterPar>(topNode, "SvxBeamCenterPar");
  if (beamcenter==NULL) {
    cout << PHWHERE<< "Can't find beamcenter. " << endl;
    return ABORTRUN;
  }
  m_beam_x = beamcenter->getBeamCenter(0);
  m_beam_y = beamcenter->getBeamCenter(1);
  cout<<"SvxCntQA::InitRun beamcenter(x,y)= "<< m_beam_x<<" "<<m_beam_y<<endl;

  // clear lowpT accept counter
  m_lowpt_ctr[0]=0;
  m_lowpt_ctr[1]=0;
  m_midpt_ctr[0]=0;
  m_midpt_ctr[1]=0;

  // message
  cout<<"SvxCntQA GoodOnlyFlag = "<<(m_goodOnlyFlag ? "True" : "False")<<endl;
  if(m_goodOnlyFlag){
    cout<<"SvxCntQA   ZVTXCUT    : "<< GOODZVTX      <<endl;
    cout<<"SvxCntQA   EMCDPHICUT : "<< GOODEMCDPHI   <<endl;
    cout<<"SvxCntQA   EMCDZCUT   : "<< GOODEMCDZ     <<endl;
    cout<<"SvxCntQA   DCHQUALITY : "<< GOODDCQUALITY <<endl;
  }


  if(verbosity>0) cout << "SvxCntQA::InitRun ended." << endl;
  return 0;
}

//==============================================================


bool bad_sensor(int layer, int ladder, int sensor) {
  return false;
}


bool bad_chip(int chip, int pix_ladder) {
  return false;
}


int SvxCntQA::process_event(PHCompositeNode *topNode) {

  //float ntp1[99],ntp2[99],ntp3[99],ntp4[99],ntp5[99],ntp6[99],ntp7[99];
  //float ntp4[99],ntp5[99],ntp6[99];

  //  cout<<"aa"<<endl;
  //  PHTrackOut         *proj = getClass<PHTrackOut>(topNode,"PHTrackOut");
  //  CrkRing             *crk = getClass<CrkRing>(topNode,"CrkRing");
  //  PadCluster          *pc3 = getClass<PadCluster>(topNode,"Pc3Cluster");
  //  emcClusterContainer *emc = getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  //  DchTrack            *dch = getClass<DchTrack>(topNode,"DchTrack");
  //  CglTrack            *cgl = getClass<CglTrack>(topNode,"CglTrack");
  PHGlobal *global = getClass<PHGlobal>(topNode,"PHGlobal");
  RunHeader *run = getClass<RunHeader>(topNode,"RunHeader");
  EventHeader *event_header = getClass<EventHeader>(topNode,"EventHeader");
  TrigLvl1    *trglvl1 = getClass<TrigLvl1>(topNode,"TrigLvl1");
  //TrigRunLvl1 *lvl1    = getClass<TrigRunLvl1>(topNode,"TrigRunLvl1");
  BbcOut *bbc    = getClass<BbcOut>(topNode,"BbcOut");
  VtxOut *vtxout = getClass<VtxOut>(topNode,"VtxOut");

  int RunNumber = (run!=NULL) ? run->get_RunNumber() : -9999;
  int EventNum = (event_header!=NULL) ? event_header->get_EvtSequence() : -9999;


  PHCentralTrack      *trk = getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  int ntrk = (trk!=NULL) ? trk->get_npart() : 0;

  SvxRawhitList *svxrawhit = getClass<SvxRawhitList>(topNode,"SvxRawhitList");
  SvxClusterList
      *svx = getClass<SvxClusterList>(topNode,"SvxClusterList");
  SvxSegmentList *svxtracks= getClass<SvxSegmentList>(topNode,"SvxSegmentList");
  SvxRawhitClusterList 
            *svxrawhitclus = getClass<SvxRawhitClusterList>(topNode,"SvxRawhitClusterList");

  SvxCentralTrackList *svxcnttrklist = getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackList");

  SvxClusterContainer *container = getClass<SvxClusterContainer>(topNode,"SvxClusterContainer");

  RpSumXYObject *rpsum = getClass<RpSumXYObject>(topNode,"RpSumXYObject");

  SvxCentralTrackList *svxcnttrkbglist = getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackBackList");


  McEvalSingleList *mctrk = getClass<McEvalSingleList>(topNode,"McSingle");
  if(m_simmode){
    if(mctrk==NULL){
      cout<<"No McSingle Info in Nodetree"<<endl;
    }
  }

  int nsvxtracks = svxtracks->get_nSegments();
  int nsvx = svx->get_nClusters();
  int nsvxrawhit = (svxrawhit!=NULL) ? svxrawhit->get_nRawhits() : -9999;
  //int nsvxrawhitclus = 0;
  int nkalfit = 0;
  float zvtx = (bbc!=NULL) ? bbc->get_VertexPoint() : -999.0;
  float bbcq = (bbc!=NULL) ? (bbc->get_ChargeSum(Bbc::North) + bbc->get_ChargeSum(Bbc::South)) : 50.0;
  //if(bbcq>200 || bbcq<10)return 1; 

  float zdcq = (global!=NULL) ? global->getZdcEnergyN()+global->getZdcEnergyS(): -9999.;

  float vtx_z = (vtxout!=NULL) ? vtxout->get_ZVertex() : -999.;
  if(m_goodOnlyFlag&&(fabs(vtx_z)>GOODZVTX)){ // skip this event
    if(verbosity>0) cout << "SvxCntQA  skip this event : run = "<<RunNumber<<" event = "<<EventNum<<endl;
    return EVENT_OK;
  }

  //if(EventNumber==0) {
  //  cout << "SvxCntQA : run = "<<RunNumber<<" event = "<<EventNum<<endl;
  //}

  

  if(verbosity>0){
    if(init_ana==0) {
      cout<<"init ana"<<endl;
      //topNode->print(); 
      init_ana=1;
    
      if(svx!=NULL)          svx->identify();          else cout<<"no SvxCluster object"<<endl; 
      if(svxtracks!=NULL)    svxtracks->identify();    else cout<<"no SvxSegment object"<<endl;
      if(svxrawhitclus!=NULL)svxrawhitclus->identify();else cout<<"no SvxRawhitClus object"<<endl;
      if(svxrawhit!=NULL)    svxrawhit->identify();    else cout<<"no SvxRawhit object"<<endl;
      if(trglvl1!=NULL)      trglvl1->identify();      else cout<<"no TrigLvl1 object"<<endl;

      cout<<endl<<endl;
      cout<<"init ana ends"<<endl;
    }
  }

  if(verbosity>1){
    if(EventNumber%1==0) {
      cout << "------- Event # " << EventNumber << " nsvxrawhit= " << nsvxrawhit << " nsvx= " << nsvx << " ntrack= " << ntrk << " nkalfit= " << nkalfit << " nsvxtracks=" << nsvxtracks << endl;
    }
  }


//------------------------ VTX information --------------------------
/*
  hsvxrawhitmult->Fill(float(nsvxrawhit));

  
  int L2X[16][5][2][384];
  int L2U[16][5][2][384];
  int L3X[24][6][2][384];
  int L3U[24][6][2][384];

  fill_n(&L2X[0][0][0][0],16*5*2*384,0);
  fill_n(&L2U[0][0][0][0],16*5*2*384,0);
  fill_n(&L3X[0][0][0][0],24*6*2*384,0);
  fill_n(&L3U[0][0][0][0],24*6*2*384,0);
*/  
/* ---------------------
  //cout<<"rawhit"<<endl;
  if(EventNumber<100&&svxrawhit!=NULL){
    for(int i=0; i<nsvxrawhit; i++) {
      SvxRawhit* tmp = svxrawhit->get_Rawhit(i);

      int hitID         = tmp->get_hitID();
      int section       = tmp->get_svxSection();
      int layer         = tmp->get_layer();
      int ladder        = tmp->get_ladder();
      int sensor        = tmp->get_sensor();
      int sensorSection = tmp->get_sensorSection();
      int readout       = tmp->get_sensorReadout();
      int type          = tmp->get_sensorType();
      int channel       = tmp->get_channel();
      int adc           = tmp->get_adc();
      int pixelModule   = tmp->get_pixelModule();
      int pixelROC      = tmp->get_pixelROC();
      int status        = tmp->get_HotDeadFlag();
      
//      if(status>0){
//        cout<<"HotDead : "<<layer<<" "<<sensor<<" "<<channel<<endl;
//      }

      //int strip_ladder = -1;

      ntp4[0]=(float)EventNumber;
      //if(readout==0) { ntp4[1]=adc; ntp4[2]=0.; }
      //if(readout==1) { ntp4[1]=0.; ntp4[2]=adc; }
      ntp4[1]=(float)hitID;
      ntp4[2]=(float)section;
      ntp4[3]=(float)layer;
      ntp4[4]=(float)ladder;
      ntp4[5]=(float)sensor;
      ntp4[6]=(float)sensorSection;
      ntp4[7]=(float)readout;
      ntp4[8]=(float)type;
      ntp4[9]=(float)channel;
      ntp4[10]=(float)adc;
      ntp4[11]=(float)pixelModule;
      ntp4[12]=(float)pixelROC;
      ntp4[13]=(float)status;
      
      ntpsvxrawhit->Fill(ntp4);

//
//    if(ladder == 2) {
//      if(readout==0) L2X[ladder][sensor][section][channel] = adc;
//      if(readout==1) L2U[ladder][sensor][section][channel] = adc;
//    }
//    if(ladder == 3) {
//      if(readout==0) L3X[ladder][sensor][section][channel] = adc;
//      if(readout==1) L3U[ladder][sensor][section][channel] = adc;
//    }
//

    }//for(nrawhit)
  } //if EventNumber<100 && SvxRawhit!=NULL
/ --------------------- */

/*
  if(fabs(zvtx)<10) {
    for(int iladder=0;iladder<16;iladder++)
      for(int isensor=0;isensor<5;isensor++)
	for(int isection=0;isection<2;isection++) {
	  int ichannel=1;
	  while(ichannel<384) {
	    int adc00 = L2X[iladder][isensor][isection][ichannel-1];
	    int adc0 = L2X[iladder][isensor][isection][ichannel];
	    if(adc0>15 && adc00<10) {
	      ichannel++;
	      int adc1 = L2X[iladder][isensor][isection][ichannel];
	      if(adc1 > 15) {
		ichannel++;
		if(L2X[iladder][isensor][isection][ichannel]<10) {
		  hadc2X->Fill(adc1+adc0);
		}
	      }
	    }
	    ichannel++;
	  }
	}
  }//if(zvtx)

  hsvxclustermult->Fill(float(nsvx));
*/

  //chick if this is a noisy event or not
  int nhit_layer[4];
  for(int i=0;i<4;i++) nhit_layer[i]=0;

/*
  int pix_chip_mult[16][30];
  for(int i=0;i<16;i++)
    for(int j=0;j<30;j++)
      pix_chip_mult[i][j]=0;
*/

  if(svx!=NULL){
    for(int i=0;i<nsvx;i++) {
      SvxCluster* clus0 = svx->get_Cluster(i);
      int layer  = clus0->get_layer();
      int ladder = clus0->get_ladder();
      int sensor = clus0->get_sensor();
      float lz = clus0->get_xyz_local(2);

      if(layer<2) {
        int pix_ladder=0;
        int chip = sensor*4 + (lz/1.3)+2;
        if(!bad_chip(chip,pix_ladder)) {
          nhit_layer[layer]++;
        }
      } else if(!bad_sensor(layer,ladder,sensor)) {
        nhit_layer[layer]++;
      }

/*   
      if(0<=layer&&layer<2) {
        int pix_ladder;
        int chip = sensor*4 + (lz/1.3)+2;

        if(layer==0) pix_ladder = ladder;
        else pix_ladder = ladder + 10;

        if(0<= chip && chip<16 &&
           0<= pix_ladder && pix_ladder < 30) {
          pix_chip_mult[chip][pix_ladder]++;
        }
      }
*/
    } // for(int i=0;i<nsvx;i++) 
  } // if(svx!=NULL)


/*
  for(int i=0;i<16;i++)
    for(int j=0;j<30;j++)
      hpix_chip_mult->Fill(pix_chip_mult[i][j]);
*/

  if(EventNumber<100&&svx!=NULL){
/*
    for(int i=0; i<nsvx; i++) {
       SvxCluster* clus0 = svx->get_Cluster(i);
       float svxx = clus0->get_xyz_global(0);
       float svxy = clus0->get_xyz_global(1);
       float svxz = clus0->get_xyz_global(2);
       int   layer = clus0->get_layer();

//---
//---     int chip_mult = -1;
//---
//---     if(layer < 2) {//pixel
//---       int sensor = clus0->get_sensor();
//---       int ladder = clus0->get_ladder();
//---       float lz = clus0->get_xyz_local(2);
//---       int pix_ladder;
//---       int chip = sensor*4 + (lz/1.3)+2;
//---
//---       if(layer==0) pix_ladder = ladder;
//---       else pix_ladder = ladder + 10;
//---
//---       if(0<=chip&&chip<16&&0<=pix_ladder&&pix_ladder<30) {
//---         h2pixel->Fill(chip,pix_ladder);
//---         if(!bad_chip(chip,pix_ladder)) {
//---           h2pixel2->Fill(chip,pix_ladder);
//---           chip_mult = pix_chip_mult[chip][pix_ladder];
//---           if(pix_chip_mult[chip][pix_ladder]<5) {
//---             h2pixel3->Fill(chip,pix_ladder);
//---           }
//---         }
//---       }
//---     }//if(layer)
//---
       
//       if(!bad_sensor(layer, clus0->get_ladder(), clus0->get_sensor())) {
         float phi = atan2(svxy,svxx);
         ntp5[0]=(float)EventNumber;
         ntp5[1]=svxx;
         ntp5[2]=svxy;
         ntp5[3]=svxz;
         ntp5[4]=clus0->get_adc(0);
         ntp5[5]=clus0->get_adc(1);
         ntp5[6]=(float)layer;
         ntp5[7]=(float) clus0->get_hitID();
         ntp5[8]=(float) clus0->get_svxSection();
         ntp5[9]=(float) clus0->get_ladder();
         ntp5[10]=(float) clus0->get_sensor();
         ntp5[11]=(float) clus0->get_sensorType();
         ntp5[12]=(float) clus0->get_edgeflag();
         ntp5[13]= clus0->get_xyz_local(0);
         ntp5[14]= clus0->get_xyz_local(1);
         ntp5[15]= clus0->get_xyz_local(2);
         ntp5[16]= phi;
         ntp5[17]= clus0->get_size();
         ntp5[18]= clus0->get_xz_size(0);
         ntp5[19]= clus0->get_xz_size(1);
         ntpsvxcluster->Fill(ntp5);
//       }
    }
*/
  }

/*
  hzvtx->Fill(zvtx);
*/

  /////////////////////////////
  // event by event info
  float zzdc = -9999.0;
  PHPoint vtxs_pos(-9999.0, -9999.0, -9999.0);
  PHPoint vtxp_pos(-9999.0, -9999.0, -9999.0);
  PHPoint vtxsw_pos(-9999.0, -9999.0, -9999.0);
  PHPoint vtxse_pos(-9999.0, -9999.0, -9999.0);
  PHPoint vtxbbc_pos(-9999.0, -9999.0, -9999.0);
  PHPoint vtxpw_pos(-9999.0, -9999.0, -9999.0);
  PHPoint vtxpe_pos(-9999.0, -9999.0, -9999.0);

  if(vtxout!=NULL) {
    zzdc     = vtxout->get_ZVertex("ZDC");
    vtxs_pos = vtxout->get_Vertex("SVX");
    vtxp_pos = vtxout->get_Vertex("SVX_PRECISE");
    vtxsw_pos = vtxout->get_Vertex("SVXW");
    vtxse_pos = vtxout->get_Vertex("SVXE");
    vtxbbc_pos = vtxout->get_Vertex("BBC");

    vtxpw_pos = vtxout->get_Vertex("SVX_PRECISEW");
    vtxpe_pos = vtxout->get_Vertex("SVX_PRECISEE");

    if(verbosity>2){
      cout<<"SVXW :  "<<(vtxout->isVtx("SVXW")?"OK":"Fail")<<endl;
      cout<<"SVXE :  "<<(vtxout->isVtx("SVXE")?"OK":"Fail")<<endl;
      cout<<"SVXPRIW :  "<<(vtxout->isVtx("SVX_PRECISEW")?"OK":"Fail")<<endl;
      cout<<"SVXPRIE :  "<<(vtxout->isVtx("SVX_PRECISEE")?"OK":"Fail")<<endl;
    }
  }

  m_evt_run   = RunNumber;
  m_evt_event = EventNumber;
  m_evt_strig = (trglvl1!=NULL) ? trglvl1->get_lvl1_trigscaled()  : -1;
  m_evt_xctr  = (trglvl1!=NULL) ? trglvl1->get_lvl1_clock_cross() : -1;
  m_evt_eseq  = EventNum;
  m_evt_n0 = nhit_layer[0];
  m_evt_n1 = nhit_layer[1];
  m_evt_n2 = nhit_layer[2];
  m_evt_n3 = nhit_layer[3];
  m_evt_zvtx = vtx_z;
  m_evt_zbbc = zvtx;
  m_evt_zzdc = zzdc;
  m_evt_ntrk = ntrk;
  m_evt_nseg = nsvxtracks;
  m_evt_bbcq = bbcq;
  m_evt_zdcq = zdcq;
  m_evt_xvtxs = vtxs_pos.getX();
  m_evt_yvtxs = vtxs_pos.getY();
  m_evt_zvtxs = vtxs_pos.getZ();
  m_evt_xvtxp = vtxp_pos.getX();
  m_evt_yvtxp = vtxp_pos.getY();
  m_evt_zvtxp = vtxp_pos.getZ();
  m_evt_zvtxsw = vtxsw_pos.getZ();
  m_evt_zvtxse = vtxse_pos.getZ();
  m_evt_xvtxpw = vtxpw_pos.getX();
  m_evt_yvtxpw = vtxpw_pos.getY();
  m_evt_zvtxpw = vtxpw_pos.getZ();
  m_evt_xvtxpe = vtxpe_pos.getX();
  m_evt_yvtxpe = vtxpe_pos.getY();
  m_evt_zvtxpe = vtxpe_pos.getZ();

  if(verbosity>2){
    cout<<"x_vtx_s : x_vtx_p : "<< vtxs_pos.getZ()<<" "<<vtxp_pos.getZ()<<" "<<vtxsw_pos.getZ()<<" "<<vtxse_pos.getZ()<<endl;
    cout<<"x_vtx_s : y_vtx_s : "<< vtxs_pos.getX()<<" "<<vtxs_pos.getY()<<endl;
    cout<<"zbbc_x : zbbc_y : zbbc_z "<< vtxbbc_pos.getX()<<" "<<vtxbbc_pos.getY()<<" "<<vtxbbc_pos.getZ()<<endl;
  }

  ntpevt->Fill();



  /*
   * offset of the beam center
   */
  //float xoffset1 = 0.23;
  //float yoffset1 = 0.08;

  /*
   * Central tracks
   */
/*---------------------------*/
  vector<float> v_mom;
  vector<float> v_the0;
  vector<float> v_phi0;
  vector<float> v_n0;
  vector<float> v_emcdphi;
  vector<float> v_emcdz;
  vector<float> v_ecore;
  vector<short> v_quality;
  if(ntrk>0) {
    for(int itrk=0;itrk<ntrk;itrk++) {
      v_mom.push_back(trk->get_mom(itrk)*trk->get_charge(itrk));
      v_the0.push_back(trk->get_the0(itrk));
      float phi0_trk = trk->get_phi0(itrk);
      if(phi0_trk > 3.141592) phi0_trk = phi0_trk - 6.283184;
      v_phi0.push_back(phi0_trk);
      v_n0.push_back(trk->get_n0(itrk));
      v_emcdphi.push_back(trk->get_emcdphi(itrk));
      v_emcdz.push_back(trk->get_emcdz(itrk));
      v_quality.push_back(trk->get_quality(itrk));
      v_ecore.push_back(trk->get_ecore(itrk));

//    if(trk->get_mom(itrk)>0.5&&trk->get_mom(itrk)<4.0) {
//      float mom_cnt    = trk->get_mom(itrk);
//      int   charge_cnt = trk->get_charge(itrk);
//      float the0_cnt   = trk->get_the0(itrk);
//      float phi0_cnt   = trk->get_phi0(itrk);
//      if(phi0_cnt > 3.141592) phi0_cnt = phi0_cnt - 6.283184;
//      float emcdphi    = trk->get_emcdphi(itrk);
//      float emcdz      = trk->get_emcdz(itrk);
//      float pt_cnt     = mom_cnt*sin(the0_cnt);
//      float zed        = trk->get_zed(itrk);
//      //	float zproj0     = zvtx+R0/tan(the0_cnt);
//      	    
//      float ux = cos(phi0_cnt);
//      float uy = sin(phi0_cnt);
//
//      if(fabs(emcdz-1.4)<15 && fabs(emcdphi+0.00015)<0.02) {
//        ntp_cnt->Fill(mom_cnt,emcdphi,emcdz,the0_cnt,phi0_cnt,zed,zvtx);
//        for(int i=0;i<nsvx;i++) {
//          SvxCluster *clus0 = svx->get_Cluster(i);
//          int   layer = clus0->get_layer();
//          float svxx = clus0->get_xyz_global(0);
//          float svxy = clus0->get_xyz_global(1);
//          float svxz = clus0->get_xyz_global(2);
//          float phiv = atan2(svxy,svxx);
//          
//          float dx = svxx - xoffset1;
//          float dy = svxy - yoffset1;
//          float rhit_2 = dx*dx+dy*dy;
//          float rhit = sqrt(rhit_2);
//          
//          // vector product (ux,uy) x (dx, dy)
//          // this is signed difference between straight line projection
//          // from beam spot (xoffset1,yoffset1) and a svx hit position.
//          float u_cross_dx = ux*dy - uy*dx;
//          float mag_bend   = -m_fieldScale(0.0013*charge_cnt*rhit_2/pt_cnt);
//          
//          float dproj = u_cross_dx;
//          float zproj = zvtx + rhit/tan(the0_cnt);
//          
//          if(layer == 0) {
//            if(fabs(phi0_cnt)<1.5) dproj = dproj + 0.01;
//            else dproj = dproj - 0.04;
//          }
//          if(layer == 1) {
//            if(fabs(phi0_cnt)<1.5) dproj = dproj + 0.04;
//            else dproj = dproj - 0.065;
//          }
//          if(layer == 2) {
//            if(fabs(phi0_cnt)<1.5) dproj = dproj + 0.06;
//            else dproj = dproj - 0.14;
//          }
//          if(layer == 3) {
//            if(fabs(phi0_cnt)<1.5) dproj = dproj + 0.1;
//            else dproj = dproj - 0.2;
//          }
//
//          if(fabs(zproj - svxz)<3) {
//            if(fabs(dproj+mag_bend)<0.1+0.1*layer){
//      	ntp_cnt_clus->Fill(mom_cnt,emcdphi,emcdz,the0_cnt,phi0_cnt,zed,layer,zproj,dproj,mag_bend,svxz,phiv,zvtx);
//            }
//          }
//          if(layer==3 && fabs(zproj-svxz)<2&& fabs(dproj+mag_bend)<0.4) {
//            for(int j=0;j<nsvx;j++) {
//      	SvxCluster *clus1 = svx->get_Cluster(j);
//      	int   layer1 = clus1->get_layer();
//      	if(layer1 < 3) {
//      	  float svxx1 = clus1->get_xyz_global(0);
//      	  float svxy1 = clus1->get_xyz_global(1);
//      	  float svxz1 = clus1->get_xyz_global(2);
//      	  float phiv1 = atan2(svxy1,svxx1);
//      	  
//      	  float dx1 = svxx1 - xoffset1;
//      	  float dy1 = svxy1 - yoffset1;
//      	  float rhit1_2 = dx1*dx1+dy1*dy1;
//      	  float rhit1 = sqrt(rhit1_2);
//      	  
//      	  // vector product (ux,uy) x (dx, dy)
//      	  // this is signed difference between straight line projection
//      	  // from beam spot (xoffset1,yoffset1) and a svx hit position.
//      	  float u_cross_dx1 = ux*dy1 - uy*dx1;
//      	  float mag_bend1   = -m_fieldScale*(0.0013*charge_cnt*rhit1_2/pt_cnt);
//      	  
//      	  float dproj1 = u_cross_dx1;
//      	  float zproj1 = zvtx + rhit1/tan(the0_cnt);
//      	  
//      	  if(layer1 == 0) {
//      	    if(fabs(phi0_cnt)<1.5) dproj1 = dproj1 + 0.01;
//      	    else dproj1 = dproj1 - 0.04;
//      	  }
//      	  if(layer1 == 1) {
//      	    if(fabs(phi0_cnt)<1.5) dproj1 = dproj1 + 0.04;
//      	    else dproj1 = dproj1 - 0.065;
//      	  }
//      	  if(layer1 == 2) {
//      	    if(fabs(phi0_cnt)<1.5) dproj1 = dproj1 + 0.06;
//      	    else dproj1 = dproj1 - 0.14;
//      	  }
//
//      	  if(fabs(zproj1-svxz1)<2.5&& fabs(dproj1+mag_bend1)<0.1+0.1*layer1) {
//      	    ntp_cnt_clus1->Fill(mom_cnt,emcdphi,emcdz,the0_cnt,phi0_cnt,zed,zproj,dproj+mag_bend,svxz,zvtx,layer1,dproj1,mag_bend1,zproj1,svxz1);
//      	  }
//      	}//if(layer1<3)
//            }//for(nsvx)j
//          }//if(layer==3)
//        }//for(svx)
//      }//if(|emcdz|<15 && |emcdphi|<0.02)
//    }//if(0.5<mom_cnt<4)
//
    }
  }
/*--------------------- */

  /*
   * Segment (StandAloneTracking)
   */
/*
  if((EventNumber%100)==0&&svxtracks!=NULL){ // 1/100 events are stored
    if(fabs(zvtx)<10) {
      hsvxsegmentmult->Fill(float(nsvxtracks));
      float val_ntp_trk[16];
      for(int i=0; i<nsvxtracks; i++) {
        SvxSegment* seg0 = svxtracks->get_segment(i);
        float dca3d = seg0->getDCA();  // 3d dca
        float dca2d = seg0->getDCA2D();  // 2d dca (x-y)
        float charge, primary;
        if(seg0->IsPositive()) charge = 1; else charge = -1;
        if(seg0->getPrimary()) primary= 1; else primary = 0;

        float nhit=0;
        for(int ily=0; ily<4; ily++){ nhit+=seg0->getNhits(ily); }
        float quality = seg0->getQuality();
        
        // print out reconstructed track
        if(verbosity>1){
          cout << "track "<<i<<endl;
          cout << "mom ="<<seg0->getMomentum()<<endl;
        }
        for(int jj=0;jj<4;jj++) {
          int clusterID = seg0->getClusterID(jj,0);

          if(verbosity>1){
            cout << jj<<":(";
            cout <<seg0->getProjectedPosition(jj,0)<<",";
            cout <<seg0->getProjectedPosition(jj,1)<<",";
            cout <<seg0->getProjectedPosition(jj,2)<<")  ";
            cout << "clusterID="<<clusterID<<endl;
          }
          
          if(clusterID>=0) {
            SvxCluster *cluster = svx->get_Cluster(clusterID);
            if(cluster>0) {
              if(verbosity>1){
                cout << "  (";
                cout << cluster->get_xyz_global(0)<<",";
                cout << cluster->get_xyz_global(1)<<",";
                cout << cluster->get_xyz_global(2)<<")"<<endl;
              }
            } else {
              cout <<"error..cluster is empty. skip this event"<<endl;
              return 0;
            }
          } else {
            if(verbosity>1){
              cout<< " ( blanck )" <<endl;
            }
          }
        }
        // calculate projected vertex from L0 and L1 hit
        float x0 = seg0->getProjectedPosition(0,0);
        float y0 = seg0->getProjectedPosition(0,1);
        float z0 = seg0->getProjectedPosition(0,2);
        float r0 = sqrt(x0*x0+y0*y0);
        float x1 = seg0->getProjectedPosition(1,0);
        float y1 = seg0->getProjectedPosition(1,1);
        float z1 = seg0->getProjectedPosition(1,2);
        float r1 = sqrt(x1*x1+y1*y1);

        int pixel0id = seg0->getClusterID(0,0);
        int pixel0ladder = (pixel0id>=0) ? svx->get_Cluster(pixel0id)->get_ladder()      : -9999;
        int pixel0sensor = (pixel0id>=0) ? svx->get_Cluster(pixel0id)->get_sensor()      : -9999;
        float L0x        = (pixel0id>=0) ? svx->get_Cluster(pixel0id)->get_xyz_global(0) : -9999;
        float L0y        = (pixel0id>=0) ? svx->get_Cluster(pixel0id)->get_xyz_global(1) : -9999;
        float L0z        = (pixel0id>=0) ? svx->get_Cluster(pixel0id)->get_xyz_global(2) : -9999;
        float L0phi      = (pixel0id>=0) ? atan2(L0y,L0x) : -9999;

        int pixel1id     = seg0->getClusterID(1,0);
        int pixel1ladder = (pixel1id>=0) ? svx->get_Cluster(pixel1id)->get_ladder()      : -9999;
        int pixel1sensor = (pixel1id>=0) ? svx->get_Cluster(pixel1id)->get_sensor()      : -9999;
        float L1x        = (pixel1id>=0) ? svx->get_Cluster(pixel1id)->get_xyz_global(0) : -9999;
        float L1y        = (pixel1id>=0) ? svx->get_Cluster(pixel1id)->get_xyz_global(1) : -9999;
        float L1z        = (pixel1id>=0) ? svx->get_Cluster(pixel1id)->get_xyz_global(2) : -9999;
        float L1phi      = (pixel1id>=0) ? atan2(L1y,L1x) : -9999;

        int strip0id = seg0->getClusterID(2,0);
        int strip0ladder = -1;
        if(strip0id>0) strip0ladder = svx->get_Cluster(strip0id)->get_ladder();
        float L2x=0;
        float L2y=0;
        float L2z=0;
        float r2 = 11.0; //nominal radius of L2
        float L2phi= -10;
        if(strip0id>0) {
          L2x = svx->get_Cluster(strip0id)->get_xyz_global(0);
          L2y = svx->get_Cluster(strip0id)->get_xyz_global(1);
          L2z = svx->get_Cluster(strip0id)->get_xyz_global(2);
          L2phi = atan2(L2y,L2x);
          r2 = sqrt(L2x*L2x + L2y*L2y);
        }
        
        int strip1id = seg0->getClusterID(3,0);
        int strip1ladder = -1;
        if(strip1id>0) strip1ladder = svx->get_Cluster(strip1id)->get_ladder();
        float L3x=0;
        float L3y=0;
        float L3z=0;
        float r3 = 17.0;
        float L3phi= -10;
        if(strip1id>0) {
          L3x = svx->get_Cluster(strip1id)->get_xyz_global(0);
          L3y = svx->get_Cluster(strip1id)->get_xyz_global(1);
          L3z = svx->get_Cluster(strip1id)->get_xyz_global(2);
          L3phi = atan2(L3y,L3x);
          r3 = sqrt(L3x*L3x + L3y*L3y);
        }
        
        float x0c = x0 - xoffset1;
        float x1c = x1 - xoffset1;
        float y0c = y0 - yoffset1;
        float y1c = y1 - yoffset1;
        
        float zvtx_proj = z0 - r0*(z1-z0)/(r1-r0);
        float phi0 = atan2(y0,x0);
        float phi0b = atan2(y0,-x0);

        float yvtx_proj = y0 - x0*(y1-y0)/(x1-x0);
        float xvtx_proj = -1.0;
        if(fabs(y1-y0)>0.0001) xvtx_proj = x0 - y0*(x1-x0)/(y1-y0);

        float yvtx_projc = y0c - x0c*(y1c-y0c)/(x1c-x0c);
        float xvtx_projc = -1.0;
        if(fabs(y1-y0)>0.01) xvtx_projc = x0c - y0c*(x1c-x0c)/(y1c-y0c);

        // DCA in (x,y) plane
        float ux = x1 - x0;
        float uy = y1 - y0;
        float ul = sqrt(ux*ux + uy*uy);
        ux = ux/ul;
        uy = uy/ul;
        // now (ux, uy) is a unit vector.

        float u_dot_dx = ux*x0c + uy*y0c;
        float u_cross_dx = ux*y0c - uy*x0c;

        float dca0 = sqrt(x0c*x0c + y0c*y0c - u_dot_dx*u_dot_dx);
        if(u_cross_dx < 0) dca0 = -dca0;
        float mom = seg0->getMomentum();
        float px  = seg0->get3Momentum(0);
        float py  = seg0->get3Momentum(1);
        float pz  = seg0->get3Momentum(2);
        float pt  = sqrt(px*px+py*py); 
        float magnetic_bend = -m_fieldScale*(0.017*charge/pt);
        float theta0 = atan2(pt,pz);
        float phi00  = atan2(py,px);


        // projection to L2
        //
        float dproj2 = -10;
        float mag_bend2 = -20;
        if(strip0id>0) {
          float dx21 = L2x - x1;
          float dy21 = L2y - y1;
          float u_cross_dx2 = ux*dy21 - uy*dx21;
          // u_cross_dx2 is a vector product of (ux,uy) and (dx21,dy21).
          // its absolute value is the distance between the (L2x,L2y) and the
          // straight line projecton point of the track from (x0,y0) to (x1,y1).
          dproj2 = u_cross_dx2;
          mag_bend2 = -m_fieldScale*(1.2E-3*r2*(r2-r1)*charge/pt);
        }
        
        // projection to L3
        float dproj3 = -10;
        float mag_bend3 = -20;
        if(strip1id>0) {
          float dx31 = L3x - x1;
          float dy31 = L3y - y1;
          float u_cross_dx3 = ux*dy31 - uy*dx31;
          // u_cross_dx3 is a vector product of (ux,uy) and (dx31,dy31).
          // its absolute value is the distance between the (L3x,L3y) and the
          // straight line projecton point of the track from (x0,y0) to (x1,y1).
          dproj3 = u_cross_dx3;
          mag_bend3 = -m_fieldScale*(1.2E-3*r3*(r3-r1)*charge/pt);
        }

        // projection from L2 and L3 (if both exists)
        float dproj32 = -10;
        float mag_bend32= -20;
        if( strip0id>0 && strip1id>0) {
          float dx32 = L3x - L2x;
          float dy32 = L3y - L2y;
          float dx21 = L2x - x1;
          float dy21 = L2y - y1;
          float l21 = sqrt(dx21*dx21+dy21*dy21);
          float ux21 = dx21/l21;
          float uy21 = dy21/l21;
          dproj32 = ux21*dy32 - uy21*dx32;
          mag_bend32 = -m_fieldScale*(1.2E-3*r3*(r3-r2)*charge/pt);
        }

        // search for the closest central
        if(ntrk>0 && mom>0.2 &&abs(theta0-1.57)<0.35 ) {
          for(int itrk=0;itrk<ntrk;itrk++) {
            hdphi->Fill(phi00-v_phi0[itrk]);
            hdthe->Fill(theta0-v_the0[itrk]);
            hdphidthe->Fill(phi00-v_phi0[itrk], theta0-v_the0[itrk]);

            if(fabs(phi00 - v_phi0[itrk])<0.05) hdthe2->Fill(theta0-v_the0[itrk]);

            if(fabs(theta0 - v_the0[itrk])<0.03) hdphi2->Fill(phi00-v_phi0[itrk]);
            if(fabs(theta0 - v_the0[itrk])<0.03) {
              hdphi3->Fill(phi00-v_phi0[itrk]);
              float dphi0 = phi00 - v_phi0[itrk] + 0.006/v_mom[itrk];
              if(fabs(phi00)<1.5) dphi0 = dphi0 + 7.3E-3;
              else dphi0 = dphi0 - 1.06E-2;

              if(fabs(dphi0)<0.05) {
                val_ntp_trk[0]  = v_mom[itrk];
                val_ntp_trk[1]  = v_phi0[itrk];
                val_ntp_trk[2]  = (v_mom[itrk]>0) ? mom : -mom;
                val_ntp_trk[3]  = phi0;
                val_ntp_trk[4]  = phi00;
                val_ntp_trk[5]  = theta0;
                val_ntp_trk[6]  = v_the0[itrk];
                val_ntp_trk[7]  = dphi0;
                val_ntp_trk[8]  = v_n0[itrk];
                val_ntp_trk[9]  = v_emcdphi[itrk];
                val_ntp_trk[10] = v_emcdz[itrk];
                val_ntp_trk[11] = v_ecore[itrk];
                val_ntp_trk[12] = dca0+magnetic_bend;
                val_ntp_trk[13] = nhit;;
                val_ntp_trk[14] = quality;
                val_ntp_trk[15] = v_quality[itrk];

                if(v_mom[itrk]>0) {
          	hdphi3a->Fill(phi00-v_phi0[itrk]);
                } else {
          	hdphi3b->Fill(phi00-v_phi0[itrk]);
                }
                ntp_trk->Fill(val_ntp_trk);
              }
            }
          }
        }

        ntp6[0]=(float)EventNumber;
        ntp6[1]=mom;
        ntp6[2]=px;
        ntp6[3]=py;
        ntp6[4]=pz;
        ntp6[5]=L0phi;
        ntp6[6]=L1phi;
        ntp6[7]=L2phi;
        ntp6[8]=L3phi;
        ntp6[9]=charge;
        ntp6[10]=(float) seg0->getNhits(0);
        ntp6[11]=(float) seg0->getNhits(1);
        ntp6[12]=(float) seg0->getNhits(2);
        ntp6[13]=(float) seg0->getNhits(3);
        ntp6[14]= seg0->getQuality();
        ntp6[15]= seg0->get_dEdX1();
        ntp6[16]= seg0->get_dEdX2();
        ntp6[17]= dca3d;
        ntp6[18]= dca2d;
        ntp6[19]= nsvxtracks;
        ntp6[20]= zvtx;
        ntp6[21]= zvtx_proj;
        ntp6[22]= xvtx_proj;
        ntp6[23]= yvtx_proj;
        ntp6[24]= phi0;
        ntp6[25]= phi0b;
        ntp6[26]= xvtx_projc;
        ntp6[27]= yvtx_projc;
        ntp6[28]= dca0;
        ntp6[29]=magnetic_bend;
        ntp6[30]= pixel0ladder;
        ntp6[31]= pixel1ladder;
        ntp6[32]= pixel0sensor;
        ntp6[33]= pixel1sensor;
        ntp6[34]= dproj2;
        ntp6[35]= mag_bend2;
        ntp6[36]= dproj3;
        ntp6[37]= mag_bend3;
        ntp6[38]= dproj32;
        ntp6[39]= mag_bend32;
        ntp6[40] = strip0ladder;
        ntp6[41] = strip1ladder;
        ntp6[42] = L0x;
        ntp6[43] = L0y;
        ntp6[44] = L0z;
        ntp6[45] = L1x;
        ntp6[46] = L1y;
        ntp6[47] = L1z;
        ntp6[48] = L2x;
        ntp6[49] = L2y;
        ntp6[50] = L2z;
        ntp6[51] = L3x;
        ntp6[52] = L3y;
        ntp6[53] = L3z;
        ntp6[54] = seg0->get_dEdX1();
        ntp6[55] = seg0->get_dEdX2();

        ntpsvxsegment->Fill(ntp6);
      }
    
    }//if(zvtx)
  } // if EventNumber%100==0
*/


  ///////////////

/*
  if(EventNumber<100){
    fillRawCluster(svxrawhitclus, svxrawhit, svx);

    fillPxlChip(bbcq, zvtx, svxrawhitclus, svxrawhit, svx);
  }
*/

  fillCentralTrack(m_ntp_cnttrk,    bbc, vtxout, svxcnttrklist,   trk, container, mctrk, m_lowpt_ctr[0], m_midpt_ctr[0]);
  fillCentralTrack(m_ntp_cnttrk_bg, bbc, vtxout, svxcnttrkbglist, trk, container, mctrk, m_lowpt_ctr[1], m_midpt_ctr[1]);

  fillRpv3(rpsum);

  //fillTest(topNode);


  EventNumber++;
  return 0;
}

void SvxCntQA::fillPxlChip(float bbcq, float zvtx, SvxRawhitClusterList *rawclus, SvxRawhitList* rawhit, SvxClusterList* clus){
  if(verbosity>0) cout<<"fillPxlChip"<<endl;

  float ntp[100];

  bool noRawClus = (rawclus==NULL);
  bool noRawhit  = (rawhit==NULL);
  bool noClus    = (clus==NULL);
  if(noRawClus || noRawhit ||noClus ) {
    if(noRawClus) cout<<"No RawhitCluster Object"<<endl;
    if(noRawhit)  cout<<"No Rawhit Object"<<endl;
    if(noClus)    cout<<"No Cluster Object"<<endl;
    return;
  }

  int nhitary[60][8];
  int nlhitary[60][8];
  int sizeary[60][8];
  int lsizeary[60][8];
  int nrawhitary[60][8];
  for(int imod=0; imod<60; imod++){
    for(int iroc=0; iroc<8; iroc++){
      nhitary[imod][iroc]  = 0;
      nlhitary[imod][iroc] = 0;
      sizeary[imod][iroc] = 0;
      lsizeary[imod][iroc] = 0;
      nrawhitary[imod][iroc] = 0;
    }
  }

  //cluster
  int nclus = clus->get_nClusters();

  for(int i=0;i<nclus;i++) {
    SvxCluster *cluster = clus->get_Cluster(i);
    int layer  = cluster->get_layer();

    // check
    if(layer<2){
      int ladder = cluster->get_ladder();
      int sensor = cluster->get_sensor();
      float localz = cluster->get_xyz_local(2);
      int size  = cluster->get_size();
      int xsize = cluster->get_xz_size(0);
      int zsize = cluster->get_xz_size(1);

      int mod    = m_svxAdrObj->getPixelModuleID(layer, ladder, sensor);
      int chip   = get_ROC_pixel(layer, ladder, sensor, localz);

      //if(mod==0&&chip==0)cout<<"L : "<<i<<" "<<layer<<" "<<ladder<<" "<<sensor<<" "<<mod<<" "<<chip<<endl;
      if((0<=mod&&mod<60) && (0<=chip&&chip<8)) {
        nhitary[mod][chip]++;
        sizeary[mod][chip] += size;
        if(xsize>20&&zsize<5) {
          nlhitary[mod][chip]++;
          lsizeary[mod][chip] += size;
        }
      } else {
       // cout<<"ERROR : mod= "<<mod<<" chip= "<<chip<<endl;
      }
    }
  }

  //rawhit
  int nraw = rawhit->get_nRawhits();
  for(int i=0;i<nraw;i++) {
    SvxRawhit  *raw = rawhit->get_Rawhit(i);

    if(raw == NULL ) {
      cout<<EventNumber<<": error! NULL pointer"<<endl;
      cout << raw <<endl;
      continue;
    }

    int raw_layer = raw->get_layer();
    //int raw_ladder = raw->get_layer();
    //int raw_sensor = raw->get_sensor();
    //int raw_channel = raw->get_channel();
    int raw_pixelmodule = raw->get_pixelModule();
    int raw_pixelROC    = raw->get_pixelROC();

    if(raw_layer<2){
      //if(raw_pixelmodule==0&&raw_pixelROC==0){
      //  cout<<"Raw : "<<raw_layer<<" "<<raw_ladder<<" "<<raw_sensor<<" ";
      //  cout<<raw_pixelmodule<<" "<<raw_pixelROC<<" "<<raw_channel<<endl;
      //}
 
      if((0<=raw_pixelmodule&&raw_pixelmodule<60) && (0<=raw_pixelROC&&raw_pixelROC<8)) {
        nrawhitary[raw_pixelmodule][raw_pixelROC]++;
      } else {
        cout<<"ERROR : mod= "<<raw_pixelmodule<<" chip= "<<raw_pixelROC<<endl;
      }
    }
  }

/*
  // rawcluster
  int nrawclus = rawclus->get_nRawhitClusters();

  int prev_clus_id = -1;
  int rrid = 0;
  int nrhit=0;
  for(int i=0;i<nrawclus;i++) {
    SvxRawhitCluster *raw2clus = rawclus->get_RawhitCluster(i);
    int rawhit_id  = raw2clus->get_rawhitID();
    int cluster_id = raw2clus->get_clusterID();

    SvxRawhit  *raw = rawhit->get_Rawhit(rawhit_id);
    SvxCluster *cluster = clus->get_Cluster(cluster_id);

    if(raw == NULL || cluster == NULL) {
      cout<<EventNumber<<": error! NULL pointer"<<endl;
      cout << raw <<endl;
      cout << cluster <<endl;
      continue;
    }

    int raw_layer       = raw->get_layer();
    int raw_pixelmodule = raw->get_pixelModule();
    int raw_pixelROC    = raw->get_pixelROC();
    int c_size   = cluster->get_size();

    bool isSelected = (raw_layer<2&&raw_pixelmodule==0&&raw_pixelROC==0);

    nrhit++;
    if(cluster_id!=prev_clus_id){
      if(isSelected) {
        cout<<endl<<"cluster "<<cluster_id<<"("<<c_size<<") : "<<endl; //("<<nrhit<<") : "<<c_layer<<" "<<raw_pixelmodule<<" "<<raw_pixelROC<<endl;
      }

      prev_clus_id = cluster_id;
      rrid=0;
      nrhit=0;
    }

    if(isSelected){
      cout<<rawhit_id<<" "<<flush;
    }

  }
*/

  // fill
  for(int imod=0; imod<60; imod++){
    for(int iroc=0; iroc<8; iroc++){
      ntp[0] = EventNumber;
      ntp[1] = bbcq;
      ntp[2] = zvtx;
      ntp[3] = m_svxAdrObj->getPixelLayer(imod);
      ntp[4] = m_svxAdrObj->getPixelLadder(imod);
      ntp[5] = m_svxAdrObj->getPixelSensor(imod, iroc);
      ntp[6] = imod;
      ntp[7] = iroc;
      ntp[8] = nhitary[imod][iroc];
      ntp[9] = nlhitary[imod][iroc];
      ntp[10]= sizeary[imod][iroc];
      ntp[11]= lsizeary[imod][iroc];
      ntp[12]= nrawhitary[imod][iroc];
      ntp_evt_pxlchip->Fill(ntp);

/*
      if(imod==0&&iroc==0){
        cout<<"total ";
        cout<<": "<<sizeary[imod][iroc]<<" ";
        cout<<": "<<nrawhitary[imod][iroc]<<" ";
        cout<<endl;
      }
*/
    }
  }

}

void SvxCntQA::fillRawCluster(SvxRawhitClusterList *svxrawclus, SvxRawhitList* svxrawhit, SvxClusterList* svxclus){
  if(verbosity>0) cout<<"fillRawCluster"<<endl;

  float ntp7[100];


  bool noRawClus = (svxrawclus==NULL);
  bool noRawhit  = (svxrawhit==NULL);
  bool noClus    = (svxclus==NULL);

  if(noRawClus || noRawhit ||noClus ) {
    if(noRawClus) cout<<"No RawhitCluster Object"<<endl;
    if(noRawhit)  cout<<"No Rawhit Object"<<endl;
    if(noClus)    cout<<"No Cluster Object"<<endl;
    return;
  }


  //rawhit<-->cluster
  //  cout<<"nraw2clus="<<nsvxrawhitclus<<endl;
  int nsvxrawhitclus = svxrawclus->get_nRawhitClusters();

  int prev_clus_id = -1;
  int rrid = 0;
  for(int i=0;i<nsvxrawhitclus;i++) {
    SvxRawhitCluster *raw2clus = svxrawclus->get_RawhitCluster(i);
    int rawhit_id  = raw2clus->get_rawhitID();
    int cluster_id = raw2clus->get_clusterID();

    SvxRawhit  *rawhit = svxrawhit->get_Rawhit(rawhit_id);
    SvxCluster *cluster = svxclus->get_Cluster(cluster_id);

    if(rawhit == NULL || cluster == NULL) {
      cout<<EventNumber<<": error! NULL pointer"<<endl;
      cout << rawhit <<endl;
      cout << cluster <<endl;
      continue;
    }

    int raw_layer   = rawhit->get_layer();
    int raw_ladder  = rawhit->get_ladder();
    int raw_sensor  = rawhit->get_sensor();
    int raw_section = rawhit->get_sensorSection();
    int raw_readout = rawhit->get_sensorReadout();
    int raw_channel = rawhit->get_channel();
    //int raw_svxchan = raw_channel%128;
    //int raw_svxchip = raw_channel/128;
    int raw_pixelmodule = rawhit->get_pixelModule();
    int raw_pixelROC    = rawhit->get_pixelROC();
//    int pixel_module = rawhit->get_pixelModule();
//    int pixel_ROC    = rawhit->get_pixelROC();
    int raw_adc     = rawhit->get_adc();

    int cls_layer   = cluster->get_layer();
    int cls_ladder  = cluster->get_ladder();
    int cls_sensor  = cluster->get_sensor();
    float cls_lx    = cluster->get_xyz_local(0);
    float cls_lz    = cluster->get_xyz_local(2);
    float cls_x     = cluster->get_xyz_global(0);
    float cls_y     = cluster->get_xyz_global(1);
    float cls_z     = cluster->get_xyz_global(2);
    float cls_size  = cluster->get_size();
    float cls_xsize = cluster->get_xz_size(0);
    float cls_zsize = cluster->get_xz_size(1);
    int   cls_adc   = cluster->get_adc(0)+cluster->get_adc(1);

    static const int secary[7] = {2,3,1,3,1,3,2};

    int  raw_ix  = (cls_layer<2) ? m_svxAdrObj->getPixelRocIX0(raw_channel) 
                                 : m_svxAdrObj->getIX0(secary[raw_section], raw_channel);

    int  raw_iz  = (cls_layer<2) ? m_svxAdrObj->getPixelRocIZ0(raw_channel)
                                 : m_svxAdrObj->getIZ0(secary[raw_section], raw_channel);

    int  raw_ixs = (cls_layer<2) ? m_svxAdrObj->getPixelSensorIX0(raw_pixelROC, raw_channel)
                 : m_svxAdrObj->getSensorIX0(raw_layer, raw_ladder, raw_sensor, raw_channel, raw_pixelROC, raw_pixelmodule);
    int  raw_izs = (cls_layer<2) ? m_svxAdrObj->getPixelSensorIZ0(raw_pixelROC, raw_channel)
                 : m_svxAdrObj->getSensorIZ0(raw_layer, raw_ladder, raw_sensor, raw_channel, raw_pixelROC, raw_pixelmodule);

    float raw_lx = -999.0;
    float raw_lz = -999.0;
    if(cls_layer<2){
      raw_lx = get_sensorXpos_pixel(raw_layer, raw_ladder, raw_sensor, raw_section, raw_readout, raw_ixs);
      raw_lz = get_sensorZpos_pixel(raw_layer, raw_ladder, raw_sensor, raw_section, raw_readout, raw_izs);
    } else {
      raw_lx = get_sensorXpos_pixel(raw_layer, raw_ladder, raw_sensor, raw_section, raw_readout, raw_channel);
      raw_lz = get_sensorXpos_pixel(raw_layer, raw_ladder, raw_sensor, raw_section, raw_readout, raw_channel);
    }

    int idx=0; 
    ntp7[idx]=EventNumber;  idx++;
    ntp7[idx]=rawhit_id;    idx++;
    ntp7[idx]=rrid;         idx++;
    ntp7[idx]=raw_layer;    idx++;
    ntp7[idx]=raw_ladder;   idx++;
    ntp7[idx]=raw_sensor;   idx++;
    ntp7[idx]=raw_section;  idx++;
    ntp7[idx]=raw_readout;  idx++;
    ntp7[idx]=raw_channel;  idx++;
    ntp7[idx]=raw_pixelROC;    idx++;
    ntp7[idx]=raw_pixelmodule; idx++;
    ntp7[idx]=cluster_id;   idx++;
    ntp7[idx]=cls_layer;    idx++;
    ntp7[idx]=cls_ladder;   idx++;
    ntp7[idx]=cls_sensor;   idx++;
    ntp7[idx]=cls_lx;       idx++;
    ntp7[idx]=cls_lz;       idx++;
    ntp7[idx]=cls_x;        idx++;
    ntp7[idx]=cls_y;        idx++;
    ntp7[idx]=cls_z;        idx++;
    ntp7[idx]=raw_ix;       idx++;
    ntp7[idx]=raw_iz;       idx++;
    ntp7[idx]=raw_ixs;      idx++;
    ntp7[idx]=raw_izs;      idx++;
    ntp7[idx]=raw_lx;       idx++;
    ntp7[idx]=raw_lz;       idx++;
    ntp7[idx]=cls_size;     idx++;
    ntp7[idx]=cls_xsize;    idx++;
    ntp7[idx]=cls_zsize;    idx++;
    ntp7[idx]=raw_adc;      idx++;
    ntp7[idx]=cls_adc;      idx++;

    ntprawclus->Fill(ntp7);

    rrid++;
    if(cluster_id!=prev_clus_id){
      prev_clus_id = cluster_id;
      rrid=0;
    }

  }
}

void SvxCntQA::fillCentralTrack(TTree *ntp_trk, 
         BbcOut* bbcout, VtxOut* vtxout, SvxCentralTrackList* svxcnttrklist, PHCentralTrack *cntlist,
                              SvxClusterContainer *container, McEvalSingleList* mctrk, int &lowPtCtr, int &midPtCtr)
{

  if(verbosity>0) cout<<"fillCentralTrack"<<endl;

  if(bbcout==NULL || vtxout==NULL || svxcnttrklist==NULL || cntlist==NULL ){
    cout<<"fillCentralTrack :: Error "<<flush;
    if(bbcout==NULL)        cout<<" bbcout is NULL"<<flush;
    if(vtxout==NULL)        cout<<" vtxout is NULL"<<flush;
    if(svxcnttrklist==NULL) cout<<" centraltrack is NULL"<<flush;
    if(cntlist==NULL)       cout<<" cnt is NULL"<<flush;
//    if(container==NULL)     cout<<" container is NULL"<<flush;
    cout<<endl;
    return;
  }

  // init array
  for(int i=0; i<56; i++){
    *(m_c_datap[i]) = -9999.;
  }

  m_c_bbcz = vtxout->get_ZVertex("BBC");
  m_c_bbcq = bbcout->get_ChargeSum(Bbc::North) + bbcout->get_ChargeSum(Bbc::South);
  m_c_t0   = bbcout->get_TimeZero();

  //PHPoint vtxp_pos = vtxout->get_Vertex("SVX_PRECISE");
  PHPoint vtxp_pos = vtxout->get_Vertex();
  m_c_xvtx = vtxp_pos.getX();
  m_c_yvtx = vtxp_pos.getY();
  m_c_zvtx = vtxp_pos.getZ();

  m_c_zvtxp = vtxout->get_ZVertex("SVX_PRECISE");
  m_c_zvtxs = vtxout->get_ZVertex("SVX");

  PHPoint vtxs_pos = vtxout->get_Vertex("SVX");
  //m_c_xvtx = vtxp_pos.getX();
  //m_c_yvtx = vtxp_pos.getY();
  //m_c_zvtx = vtxp_pos.getZ();

  // initialize TrackArray
  int nCNT = cntlist->get_npart(); // nCNT track
  SvxCentralTrack **svxCntTrkAry = new SvxCentralTrack*[nCNT];
  for(int icnt=0; icnt<nCNT; icnt++){ svxCntTrkAry[icnt] = NULL; } // init by NULL

  // 
  int ntrk = svxcnttrklist->get_nCentralTracks(); // nSvxCentralTrack
  //cout<<"ntrk : "<<ntrk<<endl;

  for(int itrk=0; itrk<ntrk; itrk++){
    SvxCentralTrack *svxtrk = svxcnttrklist->getCentralTrack(itrk);

    int cntidx=svxtrk->getDchIndex();
    if(cntidx>=nCNT){
      cout<<"Error iCNT exceeds nCNT. : iCNT="<<cntidx<<", NCNT="<<nCNT<<endl;
      continue;
    }

    svxCntTrkAry[cntidx] = svxtrk;
  }


  //////////////////////////
  // fill Tree by nCNT 
  for(int icnt=0; icnt<nCNT; icnt++){
    SvxCentralTrack *svxtrk = svxCntTrkAry[icnt];

    if(svxtrk==NULL) {
      if(verbosity>2){ cout<<"no svxcnttrk object : "<<icnt<<"  , skip"<<endl;} 
//      continue;
    }

    int cntidx=icnt;
    //cout<<"DchIndex : "<<cntidx<<endl;

    m_c_mom    = cntlist->get_mom(cntidx);
    m_c_phi0   = cntlist->get_phi0(cntidx);
    m_c_the0   = cntlist->get_the0(cntidx);
    m_c_zed    = cntlist->get_zed(cntidx);
    m_c_c      = cntlist->get_charge(cntidx);
    m_c_dcqual = cntlist->get_quality(cntidx);
    m_c_emcdphi = cntlist->get_emcdphi(cntidx);
    m_c_emcdz   = cntlist->get_emcdz(cntidx);
    m_c_pc3dphi = cntlist->get_pc3dphi(cntidx);
    m_c_pc3dz   = cntlist->get_pc3dz(cntidx);
    m_c_pc2dphi = cntlist->get_pc2dphi(cntidx);
    m_c_pc2dz   = cntlist->get_pc2dz(cntidx);
    m_c_n0     = cntlist->get_n0(cntidx);
    m_c_cch2   = cntlist->get_chi2(cntidx);
    m_c_npe0   = cntlist->get_npe0(cntidx);
    m_c_disp   = cntlist->get_disp(cntidx);
    m_c_sn0    = cntlist->get_sn0(cntidx);
    m_c_scch2  = cntlist->get_schi2(cntidx);
    m_c_snpe0  = cntlist->get_snpe0(cntidx);
    m_c_sdisp  = cntlist->get_sdisp(cntidx);
    m_c_ecore  = cntlist->get_ecore(cntidx);
//    m_c_ecorr  = 0.0; //cntlist->get_ecorr(cntidx);


    float pt = m_c_mom*sin(m_c_the0);

    if(m_goodOnlyFlag&&(
        (fabs(m_c_emcdphi)> GOODEMCDPHI) ||
        (fabs(m_c_emcdz)  > GOODEMCDZ) ||
        (      m_c_dcqual < GOODDCQUALITY) ||
        (      pt         > 10.0) ||  //  Hard cut
        ( (pt> PTUPPERLIMIT) && ( (m_c_ecore/m_c_mom) < 0.15 ))
      )
    ) { // skip this track
      if(verbosity>1){
        cout<< "SvxCntQA  skip this track : ";
        cout<<  "emcdphi = "<<m_c_emcdphi<<" ";
        cout<<  "emcdz = "<<m_c_emcdz<<" ";
        cout<<  "dcqual = 0x"<<hex<<m_c_dcqual<<dec<<endl;
      }
      continue;
    }

    if(m_goodOnlyFlag) { // skip this track
      if(pt<PTTHRESHOLD){
        lowPtCtr++;
        if( lowPtCtr!=30 ) { /*cout<<"SvxCntQA skip track pT="<<pt<<" "<<lowPtCtr<<endl;*/ continue; }
        else               { lowPtCtr=0; }
      }
      else if(pt<2){
        midPtCtr++;
        if( midPtCtr!=3 ) { /*cout<<"SvxCntQA skip track pT="<<pt<<" "<<lowPtCtr<<endl;*/ continue; }
        else               { midPtCtr=0; }
      }
    }

    if(mctrk!=NULL){
      float simpx = mctrk->get_momentumx(cntidx);
      float simpy = mctrk->get_momentumy(cntidx);
      float simpz = mctrk->get_momentumz(cntidx);

      //cout<<"particle-px,py,pz : "<<simpx<<" "<<simpy<<" "<<simpz<<endl;
      
      m_c_simpt   =  sqrt(simpx*simpx+simpy*simpy);
      float simphi0 = atan2(simpy, simpx);
      if(simphi0<-Pi/2) simphi0 += 2*Pi;
      m_c_simphi0 =  simphi0;
      m_c_simthe0 =  atan2(m_c_simpt, simpz);
      m_c_simvx   =  mctrk->get_vertexx(cntidx);
      m_c_simvy   =  mctrk->get_vertexy(cntidx);
      m_c_simvz   =  mctrk->get_vertexz(cntidx);
      m_c_simdca  = calcSimD2DCA(m_c_simpt, m_c_c, m_c_simphi0, 
                              m_c_simvx, m_c_simvy, m_c_xvtx, m_c_yvtx);
      m_c_simpid  = mctrk->get_particleid(cntidx);

      //---------------------------
      float simpxpa = mctrk->get_parentmomentumx(cntidx);
      float simpypa = mctrk->get_parentmomentumy(cntidx);
      float simpzpa = mctrk->get_parentmomentumz(cntidx);

      //cout<<"parent-px,py,pz : "<<simpxpa<<" "<<simpypa<<" "<<simpzpa<<endl;
      
      m_c_simptpa   =  (simpxpa>-900) ? sqrt(simpxpa*simpxpa+simpypa*simpypa) : -999.;
      float simphi0pa = atan2(simpypa, simpxpa);
      if(simphi0pa<-Pi/2) simphi0pa += 2*Pi;
      m_c_simphi0pa =  simphi0pa;
      m_c_simthe0pa =  atan2(m_c_simptpa, simpzpa);
      m_c_simvxpa   =  mctrk->get_parentvertexx(cntidx);
      m_c_simvypa   =  mctrk->get_parentvertexy(cntidx);
      m_c_simvzpa   =  mctrk->get_parentvertexz(cntidx);
      m_c_simdcapa  = calcSimD2DCA(m_c_simptpa, m_c_c, m_c_simphi0pa, 
                              m_c_simvxpa, m_c_simvypa, m_c_xvtx, m_c_yvtx);
      m_c_simpidpa  = mctrk->get_parentid(cntidx);

      //---------------------------
      float simpxpr = mctrk->get_primarymomentumx(cntidx);
      float simpypr = mctrk->get_primarymomentumy(cntidx);
      float simpzpr = mctrk->get_primarymomentumz(cntidx);

      //cout<<"primary-px,py,pz : "<<simpxpr<<" "<<simpypr<<" "<<simpzpr<<endl;
      
      m_c_simptpr   =  (simpxpr>-900) ? sqrt(simpxpr*simpxpr+simpypr*simpypr) : -999.;
      float simphi0pr = atan2(simpypr, simpxpr);
      if(simphi0pr<-Pi/2) simphi0pr += 2*Pi;
      m_c_simphi0pr =  simphi0pr;
      m_c_simthe0pr =  atan2(m_c_simptpr, simpzpr);
      m_c_simvxpr   =  mctrk->get_primaryvertexx(cntidx);
      m_c_simvypr   =  mctrk->get_primaryvertexy(cntidx);
      m_c_simvzpr   =  mctrk->get_primaryvertexz(cntidx);
      m_c_simdcapr  = calcSimD2DCA(m_c_simptpr, m_c_c, m_c_simphi0pr, 
                              m_c_simvxpr, m_c_simvypr, m_c_xvtx, m_c_yvtx);
      m_c_simpidpr  = mctrk->get_primaryid(cntidx);
      
    } else {
      m_c_simpt     = -999.0;
      m_c_simphi0   = -999.0;
      m_c_simthe0   = -999.0;
      m_c_simvx     = -999.0;
      m_c_simvy     = -999.0;
      m_c_simvz     = -999.0;
      m_c_simdca    = -999.0;
      m_c_simpid    = -999;

      m_c_simptpa   = -999.0;
      m_c_simphi0pa = -999.0;
      m_c_simthe0pa = -999.0;
      m_c_simvxpa   = -999.0;
      m_c_simvypa   = -999.0;
      m_c_simvzpa   = -999.0;
      m_c_simdcapa  = -999.0;
      m_c_simpidpa  = -999;

      m_c_simptpr   = -999.0;
      m_c_simphi0pr = -999.0;
      m_c_simthe0pr = -999.0;
      m_c_simvxpr   = -999.0;
      m_c_simvypr   = -999.0;
      m_c_simvzpr   = -999.0;
      m_c_simdcapr  = -999.0;
      m_c_simpidpr  = -999;
      
    }



    if(svxtrk!=NULL){
      m_c_chi2 = svxtrk->getChiSquare();
      m_c_ndf  = svxtrk->getNDF();
      m_c_nhit = svxtrk->getNhits();
      m_c_chi22= svxtrk->getChiSquare2();
      m_c_dpchi2p= svxtrk->getChiSquareDPHI();
      m_c_dzchi2p= svxtrk->getChiSquareDZ();
      m_c_unique = svxtrk->getUnique();
      m_c_d2dca  = svxtrk->getDCA2D();
      m_c_d2dca0 = svxtrk->getD2DCA0();
      m_c_dedx1 = svxtrk->get_dEdX1();
      m_c_dedx2 = svxtrk->get_dEdX2();
      m_c_zdca  = svxtrk->getDCAZ();

//    bool isN4 = (svxtrk->getNhits()>3);
//    if(isN4){
//      cout<<"Particle : ";
//      cout<<cntlist->get_mom(cntidx)<<", ";
//      cout<<cntlist->get_charge(cntidx)<<", ";
//      cout<<cntlist->get_phi0(cntidx)<<", ";
//      cout<<cntlist->get_the0(cntidx)<<", ";
//      cout<<"Zvtx= "<<zvtx<<", ";
//      cout<<endl;
//    }

      for(int i=0; i<56; i++){ *(m_c_datap[i]) = -9999.; }

      float x0=-999., y0=-999./*, x1=-999., y1=-999.*/;
      int l1hit=100;
      int vlayer[4] = {-999,-999,-999,-999};
      static const int N_ELE = 14;
      for(int ihit=0; ihit<svxtrk->getNhits(); ihit++){
        SvxClusterInfo *info = svxtrk->getClusterInfo(ihit);
        int ilay=-1;
        if     (info->getLayer()==3){ ilay=3; }
        else if(info->getLayer()==2){ ilay=2; }
        else if(info->getLayer()==1){ ilay=1; }
        else if(info->getLayer()==0){ ilay=0; }
        else                        { cout<<"Error : ClusterLayer is "<<info->getLayer()<<" : "<<ihit<<endl; continue;}


        float xsvx = info->getPosition(0);
        float ysvx = info->getPosition(1);
        float zsvx = info->getPosition(2);
        if(ilay==0){x0=xsvx; y0=ysvx;}
        if(0<ilay&&ilay<4){
          if(ilay<l1hit){ // search a hit at the next inner layer
            l1hit=ilay;
            //--x1=xsvx; 
            //--y1=ysvx;
          }
        }

        float magbend, dproj, zproj;
        calc_dphidz(m_c_xvtx, m_c_yvtx, m_c_zvtx, 
                    xsvx, ysvx, zsvx, 
                    m_c_mom, m_c_c, m_c_phi0, m_c_the0,
                    &dproj, &magbend, &zproj);

         vlayer[3-ilay] = ilay;
        int sublayer = get_sublayer(ilay, info->getLadder()); // ilay

        float sdp, sdz;
        calc_sdphidz(m_c_mom*sin(m_c_the0), sublayer, dproj+magbend, zproj-zsvx, &sdp, &sdz);

        // dphi dz from BC
        float bdp, bmag, bdz;
        calc_dphidz(m_beam_x, m_beam_x, m_c_zvtx, 
                    xsvx, ysvx, zsvx, 
                    m_c_mom, m_c_c, m_c_phi0, m_c_the0,
                    &bdp, &bmag, &bdz);

        float phi_svx = atan2(ysvx, xsvx);
        if(phi_svx<-0.5*TMath::Pi()) phi_svx += 2*TMath::Pi(); // phi range : -0.5pi -- 1.5*pi

        *(m_c_datap[N_ELE*ilay+0]) = sublayer; // ilay
        *(m_c_datap[N_ELE*ilay+1]) = info->getLadder();
//        *(m_c_datap[N_ELE*ilay+2]) = zproj;
//        *(m_c_datap[N_ELE*ilay+3]) = dproj;
//        *(m_c_datap[N_ELE*ilay+4]) = magbend;
        *(m_c_datap[N_ELE*ilay+2]) = info->getzproj();
        *(m_c_datap[N_ELE*ilay+3]) = info->getdproj();
        *(m_c_datap[N_ELE*ilay+4]) = info->getbend();
        *(m_c_datap[N_ELE*ilay+5]) = zsvx;
        *(m_c_datap[N_ELE*ilay+6]) = phi_svx;
        *(m_c_datap[N_ELE*ilay+7]) = sqrt((xsvx-m_c_xvtx)*(xsvx-m_c_xvtx)+(ysvx-m_c_yvtx)*(ysvx-m_c_yvtx));
        *(m_c_datap[N_ELE*ilay+8]) = sdp;
        *(m_c_datap[N_ELE*ilay+9]) = sdz;
        *(m_c_datap[N_ELE*ilay+10])= bdp+bmag;
        *(m_c_datap[N_ELE*ilay+11])= bdz-zsvx;
        *(m_c_datap[N_ELE*ilay+12])= info->getfitdphi();
        *(m_c_datap[N_ELE*ilay+13])= info->getfitdz();

         //cout<<info->getdproj()<<":"<<dproj<<" "<<info->getbend()<<":"<<magbend<<" "<<info->getzproj()<<":"<<zproj<<endl;
      }

      // calculate chi2 for dphi and dz
      //int   vsublayer[4] = {*(m_c_datap[N_ELE*3]), *(m_c_datap[N_ELE*2]), *(m_c_datap[N_ELE*1]), *(m_c_datap[N_ELE*0])};
      float vdphi[4] = {(*(m_c_datap[N_ELE*3+3]))+(*(m_c_datap[N_ELE*3+4])), (*(m_c_datap[N_ELE*2+3]))+(*(m_c_datap[N_ELE*2+4])), 
                        (*(m_c_datap[N_ELE*1+3]))+(*(m_c_datap[N_ELE*1+4])), (*(m_c_datap[N_ELE*0+3]))+(*(m_c_datap[N_ELE*0+4]))};
      float vdz[4]   = {(*(m_c_datap[N_ELE*3+2]))-(*(m_c_datap[N_ELE*3+5])), (*(m_c_datap[N_ELE*2+2]))-(*(m_c_datap[N_ELE*2+5])), 
                        (*(m_c_datap[N_ELE*1+2]))-(*(m_c_datap[N_ELE*1+5])), (*(m_c_datap[N_ELE*0+2]))-(*(m_c_datap[N_ELE*0+5]))};

//      float vsdp[4] = {*(m_c_datap[N_ELE*3+8]), *(m_c_datap[N_ELE*2+8]), 
//                       *(m_c_datap[N_ELE*1+8]), *(m_c_datap[N_ELE*0+8])};
//      float vsdz[4] = {*(m_c_datap[N_ELE*3+9]), *(m_c_datap[N_ELE*2+9]), 
//                       *(m_c_datap[N_ELE*1+9]), *(m_c_datap[N_ELE*0+9])};

      int   dpndf=0, dzndf=0;
      float dpchi2=0.0, dzchi2=0.0;
      calc_chi2(m_c_mom, vlayer, vdphi, vdz, &dpchi2, &dzchi2, &dpndf, &dzndf);
      m_c_dpchi2 = dpchi2;
      m_c_dzchi2 = dzchi2;
      m_c_dpndf  = dpndf;
      m_c_dzndf  = dzndf;

/*
      calc_schi2(m_c_mom*sin(m_c_the0), vsublayer, vdphi, vdz, &dpchi2, &dzchi2, &dpndf, &dzndf);
      m_c_sdpchi2 = dpchi2;
      m_c_sdzchi2 = dzchi2;

      calc_sschi2(m_c_mom*sin(m_c_the0), vsublayer, vsdp, vsdz, &dpchi2, &dzchi2, &dpndf, &dzndf, m_c_dfsdp);
      m_c_ssdpchi2 = dpchi2;
      m_c_ssdzchi2 = dzchi2;
  
      calc_chi2p(m_c_mom, m_c_the0, vlayer, vdphi, vdz, &dpchi2, &dzchi2, &dpndf, &dzndf, m_c_dfdpp, m_c_dfdzz);
      m_c_dpchi2p = dpchi2;
      m_c_dzchi2p = dzchi2;
*/

      // calculate DCA
      static const float bdir =  -1.0;

      float dca, R, L;
/*
      calc_dca(m_c_mom, m_c_phi0, m_c_the0, m_c_c, bdir,
               x0, y0, m_c_xvtx, m_c_yvtx,
               &dca, &R, &L);

      m_c_R      = R;
      m_c_L      = L;
      m_c_d2dca1 = dca;
*/

      // calculate DCA w.r.t beam center
      calc_dca(m_c_mom, m_c_phi0, m_c_the0, m_c_c, bdir,
               x0, y0, vtxs_pos.getX(), vtxs_pos.getY(),
               &dca, &R, &L);
      m_c_d2dcab = dca;
      //cout<<"DCA : "<<m_c_d2dcab<<endl;

/*
      calc_dcafrom2hit(m_c_mom, m_c_phi0, m_c_the0, m_c_c, bdir,
               x0, y0, x1, y1, 
               vtxs_pos.getX(), vtxs_pos.getY(),
               &dca, &R, &L);
      m_c_d2dca2 = dca;
*/

/*
      // dca from input Beamcenter
      calc_dca(m_c_mom, m_c_phi0, m_c_the0, m_c_c, bdir,
               x0, y0, m_beam_x, m_beam_y,
               &dca, &R, &L);
      m_c_bd2dca = dca;
*/


      // fill 2nd-closest hit to the associated cluster.
      int   *data_n[4]  = {&m_c_exn0, &m_c_exn1, &m_c_exn2, &m_c_exn3};
      float *data_dp[4] = {m_c_exdp0, m_c_exdp1, m_c_exdp2, m_c_exdp3};
      float *data_dz[4] = {m_c_exdz0, m_c_exdz1, m_c_exdz2, m_c_exdz3};
      for(int ilay=0; ilay<2; ilay++){
        for(unsigned int ihit=0; ihit<MAX; ihit++){ // initialize
          data_dp[ilay][ihit] = -999.;
          data_dz[ilay][ihit] = -999.;
        }

        (*data_n[ilay]) = svxtrk->getNSecondHits(ilay);
        for(int ihit=0; ihit<(*data_n[ilay]); ihit++){
          SvxResidualInfo *res = svxtrk->getSecondHit(ilay, ihit);
          if(ihit>=MAX) break;
  
          data_dp[ilay][ihit] = res->getdphi();
          data_dz[ilay][ihit] = res->getdz();
        }
      }
     
/*
      if(container!=NULL){
        vector<residual_data> vresidual[4];
        for(int ilay=0; ilay<4; ilay++){
          searchSecondHit(ilay, hitinfo[ilay], container, 
                          m_c_xvtx, m_c_yvtx, m_c_zvtx,
                          m_c_mom, m_c_phi0, m_c_the0, m_c_c, 
                          vresidual[ilay]);

          sort(vresidual[ilay].begin(), vresidual[ilay].end(), residual_data::compare);

          // print
          //cout<<"layer : "<<ilay<<endl;
          //for_each(vresidual[ilay].begin(), vresidual[ilay].end(), print_resdata);

          for(unsigned int ihit=0; ihit<MAX; ihit++){
            data_dp[ilay][ihit] = -999.;
            data_dz[ilay][ihit] = -999.;
          }

          *(data_n[ilay]) = (vresidual[ilay].size()>=10) ? 10 : (int) vresidual[ilay].size();
          for(unsigned int ihit=0; ihit<vresidual[ilay].size(); ihit++){
            if(ihit>MAX) break;

            residual_data& dd = vresidual[ilay][ihit];
            data_dp[ilay][ihit] = dd.dphi;
            data_dz[ilay][ihit] = dd.dz;
          }
        }
      }
*/
    }
    else {
      m_c_chi2  = -999.0;
      m_c_ndf   = -999;
      m_c_nhit  = -999;
      m_c_chi22 = -999.0;
      m_c_d2dca = -999.0;
      m_c_dedx1 = -999.0;
      m_c_dedx2 = -999.0;
      m_c_d2dca0= -999.0;
      m_c_zdca  = -999.0;
      m_c_unique= -999;

      m_c_dpchi2 = -999.;
      m_c_dzchi2 = -999.;
      m_c_dpndf  = -999.;
      m_c_dzndf  = -999.;
//      m_c_sdpchi2 = -999.;
//      m_c_sdzchi2 = -999.;
      m_c_dpchi2p = -999.;
      m_c_dzchi2p = -999.;
//      m_c_ssdpchi2 = -999.;
//      m_c_ssdzchi2 = -999.;

//      m_c_R      = -999.;
//      m_c_L      = -999.;
//      m_c_d2dca1 = -999.;
      m_c_d2dcab = -999.;
//      m_c_d2dca2 = -999.;
//      m_c_bd2dca = -999.;

      for(int i=0; i<56; i++){ *(m_c_datap[i]) = -9999.; }

      //      int   *data_n[4]  = {&m_c_exn0, &m_c_exn1, &m_c_exn2, &m_c_exn3};
      float *data_dp[4] = {m_c_exdp0, m_c_exdp1, m_c_exdp2, m_c_exdp3};
      float *data_dz[4] = {m_c_exdz0, m_c_exdz1, m_c_exdz2, m_c_exdz3};

      for(int ilay=0; ilay<4; ilay++){
	//        data_n[ilay] = 0;
        for(unsigned int ihit=0; ihit<MAX; ihit++){
          data_dp[ilay][ihit] = -999.;
          data_dz[ilay][ihit] = -999.;
        }
      }


//      for(int i=0; i<6; i++){
//        m_c_dfsdp[i]=-999.; // 3-2,3-1,3-0,2-1,2-0,1-0
//        m_c_dfdpp[i]=-999.; // 3-2,3-1,3-0,2-1,2-0,1-0
//        m_c_dfdzz[i]=-999.; // 3-2,3-1,3-0,2-1,2-0,1-0
//      }


       m_c_exn3=-999.; m_c_exn2=-999.; m_c_exn1=-999.; m_c_exn0=-999.;
      for(int i=0; i<MAX; i++){
        m_c_exdz3[i]=-999.; m_c_exdp3[i]=-999.;
        m_c_exdz2[i]=-999.; m_c_exdp2[i]=-999.;
        m_c_exdz1[i]=-999.; m_c_exdp1[i]=-999.;
        m_c_exdz0[i]=-999.; m_c_exdp0[i]=-999.;
      }

    }


   // m_ntp_cnttrk->Fill();
    ntp_trk->Fill();
  }

  delete [] svxCntTrkAry;
}

void SvxCntQA::fillTest(PHCompositeNode *topNode){
  SvxCentralTrackList *svxcntlist = findNode::getClass<SvxCentralTrackList>(topNode, "SvxCentralTrackList");
  if(svxcntlist==NULL) {cout<<"fillTest :: no CentralTrackList "<<endl; return ;}

  int ntrk = svxcntlist->get_nCentralTracks();
  cout<<"fillTest:  ntrk "<<ntrk<<endl;
  for(int itrk=0; itrk<ntrk; itrk++){
     SvxCentralTrack *trk = svxcntlist->getCentralTrack(itrk);
     if(trk==NULL){
       cout<<"SvxCentralTrack is null : itrk="<<itrk<<endl;
       continue;
     }

     float chi2  = trk->getChiSquare();
     float ndf   = trk->getNDF();
     cout<<"fillTest:  itrk : "<<itrk<<", chi2="<<chi2<<", ndf="<<ndf<<endl;
  }
}

void SvxCntQA::calc_dca(float mom, float phi0, float the0, 
                      float c, float bdir,
                      float x, float y,
                      float xvtx, float yvtx,
                      float* dca, float* R, float* L, int v)
{
  if(x<-900){
    *R   = -999.;
    *L   = -999.;
    *dca = -999.;
    return;
  }

  //static const float b = 0.003*0.92;
  static const float b = 0.003*0.90;

  float pt = mom*sin(the0);

  float l = sqrt((x-xvtx)*(x-xvtx)+(y-yvtx)*(y-yvtx));
  float phi1 = phi0+c*(bdir*b*l/pt); // dphi = b*r/pT

  float ux = cos(phi1);
  float uy = sin(phi1);

  // radius of magnetic vending
  float r = (pt/b);
  //float cx = x + bdir*c*( uy)*r;// x of circle center
  //float cy = y + bdir*c*(-ux)*r;// y of circle center
  float cx = x + c*( uy)*r;// x of circle center
  float cy = y + c*(-ux)*r;// y of circle center

  // L btw C and V
  *R = r;
  *L = sqrt((cx-xvtx)*(cx-xvtx)+(cy-yvtx)*(cy-yvtx));
  *dca = c*((*R)-(*L));

  if(v>0) cout<<"DCA : "<<(*dca)<<endl;

}

void SvxCntQA::calc_dcafrom2hit(float mom, float phi0, float the0, 
                      float c, float bdir,
                      float x0, float y0, float x1, float y1,
                      float xvtx, float yvtx,
                      float* dca, float* R, float* L, int v)
{
  if(x0<-900||x1<-900){
    *R   = -999.;
    *L   = -999.;
    *dca = -999.;
    return;
  }


  static const float b = 0.003*0.90;

  float pt = mom*sin(the0);
  float r = (pt/b);

  float vx = x1-x0;
  float vy = y1-y0;
  float lv = sqrt(vx*vx+vy*vy);
  vx/=lv; // unit vector
  vy/=lv;

  float mx = 0.5*(x1+x0);
  float my = 0.5*(y1+y0);

  // length btw middle of (p0, p1) and rotation center
  float l = sqrt((r*r) - (0.25*lv*lv));

  // rotation center
  //float cx = mx + bdir*c*( vy)*l;// x of circle center
  //float cy = my + bdir*c*(-vx)*l;// y of circle center
  float cx = mx + c*( vy)*l;// x of circle center
  float cy = my + c*(-vx)*l;// y of circle center

  // L btw C and V
  *R = r;
  *L = sqrt((cx-xvtx)*(cx-xvtx)+(cy-yvtx)*(cy-yvtx));
  *dca = c*((*R)-(*L));

  if(v>0) cout<<"DCA : "<<(*dca)<<endl;
}

float SvxCntQA::calcSimD2DCA(float pt, float charge, float phi0, float hx, float hy, float vx, float vy){
  static const float B = 0.90;
  static const float b = 0.003*B;

  float R = pt/b;
  float cx = hx + charge*R*sin(phi0);
  float cy = hy - charge*R*cos(phi0);

  float L = sqrt((vx-cx)*(vx-cx) + (vy-cy)*(vy-cy));
  //float psi = atan2((vy-cy), (vx-cx));

  // DCA point
  //float xx = cx + R*cos(psi);
  //float xy = cy + R*sin(psi);

  // DCA value
  float d2dca = charge*(R - L);
  return d2dca;
}




void SvxCntQA::calc_dphidz(float xvtx, float yvtx, float zvtx, 
                   float xsvx, float ysvx, float zsvx, 
                   float mom, float charge, float phi0, float the0,
                   float* dproj, float* magbend, float* zproj
                  )
{
    float ux = cos(phi0);
    float uy = sin(phi0);
    float pt = mom*sin(the0);

    float dx = xsvx - xvtx;
    float dy = ysvx - yvtx;
    float rhit_2 = dx*dx+dy*dy;
    float rhit = sqrt(rhit_2);
    
    // vector product (ux,uy) x (dx, dy)
    // this is signed difference between straight line projection
    // from beam spot (xoffset1,yoffset1) and a svx hit position.
    float u_cross_dx = ux*dy - uy*dx;
    (*magbend) = -m_fieldScale*(0.0013*charge*rhit_2/pt);

    (*dproj)   = u_cross_dx;
    (*zproj)   = zvtx + rhit/tan(the0);
}

void SvxCntQA::calc_sdphidz(float pt, int layer, float dp, float dz, float* sdp, float* sdz){
  float sig_dphi = getSigmaDphi(layer, pt);
  float sig_dz   = getSigmaDphi(layer, pt); // temporary


  float s_dphi = (sig_dphi>-990) ? dp/sig_dphi : -999.; // dproj+bend
  float s_dz   = (sig_dz  >-990) ? dz/sig_dz   : -999.; // zproj-zsvx

  *sdp = s_dphi;
  *sdz = s_dz;
}

void SvxCntQA:: calc_chi2p(float mom, float the0, int* vlayer, float* vdphi, float *vdz,
                        float *r_dpchi2, float *r_dzchi2, int* r_dpndf, int* r_dzndf, float *vdfdpp, float *vdfdzz)
{
  int   dpndf=0, dzndf=0;
  float dpchi2=0.0, dzchi2=0.0;

  for(int i=0; i<6; i++){ vdfdpp[i] = -999.0; vdfdzz[i] = -999.0;}

  //cout<<"calc_chi2 "<<endl;
  int prevlay=-999;
  float prev_ddphi=-999.0, prev_ddz=-999.0;
  for(int ilay=0; ilay<4; ilay++){
    int   layer = vlayer[ilay];

    float ddphi = vdphi[ilay]; // dproj+bend
    float ddz   = vdz[ilay];   // zproj-zsvx

//    cout<<prevlay<<" "<<layer<<" "<<vlayer[ilay]<<" : "<<prev_ddphi<<" "<<ddphi<<endl;

    if(prevlay>=0&&layer>=0){ // hit in both layer
      float s_dp, s_dz;
      getCorrelationSlopeP(prevlay, layer, &s_dp, &s_dz);
      if(s_dp<-1000) continue;

      float diff_dp = prev_ddphi - s_dp*ddphi;
      float diff_dz = prev_ddz   - s_dz*ddz;

      float sig_dp, sig_dz;
      getCorrelationSigmaP(mom, the0, prevlay, layer, &sig_dp, &sig_dz);
      if(sig_dp<-1000) continue;

//      cout<<" ("<<layer<<" "<<prevlay<<") diff : "<<diff_dp<<" "<<diff_dz<<" "<<s_dp<<" "<<s_dz<<" "<<sig_dp<<" "<<sig_dz<<endl;

      int corrIdx = getCorrelationIdx(prevlay, layer);
      if(corrIdx>=0) {
        vdfdpp[corrIdx] = diff_dp/sig_dp;
        vdfdzz[corrIdx] = diff_dz/sig_dz;
      }


      dpchi2 += (diff_dp*diff_dp)/(sig_dp*sig_dp);
      dzchi2 += (diff_dz*diff_dz)/(sig_dz*sig_dz);
      dpndf++;
      dzndf++;
    }


    if(layer>=0){ // hit
      prevlay    = layer;
      prev_ddphi = ddphi; // dproj+bend
      prev_ddz   = ddz;   // zproj-zsvx
    }
  }

  *r_dpchi2 = dpchi2;
  *r_dzchi2 = dzchi2;
  *r_dpndf  = dpndf;
  *r_dzndf  = dzndf;
}

void SvxCntQA::calc_sschi2(float pt, int* vlayer, float* vsdp, float *vsdz, 
         float *r_dpchi2, float *r_dzchi2, int* r_dpndf, int* r_dzndf, float *vdfsdp)
{
  int   dpndf=0, dzndf=0;
  float dpchi2=0.0, dzchi2=0.0;

  for(int i=0; i<6; i++){ vdfsdp[i] = -999.0;}

  //cout<<"calc_chi2 "<<endl;
  int prevlay=-999;
  float prev_sdphi=-999.0, prev_sdz=-999.0;
  for(int ilay=0; ilay<4; ilay++){
    int   layer = -999;
    if     (vlayer[ilay]==0||vlayer[ilay]==1) layer = vlayer[ilay];
    else if(2<=vlayer[ilay]&&vlayer[ilay]<5)  layer = 2;
    else if(5<=vlayer[ilay]&&vlayer[ilay]<8)  layer = 3;
  
    float sdphi = vsdp[ilay];
    float sdz   = vsdz[ilay];

    //cout<<prevlay<<" "<<layer<<" : "<<prev_ddphi<<" "<<ddphi<<endl;

    if(prevlay>=0&&layer>=0){ // hit in both layer

      float diff_sdp = sdphi - prev_sdphi;
      float diff_sdz = sdz   - prev_sdz;

      float sig_sdp, sig_sdz;
      getCorrelationSSigma(pt, prevlay, layer, &sig_sdp, &sig_sdz);

      int corrIdx = getCorrelationIdx(prevlay, layer);
      if(corrIdx>=0) vdfsdp[corrIdx] = diff_sdp/sig_sdp;


      dpchi2 += ((diff_sdp*diff_sdp)/(sig_sdp*sig_sdp)); 
      dzchi2 += ((diff_sdz*diff_sdz)/(sig_sdz*sig_sdz)); 
      dpndf++;
      dzndf++;

      //cout<<" calc : "<<prevlay<<" "<<layer<<" : "<<prev_ddphi<<" "<<ddphi<<" "<<s_dp<<" : ";
      //cout<<diff_dp*diff_dp<<" "<<sig_dp<<endl;

    }


    if(layer>=0){ // hit
      prevlay    = layer;
      prev_sdphi = sdphi; // dproj+bend
      prev_sdz   = sdz;   // zproj-zsvx
    }
  }

  *r_dpchi2 = dpchi2;
  *r_dzchi2 = dzchi2;
  *r_dpndf  = dpndf;
  *r_dzndf  = dzndf;
}

// new 
void SvxCntQA:: calc_schi2(float pt, int* vlayer, float* vdphi, float *vdz,
                        float *r_dpchi2, float *r_dzchi2, int* r_dpndf, int* r_dzndf)
{
  int   dpndf=0, dzndf=0;
  float dpchi2=0.0, dzchi2=0.0;

  //cout<<"calc_chi2 "<<endl;
  int prevlay=-999;
  float prev_sdphi=-999.0, prev_sdz=-999.0;
  for(int ilay=0; ilay<4; ilay++){
    int   layer = vlayer[ilay];
  
    float sig_dphi = getSigmaDphi(layer, pt);
    float sig_dz   = getSigmaDphi(layer, pt); // temporary


    float sdphi = (sig_dphi>-990) ? vdphi[ilay]/sig_dphi : -999.; // dproj+bend
    float sdz   = (sig_dz  >-990) ? vdz[ilay]  /sig_dz   : -999.; // zproj-zsvx

    //cout<<prevlay<<" "<<layer<<" : "<<prev_ddphi<<" "<<ddphi<<endl;

    if(prevlay>=0&&layer>=0){ // hit in both layer

//      float s_dp, s_dz;

      float diff_sdp = sdphi - prev_sdphi;
      float diff_sdz = sdz   - prev_sdz;

      dpchi2 += ((diff_sdp*diff_sdp)/(1.0*1.0)); // assuming sigma=1
      dzchi2 += ((diff_sdz*diff_sdz)/(1.0*1.0)); // assuming sigma=1
      dpndf++;
      dzndf++;

      //cout<<" calc : "<<prevlay<<" "<<layer<<" : "<<prev_ddphi<<" "<<ddphi<<" "<<s_dp<<" : ";
      //cout<<diff_dp*diff_dp<<" "<<sig_dp<<endl;

    }


    if(layer>=0){ // hit
      prevlay    = layer;
      prev_sdphi = sdphi; // dproj+bend
      prev_sdz   = sdz;   // zproj-zsvx
    }
  }

  *r_dpchi2 = dpchi2;
  *r_dzchi2 = dzchi2;
  *r_dpndf  = dpndf;
  *r_dzndf  = dzndf;
}

 // older version
void SvxCntQA:: calc_chi2(float mom, int* vlayer, float* vdphi, float *vdz,
                        float *r_dpchi2, float *r_dzchi2, int* r_dpndf, int* r_dzndf)
{
  int   dpndf=0, dzndf=0;
  float dpchi2=0.0, dzchi2=0.0;

  //cout<<"calc_chi2 "<<endl;
  int prevlay=-999;
  float prev_ddphi=-999.0, prev_ddz=-999.0;
  for(int ilay=0; ilay<4; ilay++){
    int   layer = vlayer[ilay];
    float ddphi = vdphi[ilay]; // dproj+bend
    float ddz   = vdz[ilay];   // zproj-zsvx

    //cout<<prevlay<<" "<<layer<<" : "<<prev_ddphi<<" "<<ddphi<<endl;

    if(prevlay>=0&&layer>=0){ // hit in both layer
      float s_dp, s_dz;
      //getCorrelationSlope(prevlay, layer, &s_dp, &s_dz);
      getCorrelationSlopeSub(prevlay, layer, &s_dp, &s_dz);
      if(s_dp<-1000) continue;

      float diff_dp = ddphi - s_dp*prev_ddphi;
      float diff_dz = ddz   - s_dz*prev_ddz;

      float sig_dp, sig_dz;
      //getCorrelationSigma(mom, prevlay, layer, &sig_dp, &sig_dz);
      getCorrelationSigmaSub(mom, prevlay, layer, &sig_dp, &sig_dz);
      if(sig_dp<-1000) continue;

      dpchi2 += (diff_dp*diff_dp)/(sig_dp*sig_dp);
      dzchi2 += (diff_dz*diff_dz)/(sig_dz*sig_dz);
      dpndf++;
      dzndf++;

      //cout<<" calc : "<<prevlay<<" "<<layer<<" : "<<prev_ddphi<<" "<<ddphi<<" "<<s_dp<<" : ";
      //cout<<diff_dp*diff_dp<<" "<<sig_dp<<endl;

    }


    if(layer>=0){ // hit
      prevlay    = layer;
      prev_ddphi = ddphi; // dproj+bend
      prev_ddz   = ddz;   // zproj-zsvx
    }
  }

  *r_dpchi2 = dpchi2;
  *r_dzchi2 = dzchi2;
  *r_dpndf  = dpndf;
  *r_dzndf  = dzndf;
}


int  SvxCntQA::getCorrelationIdx(int prevlay, int lay){ // prevlay : outer, lay : inner

  int idx=0;
  if     (lay==2&&prevlay==3) idx=0;
  else if(lay==1&&prevlay==3) idx=1;
  else if(lay==0&&prevlay==3) idx=2;
  else if(lay==1&&prevlay==2) idx=3;
  else if(lay==0&&prevlay==2) idx=4;
  else if(lay==0&&prevlay==1) idx=5;
  else                        idx=-1;

  return idx;
}

void SvxCntQA::getCorrelationSlope(int prevlay, int lay, float *s_dp, float *s_dz){ // prevlay : outer, lay : inner
//  return correlation slope, 

  // idx=    0,     1,     2,     3,     4,     5  
  //     B2-B3, B1-B3, B0-B3, B1-B2, B0-B2, B0-B1
  static const float dp_slope[6] = 
      {0.743, 0.343, 0.170, 0.459, 0.218, 0.466}; // udpate 2012.02.01 from real data
  static const float dz_slope[6] = 
      {0.646, 0.356, 0.257, 0.539, 0.403, 0.693}; // udpate 2012.02.01


  //  if layer==layer_prev, slope should be one since the radius of layer is same.
  if(lay==prevlay) {
    *s_dp = 1.0;
    *s_dz = 1.0;
     return;
  }

  int idx = getCorrelationIdx(prevlay, lay);
  if(idx<0){
    *s_dp = -9999;
    *s_dz = -9999;
    return ;
  }

  *s_dp = dp_slope[idx];
  *s_dz = dz_slope[idx];
}

void SvxCntQA::getCorrelationSigma(float mom, int prevlay, int lay, float *sig_dp, float *sig_dz){ // prevlay : outer, lay : inner
  // update 2012.02.01
  // function for r(dphi) f(x) = a + b/(x)
  static const float a_r[6] = 
     {0.00511149, 0.00311288, 0.00310955, 0.00276798, 0.0027434 , 0.00156031};
  static const float b_r[6] = 
     {0.0119503,  0.00956115, 0.00677535, 0.00766647, 0.00617688, 0.00557547 };

  // function for z(dz) f(x) = a + b/(x*x)
  static const float a_z[6] = 
     {0.059567, 0.0567594, 0.0569289, 0.0433993, 0.0496085, 0.0269911};
  static const float b_z[6] = 
     {0.00128272, 0.000278275, -0.000415167, 0.000285727, 0.00029582, 0.00065861};

  //float mom = trk->m_central->get_mom();

  int idx =getCorrelationIdx(prevlay, lay);
  if(idx<0){
    *sig_dp = -999.9;
    *sig_dz = -999.9;
    return;
  }

  float sigmar =  a_r[idx] + (b_r[idx]/(mom));
  float sigmaz =  a_z[idx] + (b_z[idx]/(mom*mom));

  *sig_dp = sigmar;
  *sig_dz = sigmaz;


}

int  SvxCntQA::getCorrelationIdxSub(int prevlay, int lay){ // prevlay : outer, lay : inner sublayer
  int idx=0;
  if     (lay==4&&prevlay==7) idx=0;
  else if(lay==3&&prevlay==7) idx=1;
  else if(lay==2&&prevlay==7) idx=2;
  else if(lay==4&&prevlay==6) idx=3;
  else if(lay==3&&prevlay==6) idx=4;
  else if(lay==2&&prevlay==6) idx=5;
  else if(lay==4&&prevlay==5) idx=6;
  else if(lay==3&&prevlay==5) idx=7;
  else if(lay==2&&prevlay==5) idx=8;
  else if(lay==1&&prevlay==7) idx=9;
  else if(lay==1&&prevlay==6) idx=10;
  else if(lay==1&&prevlay==5) idx=11;
  else if(lay==0&&prevlay==7) idx=12;
  else if(lay==0&&prevlay==6) idx=13;
  else if(lay==0&&prevlay==5) idx=14;
  else if(lay==1&&prevlay==4) idx=15;
  else if(lay==1&&prevlay==3) idx=16;
  else if(lay==1&&prevlay==2) idx=17;
  else if(lay==0&&prevlay==4) idx=18;
  else if(lay==0&&prevlay==3) idx=19;
  else if(lay==0&&prevlay==2) idx=20;
  else if(lay==0&&prevlay==1) idx=21;
  else                        idx=-1;

  return idx;
}

void SvxCntQA::getCorrelationSlopeSub(int prevlay, int lay, float *s_dp, float *s_dz){ // prevlay : outer, lay : inner
//  return correlation slope, 

  // idx=    0,     1,     2,     3,     4,     5  
  //     B2-B3, B1-B3, B0-B3, B1-B2, B0-B2, B0-B1
  static const float dp_slope[22] = {
    0.810672, 0.735183, 0.685084, 0.812625,
    0.755598, 0.696203, 0.850584, 0.616239,
    0.741347, 0.32715, 0.340349, 0.369129,
    0.159839, 0.168023, 0.180343, 0.433159,
    0.453675, 0.503934, 0.207277, 0.208916,
    0.247864, 0.496796};


  //  if layer==layer_prev, slope should be one since the radius of layer is same.
  if(lay==prevlay) {
    *s_dp = 1.0;
    *s_dz = 1.0;
     return;
  }

  int idx = getCorrelationIdxSub(prevlay, lay);
  if(idx<0){
    *s_dp = -9999;
    *s_dz = -9999;
    return ;
  }

  *s_dp = dp_slope[idx];
  *s_dz = dp_slope[idx]; // should be from dz
//  *s_dz = dz_slope[idx];
}

void SvxCntQA::getCorrelationSigmaSub(float mom, int prevlay, int lay, float *sig_dp, float *sig_dz){ // prevlay : outer, lay : inner
  // update 2012.02.08
  // function for r(dphi) f(x) = a + b/(x^c)
  float par_dp[22][3] = {
     {-0.000309968, 0.0146056  , 0.756618}, //  0 : 7 4 : 
     {0.000884045 , 0.0161722  , 0.768782}, //  1 : 7 3 : 
     {0.0117671   , 0.00856911 , 1.08942 }, //  2 : 7 2 : 
     {0.00134545  , 0.00990758 , 0.954454}, //  3 : 6 4 : 
     {0.000881634 , 0.0119418  , 0.827378}, //  4 : 6 3 : 
     {-0.000275519, 0.0155339  , 0.818915}, //  5 : 6 2 : 
     {0.00198031  , 0.00690343 , 0.97673 }, //  6 : 5 4 : 
     {0.0122611   , 0.00036956 , 2.9128  }, //  7 : 5 3 : 
     {0.00250917  , 0.0100059  , 0.920852}, //  8 : 5 2 : 
     {0.00462298  , 0.00803084 , 0.996428}, //  9 : 7 1 : 
     {0.00268112  , 0.00794925 , 1.0678  }, // 10 : 6 1 : 
     {0.00520296  , 0.0060135  , 1.12243 }, // 11 : 5 1 : 
     {0.00387491  , 0.00569721 , 1.00056 }, // 12 : 7 0 : 
     {0.00393607  , 0.00438397 , 1.27609 }, // 13 : 6 0 : 
     {0.00676021  , 0.00244637 , 1.59586 }, // 14 : 5 0 : 
     {0.00461037  , 0.00449814 , 1.23283 }, // 15 : 4 1 : 
     {0.00346966  , 0.00473196 , 1.15084 }, // 16 : 3 1 : 
     {0.0042309   , 0.00390295 , 1.18032 }, // 17 : 2 1 : 
     {0.00529488  , 0.00263481 , 1.44116 }, // 18 : 4 0 : 
     {0.00405534  , 0.00344066 , 1.19283 }, // 19 : 3 0 : 
     {0.00454982  , 0.00258512 , 1.37532 }, // 20 : 2 0 : 
     {0.00405311  , 0.000855011, 1.76088 }  // 21 : 1 0 : 
    };


  int idx =getCorrelationIdxSub(prevlay, lay);
  if(idx<0){
    *sig_dp = -999.9;
    *sig_dz = -999.9;
    return;
  }

  float sigmar =  par_dp[idx][0] + (par_dp[idx][1]/pow(mom, par_dp[idx][2]));
  float sigmaz =  par_dp[idx][0] + (par_dp[idx][1]/pow(mom, par_dp[idx][2])); // this should be for dz

  *sig_dp = sigmar;
  *sig_dz = sigmaz;
}

float SvxCntQA::getSigmaDphi(int layer, float mom){
  // function for r(dphi) f(x) = a + b/(x^c)
/*
  static float par_dp[8][3] = {
   {0.00801996, 0.00740719, 1.24002 },
   {0.00975302, 0.0162047 , 1.23965 },
   {0.0126609 , 0.0347711 , 1.10922 },
   {0.0123018 , 0.041229  , 1.04151 },
   {0.0119073 , 0.0447959 , 0.995272},
   {0.0171721 , 0.0443585 , 1.06706 },
   {0.0151594 , 0.0515581 , 1.00403 },
   {0.0115162 , 0.0582186 , 0.906259}
  };
  static float par_dp[8][3] = {
   {0.00763272, 0.00755191, 1.23016 },
   {0.00860331, 0.0173366 , 1.17043 },
   {0.0107568 , 0.0361876 , 1.05556 },
   {0.010467  , 0.0432081 , 0.961144},
   {0.0119581 , 0.0431948 , 0.984436},
   {0.0132802 , 0.0480749 , 0.968738},
   {0.0135021 , 0.0527422 , 0.943635},
   {0.014004  , 0.0538106 , 0.926481}
  };
*/

  static float par_dp[8][3] = {
   {0.00754251, 0.00794327, 1.26527 },
   {0.00821107, 0.0180555 , 1.12351 },
   {0.0107064 , 0.0374272 , 1.04341 },
   {0.0110041 , 0.0438155 , 1.00128 },
   {0.0120134 , 0.0453299 , 1.00378 },
   {0.0130942 , 0.0501797 , 0.955478},
   {0.014019  , 0.0539896 , 0.973233},
   {0.0145239 , 0.0558408 , 0.950726}
  };

  int idx= (0<=layer&&layer<8) ? layer : -1;
  if(idx<0){
    return -999.0;
  }

  float sigma =  par_dp[idx][0] + (par_dp[idx][1]/pow(mom, par_dp[idx][2]));
  return sigma;
}

void SvxCntQA::getCorrelationSSigma(float pt, int prevlay, int lay, float *sig_dp, float *sig_dz){
   // prevlay : outer, lay : inner sublayer
   // function : [0] + [1]/pow(pt, [2])*exp([3]*pt)

  float sdp_par[6][4] = {
   {0.150583, 0.0285934 ,  0.961353,  0.287619},  // 2-3
   {0.372564, 0.00579636, -3.13882 , -0.400742},  // 1-3
   {0.491746, 0.0858156 , -1.42712 , -0.203167},  // 0-3
   {0.250874, 0.0228763 , -1.92888 , -0.246507},  // 1-2
   {0.380624, 0.11531   , -1.25177 , -0.190733},  // 0-2
   {0.154449, 0.134064  , -0.727381, -0.135233}   // 0-1
  };

  int idx = getCorrelationIdx(prevlay, lay);
  if(idx<0){
    *sig_dp = -999.9;
    *sig_dz = -999.9;
    return;
  }

  float sigdp = sdp_par[idx][0] + (sdp_par[idx][1]/pow(pt, sdp_par[idx][2]))*exp(pt*sdp_par[idx][3]);

  *sig_dp = sigdp;
  *sig_dz = sigdp; // tmp

}

void SvxCntQA::getCorrelationSlopeP(int prevlay, int lay, float *s_dp, float *s_dz){ // prevlay : outer, lay : inner
//  return correlation slope, 

  // idx=    0,     1,     2,     3,     4,     5  
  //     B2-B3, B1-B3, B0-B3, B1-B2, B0-B2, B0-B1
  static const float dp_slope[6] = {
    0.9323, 0.8412, 0.8105, 0.9081, 0.8736, 0.9656 // udpate 2012.04.20 from simulation
  };

  static const float dz_slope[6] = {
    0.9589, 0.9656, 0.9537, 0.9799, 0.9680, 0.9888 // udpate 2012.02.01
  };


  //  if layer==layer_prev, slope should be one since the radius of layer is same.
  if(lay==prevlay) {
    *s_dp = 1.0;
    *s_dz = 1.0;
     return;
  }

  int idx = getCorrelationIdx(prevlay, lay);
  if(idx<0){
    *s_dp = -9999;
    *s_dz = -9999;
    return ;
  }

  *s_dp = dp_slope[idx];
  *s_dz = dz_slope[idx];
}

void SvxCntQA::getCorrelationSigmaP(float mom, float the0, int prevlay, int lay, float *sig_dp, float *sig_dz){ // prevlay : outer, lay : inner
  // update 2012.02.01
  // function for r(dphi) f(x) = a + b/(x^c)
  // dp : x = pt, dz : x = mom
  float par_dphi[6][3] = {
    {0.00457144, 0.0156314, 0.977633}, 
    {0.00886788, 0.03807  , 0.907622}, 
    {0.0101447 , 0.0459372, 0.893003}, 
    {0.00547856, 0.0255631, 0.916481}, 
    {0.00707236, 0.0346052, 0.912854}, 
    {0.00259965, 0.0101421, 1.03904 }
  };


  float par_dz[6][3] = {
    {0.039607 , 0.00416093, 1.84448}, 
    {0.0358872, 0.0153115 , 1.79397},
    {0.0383161, 0.020577  , 1.77653},
    {0.0325563, 0.0104057 , 1.6811 },
    {0.0344724, 0.0162341 , 1.65492},
    {0.0165588, 0.00483856, 1.68208} 
  };

  //float mom = trk->m_central->get_mom();
  float pt = mom*sin(the0);

  int idx =getCorrelationIdx(prevlay, lay);
  if(idx<0){
    *sig_dp = -999.9;
    *sig_dz = -999.9;
    return;
  }

  float sigmar =  par_dphi[idx][0] + (par_dphi[idx][1])/(pow(pt,  par_dphi[idx][2]));
  float sigmaz =  par_dz[idx][0]   + (par_dz[idx][1])  /(pow(mom, par_dz[idx][2]));

  *sig_dp = sigmar;
  *sig_dz = sigmaz;


}

void SvxCntQA::searchSecondHit(int layer, SvxClusterInfo *hit, SvxClusterContainer *container, 
                             float xvtx, float yvtx, float zvtx, 
                             float mom, float phi0, float the0, float c, 
                             vector<residual_data>& vlist)
{

  vlist.clear();

  if(container==NULL) {
    cout<<"container is null"<<endl;
    return;
  }

  if(hit!=NULL){
    float xsvx = hit->getPosition(0);
    float ysvx = hit->getPosition(1);
    float zsvx = hit->getPosition(2);

    float phi = atan2(ysvx, xsvx);
    if(phi<-0.5*TMath::Pi()) phi += 2*TMath::Pi(); // phi range : -0.5pi -- 1.5*pi

  
    vector<SvxCluster*> vclus;
    vector<int> vsublayer;
    container->find_clusters_block(vclus, vsublayer, layer, phi, 0.2, zsvx, 0.5); // dp=0.2rad, dz=0.5cm
  
    // choose closest 10 hits with this track from list
    for(int icls=0; icls<(int)vclus.size(); icls++){
      SvxCluster *clus = vclus[icls];
      float xhit = clus->get_xyz_global(0);
      float yhit = clus->get_xyz_global(1);
      float zhit = clus->get_xyz_global(2);

      if(fabs(xhit-xsvx)<0.0001&&fabs(yhit-ysvx)<0.0001) continue; // remove same hit

      float magbend, dproj, zproj;
      calc_dphidz(xvtx, yvtx, zvtx, 
                  xhit, yhit, zhit, 
                  mom, c, phi0, the0,
                  &dproj, &magbend, &zproj);

      vlist.push_back(residual_data(dproj+magbend, zproj-zhit));
    }
  }
}

int SvxCntQA::get_sublayer(int layer, int ladder){
{
  if ( layer<2 ) {
    return layer;
  } 
  else if ( layer==2 ) {
    if ( ladder<8 ) {
      if(       ladder%3==1 ) { return 2; }
      else if ( ladder%3==0 ) { return 3; }
      else                    { return 4; }
    } else {
      if(       ladder%3==2 ) { return 2; }
      else if ( ladder%3==0 ) { return 3; }
      else                    { return 4; }
    }
  } else {
    if ( ladder<12 ) {
      if(       ladder%3==0 ) { return 5; }
      else if ( ladder%3==1 ) { return 6; }
      else                    { return 7; }
    } else {
      if(       ladder%3==2 ) { return 5; }
      else if ( ladder%3==1 ) { return 6; }
      else                    { return 7; }
    }
  }
}

}

void SvxCntQA::fillRpv3(RpSumXYObject *rpsum){
  if(verbosity>0) cout<<"fillPpv3"<<endl;

  if(rpsum==NULL){
    if(verbosity>0) cout<<"RpSumXYObject is NULL "<<endl;
    return;
  }

  if(verbosity>1){
    rpsum->identify();
  }

  RpSnglSumXY* obj = rpsum->getRpSumXY(RP::calcIdCode(RP::ID_BBC, 0, 0));
  if(verbosity>3){
    if(obj!=NULL){
      cout<<obj->Name()<<" "<<obj->IdCode()<<" ";
      cout<<obj->QVector(0)<<" "<<obj->QVector(1)<<" ";
      cout<<obj->Weight()<<endl;
    }
  }
 
}



//==============================================================

int SvxCntQA::End(PHCompositeNode *topNode) {
  cout << "SvxCntQA::End:  Writing out..." << endl;
  OutputNtupleFile->Write();
  cout << "SvxCntQA::End:  Closing output file..." << endl;
  OutputNtupleFile->Close();
  delete OutputNtupleFile;
  OutputNtupleFile=0;
  return 0;
}


// copy from SvxPixel1v1
// Return the local X position from the sensor Ix value
double
SvxCntQA::get_sensorXpos_pixel(const int layer, const int ladder, const int sens, 
                       const int section, const int readout, int ix) const
{
  SvxSensor *sensor = m_svxGeo->GetSensorPtr(layer, ladder, sens);

  double xPitch = sensor->get_xPitch(section, readout);
  double xhalfWidth = sensor->get_xhalfWidth(section, readout);
  double xpos = (ix+0.5)*xPitch - xhalfWidth;
  xpos += sensor->get_secXpos(section, readout);
  if ( sensor->get_xcntRvrs(section, readout) ) xpos *= -1.0;
  return xpos;
}

// copy from SvxPixel1v1
// Return the local Z position from the sensor Iz value
double
SvxCntQA::get_sensorZpos_pixel(const int layer, const int ladder, const int sens, 
                       const int section, const int readout, int iz) const
{
  SvxSensor *sensor = m_svxGeo->GetSensorPtr(layer, ladder, sens);

  // First adjust the Iz to its value within the section
  for (int isec=0; isec<section; isec++)
    {
      int npitch = sensor->get_nZpitch(isec, readout);
      iz -= npitch;
    }
  double pitch = sensor->get_zPitch(section, readout);
  double halfWidth = sensor->get_zhalfWidth(section, readout);
  double zpos = (iz+0.5)*pitch - halfWidth;
  zpos += sensor->get_secZpos(section, readout);
  if ( sensor->get_zcntRvrs(section, readout) ) zpos *= -1.0;
  return zpos;
}


// Return the ROC number using local Z positio
double SvxCntQA::get_ROC_pixel(const int layer, const int ladder, const int sens, const float localz) const
{
  float zlimit[5] = {-2.78, -1.4, 0.0, 1.4, 2.78}; // cm
  //SvxSensor *sensor = m_svxGeo->GetSensorPtr(layer, ladder, sens);

  int i=0;
  for(i=0; i<4; i++){
    if(zlimit[i]<=localz&&localz<zlimit[i+1]){
      break;
    }
  }
  
  int roc = (sens%2==0) ? 3-i : 7-i;


//  if(layer==0&&ladder==0){
//    cout<<"Roc "<<roc<<" "<<localz<<" : "<<sens<<endl;
//  }

  return roc;
}

void SvxCntQA::initEvtTree(){
  ntpevt = new TTree("ntpevt", "Event Info Tree");
  ntpevt->Branch("run",    &m_evt_run,     "run/I");
  ntpevt->Branch("event",  &m_evt_event,   "event/I");
  ntpevt->Branch("strig",  &m_evt_strig,   "strig/I");
  ntpevt->Branch("eseq",   &m_evt_eseq,    "eseq/I");
  ntpevt->Branch("xctr",   &m_evt_xctr,    "xctr/I");
  ntpevt->Branch("n0",     &m_evt_n0,      "n0/I");
  ntpevt->Branch("n1",     &m_evt_n1,      "n1/I");
  ntpevt->Branch("n2",     &m_evt_n2,      "n2/I");
  ntpevt->Branch("n3",     &m_evt_n3,      "n3/I");
  ntpevt->Branch("zvtx",   &m_evt_zvtx,    "zvtx/F");
  ntpevt->Branch("zbbc",   &m_evt_zbbc,    "zbbc/F");
  ntpevt->Branch("zzdc",   &m_evt_zzdc,    "zzdc/F");
  ntpevt->Branch("nseg",   &m_evt_nseg,    "nseg/I");
  ntpevt->Branch("ntrk",   &m_evt_ntrk,    "trk/I");
  ntpevt->Branch("bbcq",   &m_evt_bbcq,    "bbcq/F");
  ntpevt->Branch("zdcq",   &m_evt_zdcq,    "zdcq/F");
  ntpevt->Branch("xvtxs",  &m_evt_xvtxs,   "xvtxs/F");
  ntpevt->Branch("yvtxs",  &m_evt_yvtxs,   "yvtxs/F");
  ntpevt->Branch("zvtxs",  &m_evt_zvtxs,   "zvtxs/F");
  ntpevt->Branch("zvtxsw", &m_evt_zvtxsw,  "zvtxsw/F");
  ntpevt->Branch("zvtxse", &m_evt_zvtxse,  "zvtxse/F");
  ntpevt->Branch("xvtxp",  &m_evt_xvtxp,   "xvtxp/F");
  ntpevt->Branch("yvtxp",  &m_evt_yvtxp,   "yvtxp/F");
  ntpevt->Branch("zvtxp",  &m_evt_zvtxp,   "zvtxp/F");
  ntpevt->Branch("xvtxpw",  &m_evt_xvtxpw,   "xvtxpw/F");
  ntpevt->Branch("yvtxpw",  &m_evt_yvtxpw,   "yvtxpw/F");
  ntpevt->Branch("zvtxpw",  &m_evt_zvtxpw,   "zvtxpw/F");
  ntpevt->Branch("xvtxpe",  &m_evt_xvtxpe,   "xvtxpe/F");
  ntpevt->Branch("yvtxpe",  &m_evt_yvtxpe,   "yvtxpe/F");
  ntpevt->Branch("zvtxpe",  &m_evt_zvtxpe,   "zvtxpe/F");
}

void SvxCntQA::initCntTree(){

  m_c_datap[0]  = &m_c_ly0;    m_c_datap[14] = &m_c_ly1;    m_c_datap[28] = &m_c_ly2;    m_c_datap[42] = &m_c_ly3;
  m_c_datap[1]  = &m_c_ld0;    m_c_datap[15] = &m_c_ld1;    m_c_datap[29] = &m_c_ld2;    m_c_datap[43] = &m_c_ld3;
  m_c_datap[2]  = &m_c_zproj0; m_c_datap[16] = &m_c_zproj1; m_c_datap[30] = &m_c_zproj2; m_c_datap[44] = &m_c_zproj3;
  m_c_datap[3]  = &m_c_dproj0; m_c_datap[17] = &m_c_dproj1; m_c_datap[31] = &m_c_dproj2; m_c_datap[45] = &m_c_dproj3;
  m_c_datap[4]  = &m_c_bend0;  m_c_datap[18] = &m_c_bend1;  m_c_datap[32] = &m_c_bend2;  m_c_datap[46] = &m_c_bend3;
  m_c_datap[5]  = &m_c_zv0;    m_c_datap[19] = &m_c_zv1;    m_c_datap[33] = &m_c_zv2;    m_c_datap[47] = &m_c_zv3;
  m_c_datap[6]  = &m_c_ph0;    m_c_datap[20] = &m_c_ph1;    m_c_datap[34] = &m_c_ph2;    m_c_datap[48] = &m_c_ph3;
  m_c_datap[7]  = &m_c_r0;     m_c_datap[21] = &m_c_r1;     m_c_datap[35] = &m_c_r2;     m_c_datap[49] = &m_c_r3;
  m_c_datap[8]  = &m_c_sdp0;   m_c_datap[22] = &m_c_sdp1;   m_c_datap[36] = &m_c_sdp2;   m_c_datap[50] = &m_c_sdp3;   
  m_c_datap[9]  = &m_c_sdz0;   m_c_datap[23] = &m_c_sdz1;   m_c_datap[37] = &m_c_sdz2;   m_c_datap[51] = &m_c_sdz3;   
  m_c_datap[10] = &m_c_bdp0;   m_c_datap[24] = &m_c_bdp1;   m_c_datap[38] = &m_c_bdp2;   m_c_datap[52] = &m_c_bdp3;   
  m_c_datap[11] = &m_c_bdz0;   m_c_datap[25] = &m_c_bdz1;   m_c_datap[39] = &m_c_bdz2;   m_c_datap[53] = &m_c_bdz3;   
  m_c_datap[12] = &m_c_fitdp0; m_c_datap[26] = &m_c_fitdp1; m_c_datap[40] = &m_c_fitdp2; m_c_datap[54] = &m_c_fitdp3;   
  m_c_datap[13] = &m_c_fitdz0; m_c_datap[27] = &m_c_fitdz1; m_c_datap[41] = &m_c_fitdz2; m_c_datap[55] = &m_c_fitdz3;   
                                  
  m_ntp_cnttrk  = new TTree("ntp_cnttrk",  "CNTtrack with VTX info tree");
  m_ntp_cnttrk_bg  = new TTree("ntp_cnttrk_bg",  "CNTtrack with VTX info tree(BG)");

  TTree *t[2] = {m_ntp_cnttrk, m_ntp_cnttrk_bg};
  for(int i=0; i<2; i++){
    t[i]->Branch("bbcz",    &m_c_bbcz,     "bbcz/F");
    t[i]->Branch("bbcq",    &m_c_bbcq,     "bbcq/F");
    t[i]->Branch("t0",      &m_c_t0,       "t0/F");
    t[i]->Branch("xvtx",    &m_c_xvtx,     "xvtx/F");
    t[i]->Branch("yvtx",    &m_c_yvtx,     "yvtx/F");
    t[i]->Branch("zvtx",    &m_c_zvtx,     "zvtx/F");
    t[i]->Branch("zvtxp",   &m_c_zvtxp,    "zvtxp/F");
    t[i]->Branch("zvtxs",   &m_c_zvtxs,    "zvtxs/F");
    t[i]->Branch("mom",     &m_c_mom,      "mom/F");
    t[i]->Branch("the0",    &m_c_the0,     "the0/F");
    t[i]->Branch("phi0",    &m_c_phi0,     "phi0/F");
    t[i]->Branch("zed",     &m_c_zed,      "zed/F");
    t[i]->Branch("c",       &m_c_c,        "c/F");
    t[i]->Branch("dcqual",  &m_c_dcqual,   "dcqual/I");
    t[i]->Branch("emcdphi", &m_c_emcdphi,  "emcdphi/F");
    t[i]->Branch("emcdz",   &m_c_emcdz,    "emcdz/F");
    t[i]->Branch("pc3dphi", &m_c_pc3dphi,  "pc3dphi/F");
    t[i]->Branch("pc3dz",   &m_c_pc3dz,    "pc3dz/F");
    t[i]->Branch("pc2dphi", &m_c_pc2dphi,  "pc2dphi/F");
    t[i]->Branch("pc2dz",   &m_c_pc2dz,    "pc2dz/F");
    t[i]->Branch("n0",      &m_c_n0,    "n0/F");
    t[i]->Branch("cch2",    &m_c_cch2,  "cch2/F");
    t[i]->Branch("npe0",    &m_c_npe0,  "npe0/F");
    t[i]->Branch("disp",    &m_c_disp,  "disp/F");
    t[i]->Branch("sn0",     &m_c_sn0,   "sn0/F");
    t[i]->Branch("scch2",   &m_c_scch2, "scch2/F");
    t[i]->Branch("snpe0",   &m_c_snpe0, "snpe0/F");
    t[i]->Branch("sdisp",   &m_c_sdisp, "sdisp/F");
    t[i]->Branch("ecore",   &m_c_ecore,  "ecore/F");
//    t[i]->Branch("ecorr",   &m_c_ecorr,  "ecorr/F");


    t[i]->Branch("ly3",     &m_c_ly3,      "ly3/F");
    t[i]->Branch("ld3",     &m_c_ld3,      "ld3/F");
    t[i]->Branch("zproj3",  &m_c_zproj3,   "zproj3/F");
    t[i]->Branch("dproj3",  &m_c_dproj3,   "dproj3/F");
    t[i]->Branch("bend3",   &m_c_bend3,    "bend3/F");
    t[i]->Branch("zv3",     &m_c_zv3,      "zv3/F");
    t[i]->Branch("ph3",     &m_c_ph3,      "ph3/F");
    t[i]->Branch("r3",      &m_c_r3,       "r3/F");
//    t[i]->Branch("sdp3",    &m_c_sdp3,     "sdp3/F");
//    t[i]->Branch("sdz3",    &m_c_sdz3,     "sdz3/F");
//    t[i]->Branch("bdp3",    &m_c_bdp3,     "bdp3/F");
//    t[i]->Branch("bdz3",    &m_c_bdz3,     "bdz3/F");
    t[i]->Branch("fitdp3",  &m_c_fitdp3,   "fitdp3/F");
    t[i]->Branch("fitdz3",  &m_c_fitdz3,   "fitdz3/F");
    t[i]->Branch("ly2",     &m_c_ly2,      "ly2/F");
    t[i]->Branch("ld2",     &m_c_ld2,      "ld2/F");
    t[i]->Branch("zproj2",  &m_c_zproj2,   "zproj2/F");
    t[i]->Branch("dproj2",  &m_c_dproj2,   "dproj2/F");
    t[i]->Branch("bend2",   &m_c_bend2,    "bend2/F");
    t[i]->Branch("zv2",     &m_c_zv2,      "zv2/F");
    t[i]->Branch("ph2",     &m_c_ph2,      "ph2/F");
    t[i]->Branch("r2",      &m_c_r2,       "r2/F");
//    t[i]->Branch("sdp2",    &m_c_sdp2,     "sdp2/F");
//    t[i]->Branch("sdz2",    &m_c_sdz2,     "sdz2/F");
//    t[i]->Branch("bdp2",    &m_c_bdp2,     "bdp2/F");
//    t[i]->Branch("bdz2",    &m_c_bdz2,     "bdz2/F");
    t[i]->Branch("fitdp2",  &m_c_fitdp2,   "fitdp2/F");
    t[i]->Branch("fitdz2",  &m_c_fitdz2,   "fitdz2/F");
    t[i]->Branch("ly1",     &m_c_ly1,      "ly1/F");
    t[i]->Branch("ld1",     &m_c_ld1,      "ld1/F");
    t[i]->Branch("zproj1",  &m_c_zproj1,   "zproj1/F");
    t[i]->Branch("dproj1",  &m_c_dproj1,   "dproj1/F");
    t[i]->Branch("bend1",   &m_c_bend1,    "bend1/F");
    t[i]->Branch("zv1",     &m_c_zv1,      "zv1/F");
    t[i]->Branch("ph1",     &m_c_ph1,      "ph1/F");
    t[i]->Branch("r1",      &m_c_r1,       "r1/F");
//    t[i]->Branch("sdp1",    &m_c_sdp1,     "sdp1/F");
//    t[i]->Branch("sdz1",    &m_c_sdz1,     "sdz1/F");
//    t[i]->Branch("bdp1",    &m_c_bdp1,     "bdp1/F");
//    t[i]->Branch("bdz1",    &m_c_bdz1,     "bdz1/F");
    t[i]->Branch("fitdp1",  &m_c_fitdp1,   "fitdp1/F");
    t[i]->Branch("fitdz1",  &m_c_fitdz1,   "fitdz1/F");
    t[i]->Branch("ly0",     &m_c_ly0,      "ly0/F");
    t[i]->Branch("ld0",     &m_c_ld0,      "ld0/F");
    t[i]->Branch("zproj0",  &m_c_zproj0,   "zproj0/F");
    t[i]->Branch("dproj0",  &m_c_dproj0,   "dproj0/F");
    t[i]->Branch("bend0",   &m_c_bend0,    "bend0/F");
    t[i]->Branch("zv0",     &m_c_zv0,      "zv0/F");
    t[i]->Branch("ph0",     &m_c_ph0,      "ph0/F");
    t[i]->Branch("r0",      &m_c_r0,       "r0/F");
//    t[i]->Branch("sdp0",    &m_c_sdp0,     "sdp0/F");
//    t[i]->Branch("sdz0",    &m_c_sdz0,     "sdz0/F");
//    t[i]->Branch("bdp0",    &m_c_bdp0,     "bdp0/F");
//    t[i]->Branch("bdz0",    &m_c_bdz0,     "bdz0/F");
    t[i]->Branch("fitdp0",  &m_c_fitdp0,   "fitdp0/F");
    t[i]->Branch("fitdz0",  &m_c_fitdz0,   "fitdz0/F");
    t[i]->Branch("chi2",    &m_c_chi2,     "chi2/F");
    t[i]->Branch("ndf",     &m_c_ndf,      "ndf/I");
    t[i]->Branch("nhit",    &m_c_nhit,     "nhit/I");
    t[i]->Branch("chi22",   &m_c_chi22,    "chi22/F");
    t[i]->Branch("unique",  &m_c_unique,   "unique/I");
    t[i]->Branch("dpchi2",  &m_c_dpchi2,   "dpchi2/F");
    t[i]->Branch("dzchi2",  &m_c_dzchi2,   "dzchi2/F");
    t[i]->Branch("dpndf",   &m_c_dpndf,    "dpndf/I");
    t[i]->Branch("dzndf",   &m_c_dzndf,    "dzndf/I");
    t[i]->Branch("dpchi2p", &m_c_dpchi2p,  "dpchi2p/F");
    t[i]->Branch("dzchi2p", &m_c_dzchi2p,  "dpchi2z/F");
//    t[i]->Branch("sdpchi2", &m_c_sdpchi2,  "sdpchi2/F");
//    t[i]->Branch("sdzchi2", &m_c_sdzchi2,  "sdzchi2/F");
//    t[i]->Branch("ssdpchi2", &m_c_ssdpchi2,  "ssdpchi2/F");
//    t[i]->Branch("ssdzchi2", &m_c_ssdzchi2,  "ssdzchi2/F");
//    t[i]->Branch("dfsdp",   m_c_dfsdp,     "dfsdp[6]/F");
//    t[i]->Branch("dfdpp",   m_c_dfdpp,     "dfdpp[6]/F");
//    t[i]->Branch("dfdzz",   m_c_dfdzz,     "dfdzz[6]/F");
    t[i]->Branch("d2dca",   &m_c_d2dca,    "d2dca/F");
    t[i]->Branch("d2dca0",  &m_c_d2dca0,   "d2dca0/F");
//    t[i]->Branch("d2dca1",  &m_c_d2dca1,   "d2dca1/F");
//    t[i]->Branch("R",       &m_c_R,        "R/F");
//    t[i]->Branch("L",       &m_c_L,        "L/F");
    t[i]->Branch("d2dcab",  &m_c_d2dcab,   "d2dcab/F");
//    t[i]->Branch("d2dca2",  &m_c_d2dca2,   "d2dca2/F");
//    t[i]->Branch("bd2dca",  &m_c_bd2dca,   "bd2dca/F");
    t[i]->Branch("zdca",    &m_c_zdca,     "zdca/F");
    t[i]->Branch("dedx1",   &m_c_dedx1,    "dedx1/F");
    t[i]->Branch("dedx2",   &m_c_dedx2,    "dedx2/F");
    t[i]->Branch("exn3",    &m_c_exn3,     "exn3/I");
    t[i]->Branch("exdz3",   m_c_exdz3,     "exdz3[exn3]/F");
    t[i]->Branch("exdp3",   m_c_exdp3,     "exdp3[exn3]/F");
    t[i]->Branch("exn2",    &m_c_exn2,     "exn2/I");
    t[i]->Branch("exdz2",   m_c_exdz2,     "exdz2[exn2]/F");
    t[i]->Branch("exdp2",   m_c_exdp2,     "exdp2[exn2]/F");
    t[i]->Branch("exn1",    &m_c_exn1,     "exn1/I");
    t[i]->Branch("exdz1",   m_c_exdz1,     "exdz1[exn1]/F");
    t[i]->Branch("exdp1",   m_c_exdp1,     "exdp1[exn1]/F");
    t[i]->Branch("exn0",    &m_c_exn0,     "exn0/I");
    t[i]->Branch("exdz0",   m_c_exdz0,     "exdz0[exn0]/F");
    t[i]->Branch("exdp0",   m_c_exdp0,     "exdp0[exn0]/F");

    // forsim
    if(m_simmode){
      t[i]->Branch("simdca",  &m_c_simdca,  "simdca/F");
      t[i]->Branch("simpt",   &m_c_simpt,   "simpt/F");
      t[i]->Branch("simphi0", &m_c_simphi0, "simphi0/F");
      t[i]->Branch("simthe0", &m_c_simthe0, "simthe0/F");
      t[i]->Branch("simvx",   &m_c_simvx,   "simvx/F");
      t[i]->Branch("simvy",   &m_c_simvy,   "simvy/F");
      t[i]->Branch("simvz",   &m_c_simvz,   "simvz/F");
      t[i]->Branch("simpid",  &m_c_simpid,  "simpid/I");
      t[i]->Branch("simpidpa",  &m_c_simpidpa,  "simpidpa/I");
      t[i]->Branch("simpidpr",  &m_c_simpidpr,  "simpidpr/I");
      t[i]->Branch("simdcapa",  &m_c_simdcapa,  "simdcapa/F");
      t[i]->Branch("simptpa",   &m_c_simptpa,   "simptpa/F");
      t[i]->Branch("simphi0pa", &m_c_simphi0pa, "simphi0pa/F");
      t[i]->Branch("simthe0pa", &m_c_simthe0pa, "simthe0pa/F");
      t[i]->Branch("simvxpa",   &m_c_simvxpa,   "simvxpa/F");
      t[i]->Branch("simvypa",   &m_c_simvypa,   "simvypa/F");
      t[i]->Branch("simvzpa",   &m_c_simvzpa,   "simvzpa/F");
      t[i]->Branch("simdcapr",  &m_c_simdcapr,  "simdcapr/F");
      t[i]->Branch("simptpr",   &m_c_simptpr,   "simptpr/F");
      t[i]->Branch("simphi0pr", &m_c_simphi0pr, "simphi0pr/F");
      t[i]->Branch("simthe0pr", &m_c_simthe0pr, "simthe0pr/F");
      t[i]->Branch("simvxpr",   &m_c_simvxpr,   "simvxpr/F");
      t[i]->Branch("simvypr",   &m_c_simvypr,   "simvypr/F");
      t[i]->Branch("simvzpr",   &m_c_simvzpr,   "simvzpr/F");
    }
  }
}
