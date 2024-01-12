#include "SvxAlignment_QA.h"
#include "SvxQADefs.h"

#include <Bbc.hh>
#include <BbcOut.h>
#include <ErtOut.h>
#include <EventHeader.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <Fun4AllHistoManager.h>
#include <PHCentralTrack.h>
#include <PHCompositeNode.h>
#include <PHGlobal.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHPoint.h>
#include <PHTrackOut.h>
#include <PHTypedNodeIterator.h>
#include <RunHeader.h>
#include <TriggerHelper.h>
#include <TrigRunLvl1.h>
#include <VtxOut.h>

#include <phool.h>
#include <getClass.h>
#include <recoConsts.h>
#include <gsl/gsl_math.h>


#include <SvxPriVertexSeedFinder.h>
#include <SvxPrimVertexFinder.h>
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <SvxSegmentList.h>
#include <SvxSegment.h>
#include <SvxBeamCenterPar.h>

#include "TFile.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TF1.h"
#include "TGraphErrors.h"
#include "TCanvas.h"
#include "TTree.h"
#include "TROOT.h"
#include "TStyle.h"
#include "TProfile.h"
#include "TMath.h"
#include "TSystem.h"

#include <iostream>
#include <iomanip>
#include <map>
#include <string>
#include <algorithm>
#include <vector>
#include <cstdlib>
#include <cmath>

using namespace std;
using namespace findNode;

//--------------------------------------------------------------------------------

SvxAlignment_QA::SvxAlignment_QA(string filename):
  bbc(NULL),
  svxcluslist(NULL),
  svxtracks(NULL),
  trk(NULL),
  vtxout(NULL),
  xbeamcenter_west(0), ybeamcenter_west(0),
  xbeamcenter_east(0), ybeamcenter_east(0),
  bbcqcut_up(500),
  bbcqcut_low(0),
  bbczcut(12),//12 cm bbcz cut
  m_readbeamcenterFromDB(true),
  runnumber(0)
{
  ThisName = "SvxAlignment_QA";
//  outfilename = filename;

  init_variables();

  // histo as NULL
  for(int ilay=0; ilay<N_LAYER; ilay++){
    for(int iladder=0; iladder<N_LADDER; iladder++){
      for(int iew=0; iew<N_EW; iew++){
        h1_dz_vtx_ladder_EW[ilay][iladder][iew] = NULL;//Fill(dz_vtx);
        h1_dphi_ladder_EW[ilay][iladder][iew]    = NULL;//Fill(dphi);
      }

      h2_dphi_vs_z_ladder[ilay][iladder]       = NULL;//Fill(svxz,dphi);
      h2_dphi_vs_phiv_ladder[ilay][iladder]    = NULL;//Fill(gc_phiv,dphi);
      h2_dz_zvtx_vs_dphi_ladder[ilay][iladder] = NULL;//

      h2_dca0_vs_phi_ladder[ilay][iladder]     = NULL;//beam center,internal
      h2_dca0p_vs_phi_ladder[ilay][iladder]    = NULL;//p.v. ,internal
      h2_dproj_vs_phi_ladder[ilay][iladder]    = NULL;//
    }
  }

  for(int iew=0; iew<N_EW; iew++){
    h1_dca0_EW[iew]         = NULL;//beam center,internal
    h1_dca0p_EW[iew]        = NULL;//p.v. ,internal
    h2_dca0_vs_phi_EW[iew]  = NULL;//beam center,internal
    h2_dca0p_vs_phi_EW[iew] = NULL;//p.v. ,internal
    h1_dcaz_EW[iew]         = NULL;
    h2_dcaz_vs_phi_EW[iew] = NULL;
  }

}

//--------------------------------------------------------------------------------

SvxAlignment_QA::~SvxAlignment_QA() {
//  if(hstOutFile!=NULL) { delete hstOutFile; hstOutFile=NULL; }
}


int SvxAlignment_QA::Init(PHCompositeNode *topNode) {   //callded whenever datafrom a new run is encountered
  if(verbosity>0) cout << "Init:SvxAlignment QA" << endl;

  init_histo();

  return 0;
}

//--------------------------------------------------------------------------------
int SvxAlignment_QA::InitRun(PHCompositeNode *topNode) //callded once at startup
{
  if(verbosity){
    cout << "SvxAlignment:: BBC charge Range: from 10 to " << bbcqcut_up << endl;
    cout << "SvxAlignment:: BBCZ Range: from -" << bbczcut << " to " << bbczcut  << endl;
  }
  init_nodes(topNode);
  
  //check magnet current
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader==NULL) { cerr << PHWHERE<< "Can't find runheader. " << endl; return ABORTRUN; }
  if(verbosity>0) cout<<"SvxAlignment_QA::InitRun RunHeader node found." << endl;
  if(runheader->get_currentCentral()>0){
    m_fieldScale = 1.0;
  } else {
    m_fieldScale = -1.0;
  }
  if(verbosity>0) cout<<"SvxAlignment_QA::InitRun  fieldScale = "<< m_fieldScale << endl; 

  runnumber = runheader->get_RunNumber();  

  if( 358512  <runnumber && runnumber < 363229 ){
   //run12 pp 200GeV setting
    if(verbosity>0) cout<<"SvxAlignment_QA::InitRun  cut parameters for run12 pp are set." << endl; 
    pc3dphi_emcdphioffset = 0.00293;
    pc3dphi_emcdphicut = 0.02 ;
    pc3dz_emcdzoffset = 0.521 ;
    pc3dz_emcdzcut = 10.0;
    dproj_cut = 1.0;
    zproj_cut = 1000.0;
    bbcqcut_low = 0;
  }else{
  //run11 AuAu setting
    if(verbosity>0) cout<<"SvxAlignment_QA::InitRun  cut parameters for run11 AuAu are set." << endl; 
    pc3dphi_emcdphioffset= -0.0003;
    pc3dphi_emcdphicut= 0.02 ;
    pc3dz_emcdzoffset=0.2 ;
    pc3dz_emcdzcut= 5.0;
    dproj_cut = 0.4;
    zproj_cut = 5.0;
    bbcqcut_low = 10.0;
  }

  if(verbosity>0) cout<<"SvxAlignment_QA::InitRun finished. " << endl;
  return EVENT_OK;
}

int SvxAlignment_QA::process_event(PHCompositeNode *topNode) //callded for every event
{
  init_variables();

  if(bbc==NULL||svxcluslist==NULL||svxtracks==NULL||
     trk==NULL||vtxout==NULL||beamcenter==NULL)
  {
    cerr<<"SvxAlignment_QA::process_event skip this event"<<endl;
    cerr<<"   SvxAlignment_QA::   "<<endl;
    if(bbc==NULL)         cerr<<"No BbcOut"<<flush;
    if(svxcluslist==NULL) cerr<<"No SvxClusterList"<<flush;
    if(svxtracks==NULL)   cerr<<"No SvxSegmentList"<<flush;
    if(trk==NULL)         cerr<<"No PHCentralTrack"<<flush;
    if(vtxout==NULL)      cerr<<"No VtxOut"<<flush;
    if(beamcenter==NULL)  cerr<<"No BeamCenter"<<flush;
    cerr<<endl;

    return EVENT_OK;
  }

  bbcz = bbc->get_VertexPoint();
  bbcq = bbc->get_ChargeSum(Bbc::North) + bbc->get_ChargeSum(Bbc::South);
  if(fabs(bbcz) > bbczcut)return EVENT_OK; 
  if(bbcq       < bbcqcut_low) return EVENT_OK;
  if(bbcqcut_up < bbcq) return EVENT_OK;

  fill_histo();

  return EVENT_OK;
}

//-------------------------------------------------------------------------------

int SvxAlignment_QA::End(PHCompositeNode *topNode) // Last call before you quit
{ 

  return EVENT_OK;
}

bool SvxAlignment_QA::init_histo(){
  

  Fun4AllServer *se = Fun4AllServer::instance();
  Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
  if (!hm)
  {
    hm = new Fun4AllHistoManager(HistoManagerName);
    se->registerHistoManager(hm);
  }
  static const int nlayers = N_LAYER;
 // const int nladders[nlayers] = {10,20,16,24};
  static const int nladders = N_LADDER;
  static const int n_EW = N_EW;// 0 : west 1: east
  static const float phi_sigma[nlayers]={0.02,0.03,0.07,0.09};
  static const float z_sigma[nlayers]={0.05,0.05,0.09,0.12};

  //define histos
  
  for(int ilr=0;ilr<nlayers;ilr++){
    for(int ild=0;ild<nladders;ild++){
      for(int iew=0;iew<n_EW;iew++){
        h1_dz_vtx_ladder_EW[ilr][ild][iew] = new TH1F(Form("h1_dz_vtx_ladder_EW_%d_%d_%d",ilr,ild,iew),Form("dz with zvtx at layer%d and ladder%d and EW%d",ilr,ild,iew),200,-1.0,1.0);
        hm->registerHisto(h1_dz_vtx_ladder_EW[ilr][ild][iew]);
        h1_dphi_ladder_EW[ilr][ild][iew] = new TH1F(Form("h1_dphi_ladder_EW_%d_%d_%d",ilr,ild,iew),Form("d#phi at layer%d and ladder%d and EW%d",ilr,ild,iew),400,-0.4,0.4);
        hm->registerHisto(h1_dphi_ladder_EW[ilr][ild][iew]);
      }
    }
  }

  for(int ilr=0;ilr<nlayers;ilr++){
    for(int ild=0;ild<nladders;ild++){
      h2_dphi_vs_z_ladder[ilr][ild] = new TH2F(Form("h2_dphi_vs_z_ladder_%d_%d",ilr,ild),Form("d#phi vs. z at layer%d at ladder%d",ilr,ild),160,-16,16,100,-2.0*z_sigma[ilr],2.0*z_sigma[ilr]);
      hm->registerHisto(h2_dphi_vs_z_ladder[ilr][ild]);
      h2_dphi_vs_phiv_ladder[ilr][ild] = new TH2F(Form("h2_dphi_vs_phiv_ladder_%d_%d",ilr,ild),Form("d#phi vs. vtx #phi at layer%d at ladder%d",ilr,ild),3000,-3.2,4.2,100,-2.0*phi_sigma[ilr],2.0*phi_sigma[ilr]);
      hm->registerHisto(h2_dphi_vs_phiv_ladder[ilr][ild]);
      h2_dz_zvtx_vs_dphi_ladder[ilr][ild] = new TH2F(Form("h2_dz_zvtx_vs_dphi_ladder_%d_%d",ilr,ild),Form("dz vs.d#phi with zvtx at layer%d and ladder%d ",ilr,ild),50,-0.4,0.4,50,-2.0,2.0);
      hm->registerHisto(h2_dz_zvtx_vs_dphi_ladder[ilr][ild]);
    } 
  }

  //internal alignment

  h1_dca0_EW[0] = new TH1F("h1_dca0_WEST","DCA to beamcenter WEST",600,-0.3,0.3);
  h1_dca0_EW[0]->SetXTitle("dca0+bend(cm)");
  hm->registerHisto(h1_dca0_EW[0]);
  h1_dca0p_EW[0] = new TH1F("h1_dca0p_WEST","DCA to SVX_PRECISE WEST",600,-0.3,0.3);
  hm->registerHisto(h1_dca0p_EW[0]);
  h1_dcaz_EW[0] = new TH1F("h1_dcaz_WEST","DCAz WEST",600,-0.3,0.3);
  hm->registerHisto(h1_dcaz_EW[0]);
  h1_dca0_EW[1] = new TH1F("h1_dca0_EAST","DCA to beamcenter EAST",600,-0.3,0.3);
  hm->registerHisto(h1_dca0_EW[1]);
  h1_dca0p_EW[1] = new TH1F("h1_dca0p_EAST","DCA to SVX_PRECISE EAST",600,-0.3,0.3);
  hm->registerHisto(h1_dca0p_EW[1]);
  h1_dcaz_EW[1] = new TH1F("h1_dcaz_EAST","DCAz EAST",600,-0.3,0.3);
  hm->registerHisto(h1_dcaz_EW[1]);
  h2_dca0_vs_phi_EW[0] = new TH2F("h2_dca0_vs_bphi_WEST","DCA to beamcenter vs phi WEST",100,-1.5,1.5,100,-0.04,0.04);
  hm->registerHisto(h2_dca0_vs_phi_EW[0]);
  h2_dca0p_vs_phi_EW[0] = new TH2F("h2_dca0p_vs_bphi_WEST","DCA to SVX_PRECISE vs phi WEST",100,-1.5,1.5,100,-0.04,0.04);
  hm->registerHisto(h2_dca0p_vs_phi_EW[0]);
  h2_dcaz_vs_phi_EW[0] = new TH2F("h2_dcaz_vs_bphi_WEST","DCAz vs phi WEST",100,-1.5,1.5,100,-0.08,0.08);
  hm->registerHisto(h2_dcaz_vs_phi_EW[0]);
  h2_dca0_vs_phi_EW[1] = new TH2F("h2_dca0_vs_bphi_EAST","DCA to beamcenter vs phi EAST",100,-1.5,1.5,100,-0.04,0.04);
  hm->registerHisto(h2_dca0_vs_phi_EW[1]);
  h2_dca0p_vs_phi_EW[1] = new TH2F("h2_dca0p_vs_bphi_EAST","DCA to SVX_PRECISE vs phi EAST",100,-1.5,1.5,100,-0.04,0.04);
  hm->registerHisto(h2_dca0p_vs_phi_EW[1]);
  h2_dcaz_vs_phi_EW[1] = new TH2F("h2_dcaz_vs_bphi_EAST","DCAz vs phi EAST",100,-1.5,1.5,100,-0.08,0.08);
  hm->registerHisto(h2_dcaz_vs_phi_EW[1]);
  
  for(int ilr=0; ilr<nlayers; ilr++){
    for(int ild=0; ild<nladders; ild++){
      h1_dca0_ladder[ilr][ild] = new TH1F(Form("h1_dca0_ladder_%d_%d",ilr,ild),Form("dca0+bend at layer%d and ladder%d ",ilr,ild),1000,-0.3,0.3);
      hm->registerHisto(h1_dca0_ladder[ilr][ild]);
      h1_dca0p_ladder[ilr][ild] = new TH1F(Form("h1_dca0p_ladder_%d_%d",ilr,ild),Form("dca0p+bend at layer%d and ladder%d ",ilr,ild),1000,-0.3,0.3);
      hm->registerHisto(h1_dca0p_ladder[ilr][ild]);
      h1_dproj_ladder[ilr][ild] = new TH1F(Form("h1_dproj_ladder_%d_%d",ilr,ild),Form("dproj at layer%d and ladder%d  ",ilr,ild),1000,-1.0,1.0);
      hm->registerHisto(h1_dproj_ladder[ilr][ild]);
      h2_dca0_vs_phi_ladder[ilr][ild] = new TH2F(Form("h2_dca0_vs_phi_ladder_%d_%d",ilr,ild),Form("dca0 vs.phi layer%d and ladder%d  ",ilr,ild),300,-1.5,1.5,600,-0.3,0.3);
      hm->registerHisto(h2_dca0_vs_phi_ladder[ilr][ild]);
      h2_dca0p_vs_phi_ladder[ilr][ild] = new TH2F(Form("h2_dca0p_vs_phi_ladder_%d_%d",ilr,ild),Form("dca0p vs.phi layer%d and ladder%d  ",ilr,ild),300,-1.5,1.5,600,-0.3,0.3);
      hm->registerHisto(h2_dca0p_vs_phi_ladder[ilr][ild]);
      h2_dproj_vs_phi_ladder[ilr][ild] = new TH2F(Form("h2_dproj_vs_phi_ladder_%d_%d",ilr,ild),Form("dproj vs.dphi at layer%d and ladder%d ",ilr,ild),300,-1.5,1.5,600,-0.3,0.3);
      hm->registerHisto(h2_dproj_vs_phi_ladder[ilr][ild]);
    } 
  }

  if(verbosity>0)  cout << "init histos" << endl;
  return true;
}


bool SvxAlignment_QA::init_nodes(PHCompositeNode *topNode){

  bbc = getClass<BbcOut>(topNode,"BbcOut"); 
  if(!bbc) {cerr <<  PHWHERE  << " ERROR: Can't find BbcOut " << endl; return false;} 

  svxcluslist = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  if(svxcluslist==NULL) {cerr << PHWHERE << " ERROR: Can't find SvxClusterList." << endl;  return false;}

  svxtracks = findNode::getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  if(svxtracks==NULL) {cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;  return false;}

  trk = getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(!trk) {cerr <<  PHWHERE  << " ERROR: Can't find PHCentralTrack "<< endl; return false;} 

  vtxout = getClass<VtxOut>(topNode,"VtxOut");
  if(vtxout==NULL) {cerr <<  PHWHERE  << " ERROR: Can't find VtxOut "<< endl; return false;} 
 
  beamcenter = findNode::getClass<SvxBeamCenterPar>(topNode, "SvxBeamCenterPar");
  if(beamcenter){
    if(m_readbeamcenterFromDB){
      xbeamcenter_west = beamcenter->getBeamCenter(0);
      ybeamcenter_west = beamcenter->getBeamCenter(1);
      xbeamcenter_east = beamcenter->getBeamCenter(0);
      ybeamcenter_east = beamcenter->getBeamCenter(1);
      if(verbosity>0)  cout << "SvxAlignment_QA  :: Get beam center " << endl;  
    }
  }else{
     cerr << PHWHERE<< "Can't find BeamCenter object. " << endl; 
     return false;
  } 

  return true;
}

void SvxAlignment_QA::init_variables()
{

  charge=0;
  mom=pt=-1;
  for(int i=0;i<3;i++) p[i]=0;
  for(int i=0;i<4;i++) nhit[i]=0;
  bbcz=-9999;
  bbcq=0;
  for(int i=0;i<4;i++) phi[i]=-9999; 
  for(int i=0;i<4;i++) phib[i]=-9999;
  bphi=-9999;
  bphib=-9999;
  dca0=-9999; 
  dca0p=-9999; 
  dcaz=-9999; 
  dproj2=-9999; 
  dproj3=-9999;
  mag_bend2=-9999;
  mag_bend3=-9999;
  nsvxtracks=0;

  magnetic_bend=-9999;
  magnetic_bendp=-9999;
  for(int i=0;i<4;i++) ladder[i]=-1; 
  for(int i=0;i<4;i++) sensor[i]=-1;

  gl_ntrk=0;
  gl_mom=-9999;
  gl_phi0=-9999;
  gl_the0=-9999;
  gl_pt=-9999;
  gl_charge=0;
  gl_emcdz=-9999;
  gl_emcdphi=-9999;
  gl_pc3dz=-9999;
  gl_pc3dphi=-9999;
  gl_zed=-9999;
  gl_itrack=-9999;
  gl_trk_quality=-9999;
  gl_n0=-9999;
  gl_ecore=-9999;

  gc_ilayer=-1; 
  gc_iladder=-1;
  gc_zproj=-9999;
  gc_dproj=-9999;
  gc_mag_bend=-9999;
  gc_svxx=-9999;
  gc_svxy=-9999;
  gc_svxz=-9999;
  gc_phiv=-9999;
  gc_zvtx=-9999;

  for(int i=0;i<3;i++) svxprim[i]=-9999;//data from vtxout node
  
  if(verbosity>0)  cout << "init variables" << endl;
  return;
}


void SvxAlignment_QA::fill_histo()
{
  //
  // Segment (StandAloneTracking)
  //
  nsvxtracks = svxtracks->get_nSegments();
  for(int i=0; i<nsvxtracks; i++) {
    SvxSegment* seg0 = svxtracks->get_segment(i);
    if(seg0->IsPositive()) charge = 1; else charge = -1;
    // calculate projected vertex from L0 and L1 hit
    int pixel0id = seg0->getClusterID(0);//default val. is -1
    int pixel1id = seg0->getClusterID(1);
    if ((pixel0id <= 0) || (pixel1id <= 0)) {
      if (verbosity > 100) cout << PHWHERE << "could not find clusterID! " << endl;
      continue;
    }
    float x0 = svxcluslist->get_Cluster(pixel0id)->get_xyz_global(0);
    float y0 = svxcluslist->get_Cluster(pixel0id)->get_xyz_global(1);
    float z0 = svxcluslist->get_Cluster(pixel0id)->get_xyz_global(2);
    float r0 = sqrt(x0 * x0 + y0 * y0);

    float x1 = svxcluslist->get_Cluster(pixel1id)->get_xyz_global(0);
    float y1 = svxcluslist->get_Cluster(pixel1id)->get_xyz_global(1);
    float z1 = svxcluslist->get_Cluster(pixel1id)->get_xyz_global(2);
    float r1 = sqrt(x1 * x1 + y1 * y1);
    //float r1 = sqrt(x1*x1+y1*y1);
    ladder[0] = svxcluslist->get_Cluster(pixel0id)->get_ladder();
    ladder[1] = svxcluslist->get_Cluster(pixel1id)->get_ladder();
    sensor[0] = svxcluslist->get_Cluster(pixel0id)->get_sensor();     
    sensor[1] = svxcluslist->get_Cluster(pixel1id)->get_sensor(); 
    

    //layer 2 
    float x2 = -99999;
    float y2 = -99999;
    ladder[2]=-1;
    sensor[2]=-1;
    int strip0id = seg0->getClusterID(2);
    if(strip0id>=0){
      ladder[2] = svxcluslist->get_Cluster(strip0id)->get_ladder();
      sensor[2] = svxcluslist->get_Cluster(strip0id)->get_sensor();
      x2 = svxcluslist->get_Cluster(strip0id)->get_xyz_global(0);
      y2 = svxcluslist->get_Cluster(strip0id)->get_xyz_global(1);
    }


    //float r2 = sqrt(x2*x2+y2*y2);
     
    //layer 3
    float x3 = -99999;
    float y3 = -99999;
    ladder[3]=-1;
    sensor[3]=-1;
    int strip1id = seg0->getClusterID(3);
    if(strip1id>=0){
      ladder[3] = svxcluslist->get_Cluster(strip1id)->get_ladder();
      sensor[3] = svxcluslist->get_Cluster(strip1id)->get_sensor();
      x3 = svxcluslist->get_Cluster(strip1id)->get_xyz_global(0);
      y3 = svxcluslist->get_Cluster(strip1id)->get_xyz_global(1);
    }

    //float r3 = sqrt(x3*x3+y3*y3);
    
    float x0c, y0c /*,x1c, y1c*/;
    /*float x2c, y2c,*/ /*x3c, y3c*/;
    if(x0>0) {
      x0c = x0 - xbeamcenter_west;
      //x1c = x1 - xbeamcenter_west;
      y0c = y0 - ybeamcenter_west;
      //y1c = y1 - ybeamcenter_west;
      //x2c = x2 - xbeamcenter_west;
      //x3c = x3 - xbeamcenter_west;
      //y2c = y2 - ybeamcenter_west;
      //y3c = y3 - ybeamcenter_west;
    } else {
      x0c = x0 - xbeamcenter_east;
      //x1c = x1 - xbeamcenter_east;
      y0c = y0 - ybeamcenter_east;
      //y1c = y1 - ybeamcenter_east;
      //x2c = x2 - xbeamcenter_east;
      //x3c = x3 - xbeamcenter_east;
      //y2c = y2 - ybeamcenter_east;
      //y3c = y3 - ybeamcenter_east;
    }


    phi[0] = atan2(y0,x0);
    phi[1] = atan2(y1,x1);
    phi[2] = atan2(y2,x2);
    phi[3] = atan2(y3,x3);
    phib[0] = atan2(y0,-x0);
    phib[1] = atan2(y1,-x1);
    phib[2] = atan2(y2,-x2);
    phib[3] = atan2(y3,-x3);

    bphi = atan2((y1-y0),(x1-x0));
    bphib = atan2((y1-y0),-(x1-x0));

    float ux = x1 - x0;
    float uy = y1 - y0;
    float ul = sqrt(ux*ux + uy*uy);
    ux = ux/ul;
    uy = uy/ul;

    float u_dot_dx = ux*x0c + uy*y0c;
    float u_cross_dx = ux*y0c - uy*x0c;
    
    dca0 = sqrt(x0c*x0c + y0c*y0c - u_dot_dx*u_dot_dx);
    if(u_cross_dx > 0) { //u_cross_dx is opposite sign compared with external one
      dca0 = -dca0;
    }

    mom = seg0->getMomentum();
    p[0]  = seg0->get3Momentum(0);
    p[1]  = seg0->get3Momentum(1);
    p[2]  = seg0->get3Momentum(2);
    pt  = sqrt(p[0]*p[0]+p[1]*p[1]);
    nhit[0] = seg0->getNhits(0); 
    nhit[1] = seg0->getNhits(1); 
    nhit[2] = seg0->getNhits(2); 
    nhit[3] = seg0->getNhits(3); 
    
    if (vtxout->isVtx("SVX_PRECISE")) {
      PHPoint verpointsvxprim = vtxout->get_Vertex("SVX_PRECISE");
      float primary_t[3]={0,0,0};
      primary_t[0] = verpointsvxprim.getX();
      primary_t[1] = verpointsvxprim.getY();
      primary_t[2] = verpointsvxprim.getZ();
      if ((fabs(primary_t[0]) < 1.)
          && (fabs(primary_t[1]) < 1.)
          && (fabs(primary_t[2]) < 12.)) {
        //dca0
        float x0p = x0 - primary_t[0];
        float y0p = y0 - primary_t[1];
        float z0p = z0 - primary_t[2];
 //       float x1p = x1 - primary_t[0];
 //       float y1p = y1 - primary_t[1];
//        float z1p = z1 - primary_t[2];
        float u_dot_dxp = ux * x0p + uy * y0p;
        float u_cross_dxp = ux * y0p - uy * x0p;
        dca0p = sqrt(x0p * x0p + y0p * y0p - u_dot_dxp * u_dot_dxp);
        if (u_cross_dxp > 0) {
          dca0p = -dca0p;
        }

        float r0p = sqrt(x0p * x0p + y0p * y0p);
 //       float r1p = sqrt(x1p * x1p + y1p * y1p);
 ///       float vtxprojz = z0 - r0p * (z1 - z0) / (r1p - r0p);
        //       float uz = z1 - z0;//abs(z1-z0);
        float uz = z1 - z0;
        float ur = r1 - r0;
        float ulz = sqrt(uz * uz + ur * ur);
        uz = uz / ulz;
        ur = ur / ulz;
        float u_dot_dzp = uz * z0p + ur * r0p;
        float u_cross_dzp = uz * z0p - ur * r0p;
        dcaz = sqrt(z0p * z0p + r0p * r0p - u_dot_dzp * u_dot_dzp);
        if (u_cross_dzp > 0) {
          dcaz = -dcaz;
        }
      }
    }//if SVX_PRECISE 

    float L_square=-99999.9;
    if(x0>=0){
      L_square = (x0-xbeamcenter_west)*(x0-xbeamcenter_west)+ (y0-ybeamcenter_west)*(y0-ybeamcenter_west);
    }else{
      L_square = (x0-xbeamcenter_east)*(x0-xbeamcenter_east)+ (y0-ybeamcenter_east)*(y0-ybeamcenter_east);
    }
    const float bendconst = 0.0027 ;///0.3 * 0.9 Tesla /100cm /mom
    magnetic_bend = -1.0*m_fieldScale*bendconst*L_square*charge/pt;

    if (vtxout->isVtx("SVX_PRECISE")) {
      PHPoint verpointsvxprim = vtxout->get_Vertex("SVX_PRECISE");
      float primary_t[3];
      primary_t[0] = verpointsvxprim.getX();
      primary_t[1] = verpointsvxprim.getY();
      primary_t[2] = verpointsvxprim.getZ();
      L_square = (x0 - primary_t[0]) * (x0 - primary_t[0]) + (y0 - primary_t[1]) * (y0 - primary_t[1]);
      const float bendconst = 0.0027 ;///0.3 * 0.9 Tesla /100cm /pt
      magnetic_bendp = -1.0 * m_fieldScale * bendconst * L_square * charge / pt;
    }

    if(strip0id>0) {
      float    dx21 = x2 - x1;
      float    dy21 = y2 - y1;
      float    u_cross_dx2 = ux*dy21 - uy*dx21;
      // u_cross_dx2 is a vector product of (ux,uy) and (dx21,dy21).
      // its absolute value is the distance between the (L2x,L2y) and the
      // straight line projecton point of the track from (x0,y0) to (x1,y1).
      dproj2 = u_cross_dx2;
      //mag_bend2 = -1.0*m_fieldScale*1.2E-3*r2*(r2-r1)*charge/pt;
    }
    
    // projection to L3
    if(strip1id>0) {
      float   dx31 = x3 - x1;
      float   dy31 = y3 - y1;
      float   u_cross_dx3 = ux*dy31 - uy*dx31;
      // u_cross_dx3 is a vector product of (ux,uy) and (dx31,dy31).
      // its absolute value is the distance between the (L3x,L3y) and the
      // straight line projecton point of the track from (x0,y0) to (x1,y1).
      dproj3 = u_cross_dx3;
      //mag_bend3 = -1.0*m_fieldScale*1.2E-3*r3*(r3-r1)*charge/pt;
    }

    const float ptcut_min=0.7;
    if(pt>ptcut_min){
      if(fabs(bphi)<1.5){
        h1_dca0_EW[0]->Fill(dca0+magnetic_bend);
        h2_dca0_vs_phi_EW[0]->Fill(bphi,dca0+magnetic_bend);
      }else{
        h1_dca0_EW[1]->Fill(dca0+magnetic_bend);
        h2_dca0_vs_phi_EW[1]->Fill(bphib,dca0+magnetic_bend);
      }
      if(vtxout->isVtx("SVX_PRECISE")){
        if(fabs(bphi)<1.5){
          h1_dca0p_EW[0]->Fill(dca0p+magnetic_bendp);
          h2_dca0p_vs_phi_EW[0]->Fill(bphi,dca0p+magnetic_bendp);
        }else{
          h1_dca0p_EW[1]->Fill(dca0p+magnetic_bendp);
          h2_dca0p_vs_phi_EW[1]->Fill(bphib,dca0p+magnetic_bendp);
        }
      }
      for(int ilr=0;ilr<N_LAYER;ilr++){
        if(fabs(bphi)<1.5){
          if(ilr<2 ){
            h1_dca0_ladder[ilr][ladder[ilr]]->Fill(dca0+magnetic_bend);
            h2_dca0_vs_phi_ladder[ilr][ladder[ilr]]->Fill(bphi,dca0+magnetic_bend);
          }else if(ilr==2 && ladder[ilr]>=0){
            h1_dproj_ladder[ilr][ladder[ilr]]->Fill(dproj2);
            h2_dproj_vs_phi_ladder[ilr][ladder[ilr]]->Fill(bphi,dproj2);
          }else if(ilr==3 && ladder[ilr]>=0){
            h1_dproj_ladder[ilr][ladder[ilr]]->Fill(dproj3);
            h2_dproj_vs_phi_ladder[ilr][ladder[ilr]]->Fill(bphi,dproj3);
          }
        }else{
          if(ilr<2){
            h1_dca0_ladder[ilr][ladder[ilr]]->Fill(dca0+magnetic_bend);
            h2_dca0_vs_phi_ladder[ilr][ladder[ilr]]->Fill(bphib,dca0+magnetic_bend);
          }else if(ilr==2 && ladder[ilr]>=0){
            h1_dproj_ladder[ilr][ladder[ilr]]->Fill(dproj2);
            h2_dproj_vs_phi_ladder[ilr][ladder[ilr]]->Fill(bphib,dproj2);
          }else if(ilr==3 && ladder[ilr]>=0){
            h1_dproj_ladder[ilr][ladder[ilr]]->Fill(dproj3);
            h2_dproj_vs_phi_ladder[ilr][ladder[ilr]]->Fill(bphib,dproj3);
          } 
        }
      }
    }//pt cut

  }//for nsvxstrack


//////////////////////////////////////////
//central info.
/////////////////////////////////////////
  (trk!=NULL) ? gl_ntrk = trk->get_npart():0;
  
  PHPoint verpoint = vtxout->get_Vertex();
  gc_vtxout[0] = verpoint.getX();
  gc_vtxout[1] = verpoint.getY();
  gc_vtxout[2] = verpoint.getZ();

  PHPoint verpointsvxprim = vtxout->get_Vertex("SVX_PRECISE");
  svxprim[0] = verpointsvxprim.getX();
  svxprim[1] = verpointsvxprim.getY();
  svxprim[2] = verpointsvxprim.getZ();
  
  int nsvxcluslist = svxcluslist->get_nClusters();
  //always require primary vertex
  if (!vtxout->isVtx("SVX_PRECISE")) return ;
  //---------------PHCentral Track-----------------------------
  if(trk!=NULL){
    for(int itrk=0; itrk<gl_ntrk; itrk++){
      gl_itrack=itrk; 
      //cout<<"gl_itrack="<<gl_itrack<<"   itrk="<<itrk<<endl;
      gl_mom = trk->get_mom(itrk);
      gl_tr_mom[itrk]=gl_mom; 
      gl_phi0 = trk->get_phi0(itrk);
      if(gl_phi0 > M_PI) gl_phi0 = gl_phi0 - 2.*M_PI;
      gl_tr_phi0[itrk]=gl_phi0;
      
      gl_the0 = trk->get_the0(itrk); //theta at the vertex
      gl_tr_the0[itrk]=gl_the0;
      gl_pt = fabs(gl_mom)*sin(gl_the0);//pt ok
      gl_tr_pt[itrk]= gl_pt;
      gl_charge = trk->get_charge(itrk);//charge
      gl_tr_charge[itrk] = gl_charge;
      gl_emcdz = trk->get_emcdz(itrk);//emcz
      gl_tr_emcdz[itrk]= gl_emcdz;
      gl_emcdphi = trk->get_emcdphi(itrk);//emcdphi
      gl_tr_emcdphi[itrk]= gl_emcdphi;
      gl_pc3dz = trk->get_pc3dz(itrk);//pc3dz
      gl_tr_pc3dz[itrk]=gl_pc3dz;
      gl_pc3dphi = trk->get_pc3dphi(itrk);//pc3dphi
      gl_tr_pc3dphi[itrk]=gl_pc3dphi;
      gl_zed= trk->get_zed(itrk);// Z coordinate at which the track crosses PC1
      gl_tr_zed[itrk]=gl_zed;
      gl_trk_quality = trk->get_quality(itrk);   
      gl_tr_trk_quality[itrk]=gl_trk_quality;
      gl_n0=trk->get_n0(itrk);
      gl_tr_n0[itrk]=gl_n0;
      gl_ecore=trk->get_ecore(itrk); //EMC "shower core" energy.
      gl_tr_ecore[itrk]=gl_ecore;

      float ux = cos(gl_phi0); //unit vector 
      float uy = sin(gl_phi0); //unit vector
      
      for(int i=0; i<nsvxcluslist; i++){
        //--------VTX information ------------------------------    
        SvxCluster *cluster0 = svxcluslist->get_Cluster(i);
        gc_ilayer = cluster0->get_layer();
        gc_iladder = cluster0->get_ladder(); 
        gc_svxx = cluster0->get_xyz_global(0);
        float gc_svxxc = gc_svxx - svxprim[0];
        gc_svxy = cluster0->get_xyz_global(1);
        float gc_svxyc = gc_svxy - svxprim[1];
        gc_svxz = cluster0->get_xyz_global(2);
        gc_phiv = atan2(gc_svxy,gc_svxx);
        
        float svx_xy2 = gc_svxxc*gc_svxxc+gc_svxyc*gc_svxyc;
        float svx_xy = sqrt(svx_xy2);
        
        // vector product (ux,uy) x (dx, dy)
        // this is signed difference between straight line projection
        // from primary vertex (svxprim[0],svxprim[1]) and a svx hit position.
        
        const float gc_bendconst=0.00135;
        gc_dproj = ux*gc_svxyc - uy*gc_svxxc;
        gc_mag_bend   = -1.0*m_fieldScale*gc_bendconst*gl_charge*svx_xy2/gl_pt;
        gc_zproj = svx_xy/tan(gl_the0);
        

        //angle cut to reject the cluster which is opposite side of the track
        const float clustercut_angle=0.25;
        if(fabs(fabs(gc_phiv)-fabs(gl_phi0))<clustercut_angle){
          //pt cut
          const float pt_cut_min=0.5;
          const float pt_cut_max=10.0;
          if(trk->get_mom(itrk)>pt_cut_min && trk->get_mom(itrk)<pt_cut_max){
            if(
               fabs(gl_pc3dphi-gl_emcdphi+pc3dphi_emcdphioffset) < pc3dphi_emcdphicut 
            && fabs(gl_pc3dz-gl_emcdz+pc3dz_emcdzoffset) < pc3dz_emcdzcut 
            && gl_pc3dphi>-2000 
            && gl_emcdphi>-2000
            && (gl_trk_quality==31 || gl_trk_quality==63)
            && (fabs(gc_zproj+bbcz - gc_svxz)<zproj_cut) 
            && (fabs(gc_dproj+gc_mag_bend)<dproj_cut)) // for default
            {
              float dphi = gc_dproj + gc_mag_bend;
              float dz_vtx = gc_zproj + svxprim[2] -gc_svxz;
              int iew = 0; 
              if(fabs(gl_phi0)<1.5){
                iew=0;
              }else{
                iew=1;
              }
              int fillflag_dphi = 0; 
              int fillflag_dz = 0;
              static const int nlayers = N_LAYER;
              static const float phi_sigma[nlayers]={0.02,0.03,0.07,0.09};
              static const float z_sigma[nlayers]={0.05,0.05,0.09,0.12};
              if(fabs(svxprim[2]-bbcz)<4.0 && fabs(dz_vtx)<2.0*z_sigma[gc_ilayer])fillflag_dz =1;	
              if(fabs(dphi)<2.0*phi_sigma[gc_ilayer])fillflag_dphi=1;
              if(fillflag_dphi==1){
                if(fabs(svxprim[2]-bbcz)>0.00001 && fabs(svxprim[2]-bbcz)<4.0) h1_dz_vtx_ladder_EW[gc_ilayer][gc_iladder][iew]->Fill(dz_vtx);
              }
              if(fillflag_dz==1){
                h1_dphi_ladder_EW[gc_ilayer][gc_iladder][iew]->Fill(dphi);
              }
              if(fillflag_dphi==1 && fillflag_dz==1){
                h2_dz_zvtx_vs_dphi_ladder[gc_ilayer][gc_iladder]->Fill(dphi,dz_vtx);
                h2_dphi_vs_z_ladder[gc_ilayer][gc_iladder]->Fill(gc_svxz,dphi);
                if((gc_ilayer==0 && gc_iladder==7)||(gc_ilayer==1 && (gc_iladder==14 ||gc_iladder==15)) ||(gc_ilayer==2 && gc_iladder==11) ||(gc_ilayer==3 && gc_iladder == 18) ){
                  if(gc_phiv<=0){gc_phiv=gc_phiv+2.*M_PI;}
                }
                h2_dphi_vs_phiv_ladder[gc_ilayer][gc_iladder]->Fill(gc_phiv,dphi);
              }
            }//  if(fabs(gc_zproj+bbcz - gc_svxz)<5.0) 
          }//if(trk->get_mom(itrk)>pt_cut_min&&trk->get_mom(itrk)<pt_cut_max)
        }//if(fabs(fabs(gc_phiv)-fabs(gl_phi0))<clustercut_angle)
      } //for nsvxclsterlist
    }// for ntrk
  }// 

}

void SvxAlignment_QA::set_WestBeamCenter(float xwest,float ywest){
  xbeamcenter_west=xwest;
  ybeamcenter_west=ywest;
  m_readbeamcenterFromDB=false;
  cout << "SvxAlignment_QA :: set beam center west (" << xbeamcenter_west <<  "," << ybeamcenter_west << ")" << endl;
} 

void SvxAlignment_QA::set_EastBeamCenter(float xeast,float yeast){
  xbeamcenter_east=xeast;
  ybeamcenter_east=yeast;
  m_readbeamcenterFromDB=false;
  cout << "SvxAlignment_QA :: set beam center east (" << xbeamcenter_east <<  "," << ybeamcenter_east << ")" << endl;
} 

