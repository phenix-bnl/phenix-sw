#include "SvxQAForProduction.h"
#include "SvxQADefs.h"

#include <Bbc.hh>
#include <BbcOut.h>
#include <EventHeader.h>
#include <Fun4AllServer.h>
#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <PHCentralTrack.h>
#include <PHCompositeNode.h>
#include <PHGlobal.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHPoint.h>
#include <PHTrackOut.h>
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
#include <SvxParManager.h>
#include <SvxCentralTrackv9.h>
#include <SvxCentralTrackReco.h>
#include <SvxCentralTrackListv9.h>
#include <SvxClusterInfo.h>
#include <SvxPixelHotDeadMap.h>
#include <SvxQAEventSelection.h>

#include <TFile.h>
#include <TH1F.h>
#include <TH2F.h>

#include <cstring> // for memset
#include <iostream>


using namespace std;
using namespace findNode;

//--------------------------------------------------------------------------------

SvxQAForProduction::SvxQAForProduction(string filename):
  SubsysReco("SvxQAForProduction"),
  hm(NULL),
  h_zvtx(NULL),
  h_bbcq(NULL),
  h_bbcq_zcut(NULL),
  h_seedz_when_primisnan(NULL),
  h_bbcz_minus_seedz(NULL),
  h_bbcz_minus_primz(NULL),
  h_seedz_minus_primz(NULL),
  h_seedz_vs_bbcz(NULL),
  h_primz_vs_bbcz(NULL),
  h_primz_vs_seedz(NULL),
  h_primx(NULL),
  h_primy(NULL),
  h_primx_minus_dbcenter(NULL),
  h_primy_minus_dbcenter(NULL),
  h_primxy2d_minus_dbcenter(NULL),
  h_EWoffset_x(NULL),
  h_EWoffset_y(NULL),
  h_EWoffset_z(NULL),
  h_nsegment_bbcqbin(NULL),
  h_nprimary_bbcqbin(NULL),
  h_nseed_bbcqbin(NULL),
  h_nsegment_bbcqbin_zcut(NULL),
  h_nprimary_bbcqbin_zcut(NULL),
  h_nseed_bbcqbin_zcut(NULL),
  h_seg_3hitprobability(NULL),
  h_seg_4hitprobability(NULL),
  h_seg_pt(NULL),
  h_seg_pz(NULL),
  h_nsvxc_bbcqbin(NULL),
  h_nsvxc_vs_ncnt(NULL),
  h_svxc_nhit(NULL),
  m_hpt_goodcnt_west(NULL),
  m_hpt_goodcnt_east(NULL),
  m_hquality_cnt_west(NULL),
  m_hquality_cnt_east(NULL),
  m_hemcdphi_west(NULL),
  m_hemcdphi_east(NULL),
  m_hemcdz_west(NULL),
  m_hemcdz_east(NULL),
  m_hpc3dphi_west(NULL),
  m_hpc3dphi_east(NULL),
  m_hpc3dz_west(NULL),
  m_hpc3dz_east(NULL),
  run(NULL),
  event_header(NULL),
  bbc(NULL),
  svxcluslist(NULL),
  svxtracks(NULL),
  trk(NULL),
  svxcnttrklist(NULL),
  vtxout(NULL),
  beamcenter(NULL),
  runnumber(0),
  bbczcut(8), //8cm
  m_readbeamcenterFromDB(true),
  d_beamcenterFromDB_x(0),
  d_beamcenterFromDB_y(0),
  d_eventselection(NULL)
{
  //  outfilename = filename;

  init_variables();

  // all histogram is null
  // hstOutFile = NULL;

  memset(h_seg_3hitprobability_pt, 0, sizeof(h_seg_3hitprobability_pt));
  memset(h_seg_4hitprobability_pt, 0, sizeof(h_seg_4hitprobability_pt));

  memset(h_svxc_chi2_E, 0, sizeof(h_svxc_chi2_E));
  memset(h_svxc_chi2_W, 0, sizeof(h_svxc_chi2_W));
  memset(h_svxc_chi2_pt_E, 0, sizeof(h_svxc_chi2_pt_E));
  memset(h_svxc_chi2_pt_W, 0, sizeof(h_svxc_chi2_pt_W));

  memset(h_svxc_dca2d_pt_E, 0, sizeof(h_svxc_dca2d_pt_E));
  memset(h_svxc_dca2d_pt_W, 0, sizeof(h_svxc_dca2d_pt_W));
  memset(h_svxc_dcaz_pt_E, 0, sizeof(h_svxc_dcaz_pt_E));
  memset(h_svxc_dcaz_pt_W, 0, sizeof(h_svxc_dcaz_pt_W));

  memset(h_svxc_dca2dp_pt_E, 0, sizeof(h_svxc_dca2dp_pt_E));
  memset(h_svxc_dca2dp_pt_W, 0, sizeof(h_svxc_dca2dp_pt_W));
  memset(h_svxc_dcazp_pt_E, 0, sizeof(h_svxc_dcazp_pt_E));
  memset(h_svxc_dcazp_pt_W, 0, sizeof(h_svxc_dcazp_pt_W));

  //for pixel
  memset(h2_pixelcluster,0,sizeof(h2_pixelcluster));
  memset(h2_stripcluster,0,sizeof(h2_stripcluster));
  memset(pixel_bbcq_vs_nhit, 0, sizeof(pixel_bbcq_vs_nhit));
  memset(strip_bbcq_vs_nhit, 0, sizeof(strip_bbcq_vs_nhit));


}


//--------------------------------------------------------------------------------

SvxQAForProduction::~SvxQAForProduction() 
{
  delete d_eventselection;
}


int SvxQAForProduction::Init(PHCompositeNode *topNode) {   //callded whenever datafrom a new run is encountered
  if(verbosity>0) cout << "Init:SvxQAForProduction" << endl;

  // initialize histogram
  init_histo();

  // initialize event selection handler
  d_eventselection = new SvxQAEventSelection();
  d_eventselection->Set_BBCZCut(bbczcut);
  d_eventselection->Set_TickCut(true);
  d_eventselection->Set_Verbosity(verbosity);

  return 0;
}

//--------------------------------------------------------------------------------
int SvxQAForProduction::InitRun(PHCompositeNode *topNode) //callded once at startup
{
  if(verbosity>0){
    cout<<"SvxQAForProduction::InitRun"<<endl;
  }
  init_nodes(topNode);


  return EVENT_OK;
}

int SvxQAForProduction::process_event(PHCompositeNode *topNode) //callded for every event
{
  init_variables();
  if(!init_nodes_event(topNode)){
    if(verbosity>0) cout << PHWHERE <<  "can not find nodes " << endl;
  }

  //this handles the trigger & event selection
  if (!d_eventselection->EventSelection(topNode)) return EVENT_OK;

  fill_histo();


  return EVENT_OK;
}

//-------------------------------------------------------------------------------

int SvxQAForProduction::End(PHCompositeNode *topNode) // Last call before you quit
{

  if(verbosity>0)cout << PHWHERE << "Writing out histograms..." << endl;
  if(verbosity>0) cout << PHWHERE << "   done." << endl;

  // all histogram are deleted at TFile::Close

  return EVENT_OK;
}


bool SvxQAForProduction::init_histo(){

   Fun4AllServer *se = Fun4AllServer::instance();
   Fun4AllHistoManager *hm = se->getHistoManager(HistoManagerName);
   if (!hm)
   {
     hm = new Fun4AllHistoManager(HistoManagerName);
     se->registerHistoManager(hm);
   }
  //define hist
  h_zvtx = new TH1F("h_zvtx","bbc zvertex",100,-50,50);
  hm->registerHisto(h_zvtx);
  h_bbcq = new TH1F("h_bbcq","bbc charge",250,0,2500);
  hm->registerHisto(h_bbcq);
  h_bbcq_zcut = new TH1F("h_bbcq_zcut","bbc charge with bbcz cut",250,0,2500);
  hm->registerHisto(h_bbcq_zcut);
  h_seedz_when_primisnan = new TH1F("h_seedz_when_primisnan","h_seedz_when_primisnan",1000,-1000,1000);
  hm->registerHisto(h_seedz_when_primisnan);
  h_bbcz_minus_seedz = new TH1F("h_bbcz_minus_seedz","hbbcz_minus_seedz",1000,-15,15);
  hm->registerHisto(h_bbcz_minus_seedz);
  h_bbcz_minus_primz = new TH1F("h_bbcz_minus_primz","hbbcz_minus_primz",1000,-15,15);
  hm->registerHisto(h_bbcz_minus_primz);
  h_seedz_minus_primz = new TH1F("hseedz_minus_primz","hseedz_minus_primz",5000,-15,15);
  hm->registerHisto(h_seedz_minus_primz);
  h_seedz_vs_bbcz = new TH2F("h_seedz_vs_bbcz","h_seedz_vs_bbcz",1000,-15,15,1000,-15,15);
  hm->registerHisto(h_seedz_vs_bbcz);
  h_primz_vs_bbcz = new TH2F("h_primz_vs_bbcz","h_primz_vs_bbcz",3000,-15,15,1000,-15,15);
  hm->registerHisto(h_primz_vs_bbcz);
  h_primz_vs_seedz = new TH2F("h_primz_vs_seedz","h_primz_vs_seedz",3000,-15,15,3000,-15,15);
  hm->registerHisto(h_primz_vs_seedz);

  //beam center check
  h_primx = new TH1F("h_primary_x","Primary x ",1000,-0.5,0.5);
  hm->registerHisto(h_primx);
  h_primy = new TH1F("h_primary_y","Primary y ",1000,-0.5,0.5);
  hm->registerHisto(h_primy);

  h_EWoffset_x = new TH1F("h_EWoffset_x","EastToWest offset x",1000,-0.2,0.2);
  h_EWoffset_x->SetXTitle("west - east (cm)");
  hm->registerHisto(h_EWoffset_x);
  h_EWoffset_y = new TH1F("h_EWoffset_y","EastToWest offset y",1000,-0.2,0.2);
  h_EWoffset_y->SetXTitle("west - east (cm)");
  hm->registerHisto(h_EWoffset_y);
  h_EWoffset_z = new TH1F("h_EWoffset_z","EastToWest offset z",1000,-0.2,0.2);
  h_EWoffset_z->SetXTitle("west - east (cm)");
  hm->registerHisto(h_EWoffset_z);

  h_primx_minus_dbcenter = new TH1F("h_primary_x_minus_dbcenter","Primary x - xcenterDB ",1000,-0.5,0.5);
  hm->registerHisto(h_primx_minus_dbcenter);
  h_primy_minus_dbcenter = new TH1F("h_primary_y_minus_dbcenter","Primary y - ycenterDB ",1000,-0.5,0.5);
  hm->registerHisto(h_primy_minus_dbcenter);
  h_primxy2d_minus_dbcenter = new TH2F("h_primary_xy2d_minus_dbcenter","Primary - beam center in DB plot",1000,-0.5,0.5,1000,-0.5,0.5);
  hm->registerHisto(h_primxy2d_minus_dbcenter);

  //segment QA
  h_nprimary_bbcqbin = new TH1F("h_nprimary_bbcqbin","number of primary with bbcq bin",250,0,2500);
  hm->registerHisto(h_nprimary_bbcqbin);
  h_nsegment_bbcqbin = new TH1F("h_nsegment_bbcqbin","number of segment with bbcq bin",250,0,2500);
  hm->registerHisto(h_nsegment_bbcqbin);
  h_nseed_bbcqbin = new TH1F("h_nseed_bbcqbin","number of seed with bbcq bin",250,0,2500);
  hm->registerHisto(h_nseed_bbcqbin);

  h_nprimary_bbcqbin_zcut = new TH1F("h_nprimary_bbcqbin_zcut","number of primary with bbcq bin with bbcz cut",250,0,2500);
  hm->registerHisto(h_nprimary_bbcqbin_zcut);
  h_nsegment_bbcqbin_zcut = new TH1F("h_nsegment_bbcqbin_zcut","number of segment with bbcq bin with bbcz cut",250,0,2500);
  hm->registerHisto(h_nsegment_bbcqbin_zcut);
  h_nseed_bbcqbin_zcut = new TH1F("h_nseed_bbcqbin_zcut","number of seed with bbcq bin with bbcz cut",250,0,2500);
  hm->registerHisto(h_nseed_bbcqbin_zcut);


  static const int n_ptbin = N_PTBIN;

  h_seg_3hitprobability = new TH1F("h_seg_3hitprobability","h_seg_3hitprobability",100,0,1);
  for(int ipt=0; ipt<n_ptbin; ipt++){
    h_seg_3hitprobability_pt[ipt] = new TH1F(Form("h_seg_3hitprobability_pt%d",ipt),Form("h_seg_3hitprobability_pt%d",ipt),100,0,1);
    hm->registerHisto(h_seg_3hitprobability_pt[ipt]);
  }

  h_seg_4hitprobability = new TH1F("h_seg_4hitprobability","h_seg_4hitprobability",100,0,1);
  for(int ipt=0; ipt<n_ptbin; ipt++){
    h_seg_4hitprobability_pt[ipt] = new TH1F(Form("h_seg_4hitprobability_pt%d",ipt),Form("h_seg_4hitprobability_pt%d",ipt),100,0,1);
    hm->registerHisto(h_seg_4hitprobability_pt[ipt]);
  }

  h_seg_pt = new TH1F("h_seg_pt","h_seg_pt",200,0,20);
  hm->registerHisto(h_seg_pt);
  h_seg_pz = new TH1F("h_seg_pz","h_seg_pz",200,0,20);
  hm->registerHisto(h_seg_pz);

  //svxcnt
  h_nsvxc_bbcqbin = new TH1F("h_nsvxc_bbcqbin","number of svxcnt track with bbcq bin ",250,0,2500);
  hm->registerHisto(h_nsvxc_bbcqbin);
  h_nsvxc_vs_ncnt = new TH2F("h_nsvxc_vs_ncnt","number of svxcnt track vs cnt ",1000,0,1000,1000,0,1000);
  hm->registerHisto(h_nsvxc_vs_ncnt);

  static const int n_hitbin=N_HITBIN;
  for(int i=0;i<n_hitbin;i++){
    h_svxc_chi2_E[i]= new TH1F(Form("h_svxc_chi2_E_hit%d",i), Form("h_svxc_chi2_E_hit%d",i),1000,0,1000);
    hm->registerHisto(h_svxc_chi2_E[i]);
    h_svxc_chi2_W[i]= new TH1F(Form("h_svxc_chi2_W_hit%d",i), Form("h_svxc_chi2_W_hit%d",i),1000,0,1000);
    hm->registerHisto(h_svxc_chi2_W[i]);
  }

  for(int i=0;i<n_hitbin;i++){
    for(int j=0;j<n_ptbin;j++){
      h_svxc_chi2_pt_E[i][j] = new TH1F(Form("h_svxc_chi2_hit%d_pt%d_E",i,j), Form("h_svxc_chi2_hit%d_pt%d_E",i,j),200,0,200);
      hm->registerHisto(h_svxc_chi2_pt_E[i][j]);
      h_svxc_chi2_pt_W[i][j] = new TH1F(Form("h_svxc_chi2_hit%d_pt%d_W",i,j), Form("h_svxc_chi2_hit%d_pt%d_W",i,j),200,0,200);
      hm->registerHisto(h_svxc_chi2_pt_W[i][j]);
    }

  }

  for(int ipt=0; ipt<n_ptbin; ipt++){
    h_svxc_dca2d_pt_E[ipt] = new TH1F(Form("h_svxc_dca2d_pt%d_E",ipt),Form("h_svxc_dca2d_pt%d_E",ipt),2000,-0.5,0.5);
    hm->registerHisto(h_svxc_dca2d_pt_E[ipt]);
    h_svxc_dca2d_pt_W[ipt] = new TH1F(Form("h_svxc_dca2d_pt%d_W",ipt),Form("h_svxc_dca2d_pt%d_W",ipt),2000,-0.5,0.5);
    hm->registerHisto(h_svxc_dca2d_pt_W[ipt]);
    h_svxc_dcaz_pt_E[ipt]  = new TH1F(Form("h_svxc_dcaz_pt%d_E",ipt),Form("h_svxc_dcaz_pt%d_E",ipt),2000,-1,1);
    hm->registerHisto(h_svxc_dcaz_pt_E[ipt]);
    h_svxc_dcaz_pt_W[ipt]  = new TH1F(Form("h_svxc_dcaz_pt%d_W",ipt),Form("h_svxc_dcaz_pt%d_W",ipt),2000,-1,1);
    hm->registerHisto(h_svxc_dcaz_pt_W[ipt]);

    h_svxc_dca2dp_pt_E[ipt] = new TH1F(Form("h_svxc_dca2dp_pt%d_E",ipt),Form("h_svxc_dca2dp_pt%d_E",ipt),2000,-0.5,0.5);
    hm->registerHisto(h_svxc_dca2dp_pt_E[ipt]);
    h_svxc_dca2dp_pt_W[ipt] = new TH1F(Form("h_svxc_dca2dp_pt%d_W",ipt),Form("h_svxc_dca2dp_pt%d_W",ipt),2000,-0.5,0.5);
    hm->registerHisto(h_svxc_dca2dp_pt_W[ipt]);
    h_svxc_dcazp_pt_E[ipt]  = new TH1F(Form("h_svxc_dcazp_pt%d_E",ipt),Form("h_svxc_dcazp_pt%d_E",ipt),2000,-1,1);
    hm->registerHisto(h_svxc_dcazp_pt_E[ipt]);
    h_svxc_dcazp_pt_W[ipt]  = new TH1F(Form("h_svxc_dcazp_pt%d_W",ipt),Form("h_svxc_dcazp_pt%d_W",ipt),2000,-1,1);
    hm->registerHisto(h_svxc_dcazp_pt_W[ipt]);
  }
  h_svxc_nhit = new TH1F("h_svxc_nhit","h_svxc_nhit",10,0,10);
  hm->registerHisto(h_svxc_nhit);

  //cluster z-phi distribution
  h2_pixelcluster[0] = new TH2F("h2_cluster_zphi_0", "cluster distribution of barrel 0",500, -15, 15, 700, -2, 5);
  h2_pixelcluster[0]->SetXTitle("z(cm)");
  h2_pixelcluster[0]->SetYTitle("phi");
  hm->registerHisto(h2_pixelcluster[0]);
  h2_pixelcluster[1] = new TH2F("h2_cluster_zphi_1", "cluster distribution of barrel 1",500, -15, 15, 700, -2, 5);
  h2_pixelcluster[1]->SetXTitle("z(cm)");
  h2_pixelcluster[1]->SetYTitle("phi");
  hm->registerHisto(h2_pixelcluster[1]);


  h2_stripcluster[0] = new TH2F("h2_cluster_zphi_2", "cluster distribution of barrel 2",400, -20, 20, 700, -2, 5);
  h2_stripcluster[0]->SetXTitle("z(cm)");
  h2_stripcluster[0]->SetYTitle("phi");
  hm->registerHisto(h2_stripcluster[0]);
  h2_stripcluster[1] = new TH2F("h2_cluster_zphi_3", "cluster distribution of barrel 3",400, -20, 20, 700, -2, 5);
  h2_stripcluster[1]->SetXTitle("z(cm)");
  h2_stripcluster[1]->SetYTitle("phi");
  hm->registerHisto(h2_stripcluster[1]);


  const int nladder[4] = {10, 20, 16, 24};
  const int nsensor[4] = {4, 4, 5, 6};
  for (int ilr = 0; ilr < 2; ilr++) {
    for (int ild = 0; ild < nladder[ilr]; ild++) {
      for (int isn = 0; isn < 4; isn++) {
        for (int ichp = 0; ichp < 4; ichp++) {
          int ROCno = ichp + (isn % 2) * 4;//ROCno 0-8
          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]
          = new TH2I(Form("h2_bbcq_vs_nhit_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("pixel bbccharge vs nhit B%dL%dS%d Roc%d",ilr,ild,isn,ROCno),100,0,2000,100,0,100);
          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]->SetXTitle("bbc charge");
          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]->SetYTitle("ncluster");
          hm->registerHisto(pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]);
        }
      }
    }
  }
  for (int istriplr = 0; istriplr < 2; istriplr++) {
    for (int ild = 0; ild < nladder[istriplr+2]; ild++) {
      for (int isn = 0; isn < nsensor[istriplr+2]; isn++) {
        for (int iSS = 0; iSS < 2; iSS++) {
          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]
          = new TH2I(Form("h2_bbcq_vs_nhit_B%dL%dS%dC%d",istriplr+2,ild,isn,iSS),Form("strip bbccharge vs nhit B%dL%dS%dC%d",istriplr+2,ild,isn,iSS),100,0,2000,100,0,100);
          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]->SetXTitle("bbc charge");
          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]->SetYTitle("ncluster");
          hm->registerHisto(strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]);
        }//iSS
      }//isn
    }//ild
  }//istrip

  m_hpt_goodcnt_west = new TH1F("hpt_goodcnt_west","hpt_goodcnt_west",1000,0,100);
  hm->registerHisto(m_hpt_goodcnt_west);
  m_hpt_goodcnt_east = new TH1F("hpt_goodcnt_east","hpt_goodcnt_east",1000,0,100);
  hm->registerHisto(m_hpt_goodcnt_east);
  m_hquality_cnt_west = new TH1I("hquality_cnt_west","hquality_cnt_west",100,0,100);
  hm->registerHisto(m_hquality_cnt_west);
  m_hquality_cnt_east = new TH1I("hquality_cnt_east","hquality_cnt_east",100,0,100);
  hm->registerHisto(m_hquality_cnt_east);
  m_hemcdphi_west = new TH1F("hemcdphi_west","hemcdphi_west",400,-0.2,0.2);
  hm->registerHisto(m_hemcdphi_west);
  m_hemcdphi_east = new TH1F("hemcdphi_east","hemcdphi_east",400,-0.2,0.2);
  hm->registerHisto(m_hemcdphi_east);
  m_hemcdz_west = new TH1F("hemcdz_west","hemcdz_west",120,-60,60);
  hm->registerHisto(m_hemcdz_west);
  m_hemcdz_east = new TH1F("hemcdz_east","hemcdz_east",120,-60,60);
  hm->registerHisto(m_hemcdz_east);
  m_hpc3dphi_west = new TH1F("hpc3dphi_west","pc3dphi_west",100,-0.2,0.2);
  hm->registerHisto(m_hpc3dphi_west);
  m_hpc3dphi_east = new TH1F("hpc3dphi_east","pc3dphi_east",100,-0.2,0.2);
  hm->registerHisto(m_hpc3dphi_east);
  m_hpc3dz_west = new TH1F("hpc3dz_west","pc3dz_west",120,-60,60);
  hm->registerHisto(m_hpc3dz_west);
  m_hpc3dz_east = new TH1F("hpc3dz_east","pc3dz_east",120,-60,60);
  hm->registerHisto(m_hpc3dz_east);

  return true;
}


bool SvxQAForProduction::init_nodes(PHCompositeNode *topNode){

  run = findNode::getClass<RunHeader>(topNode,"RunHeader");
  if(!run){
    if(verbosity)cerr <<  PHWHERE  << " ERROR: Can't find RunHeader " << endl;
    return false;
  }
  runnumber = run->get_RunNumber();
  if(verbosity>0) cout<<"SvxQAForProduction::init_node runnumber="<<runnumber<<endl;

  beamcenter = findNode::getClass<SvxBeamCenterPar>(topNode, "SvxBeamCenterPar");
  if(beamcenter){
    if(m_readbeamcenterFromDB){
//      beamcenter->fetchFromDB(runnumber);
      d_beamcenterFromDB_x = beamcenter->getBeamCenter(0);
      d_beamcenterFromDB_y = beamcenter->getBeamCenter(1);
      if(verbosity>0){
        cout << "SvxQAForProduction  :: Get beam center from DB" << endl;
        beamcenter->print();
      }
    }
  }else{
    if(verbosity>0) cerr << PHWHERE<< "Can't find BeamCenter object. " << endl;
    return false;
  }

  return true;
}

bool SvxQAForProduction::init_nodes_event(PHCompositeNode *topNode){

  event_header = getClass<EventHeader>(topNode,"EventHeader");

  bbc = getClass<BbcOut>(topNode,"BbcOut");
  if(!bbc) {if(verbosity>0)cerr <<  PHWHERE  << " ERROR: Can't find BbcOut " << endl;}

  vtxout = getClass<VtxOut>(topNode,"VtxOut");
  if(!vtxout) {if(verbosity>0)cerr <<  PHWHERE  << " ERROR: Can't find VtxOut "<< endl;}

  svxcluslist= getClass<SvxClusterList>(topNode, "SvxClusterList");
  if (svxcluslist==NULL) {if(verbosity>0)cerr << PHWHERE << " ERROR: Can't find SvxClusterList." << endl;}

  svxtracks = getClass<SvxSegmentList>(topNode,"SvxSegmentList");
  if(svxtracks==NULL) {if(verbosity>0)cerr <<  PHWHERE  << " ERROR: Can't find SvxSegmentList "<< endl;}

  trk = getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(!trk) {if(verbosity>0)cerr <<  PHWHERE  << " ERROR: Can't find PHCentralTrack "<< endl;}

  svxcnttrklist = getClass<SvxCentralTrackList>(topNode,"SvxCentralTrackList");
  if(!svxcnttrklist) {if(verbosity>0)cerr <<  PHWHERE  << " ERROR: Can't find PHCentralTrack "<< endl;}

  return true;
}

void SvxQAForProduction::init_variables()
{

  nsvxtracks=0;
  nsvxcluster=0;

  if(verbosity>0)  cout << "init variables" << endl;
  return;
}


void SvxQAForProduction::fill_histo()
{
  //zvtx
  if(verbosity>0)  cout << "filling histo" << endl;
  float bbcz = vtxout->get_Vertex("BBC").getZ();
  float bbcq = bbc->get_ChargeSum(Bbc::North) + bbc->get_ChargeSum(Bbc::South);

  PHPoint verpoint = vtxout->get_Vertex();

  float svxseed[3]={-9999,-9999,-9999};//data from vtxout node
  float svxprim[3]={-9999,-9999,-9999};//data from vtxout node


  PHPoint verpointsvxseed = vtxout->get_Vertex("SVX");
  if(verbosity>0){
    if(isnan(svxseed[2])) cout << "NAN: SVX Z " << endl;
  }
  if(vtxout->isVtx("SVX")){
    svxseed[0] = verpointsvxseed.getX();
    svxseed[1] = verpointsvxseed.getY();
    svxseed[2] = verpointsvxseed.getZ();
  }

  PHPoint verpointsvxprim = vtxout->get_Vertex("SVX_PRECISE");
  if(vtxout->isVtx("SVX_PRECISE")){
    svxprim[0] = verpointsvxprim.getX();
    svxprim[1] = verpointsvxprim.getY();
    svxprim[2] = verpointsvxprim.getZ();
  }
  if(verbosity>0){
    if(isnan(svxprim[2])) cout << "NAN: SVX_PRICISE" << endl;
  }
  //zvtx
  if(verbosity>0)  cout << "filling zvtx info." << endl;
  h_zvtx->Fill(bbcz);
  h_bbcq->Fill(bbcq);
  if(isnan(svxprim[2])) h_seedz_when_primisnan->Fill(svxseed[2]);
  if(fabs(bbcz)<bbczcut){
    h_bbcq_zcut->Fill(bbcq);
    if(svxseed[2] != bbcz) h_bbcz_minus_seedz->Fill(bbcz-svxseed[2]);
    h_bbcz_minus_primz->Fill(bbcz-svxprim[2]);
    if(svxseed[2] != bbcz &&  vtxout->isVtx("SVX") &&  vtxout->isVtx("SVX_PRECISE")) h_seedz_minus_primz->Fill(svxseed[2]-svxprim[2]);
    if(svxseed[2] != bbcz) h_seedz_vs_bbcz->Fill(svxseed[2],bbcz);
    h_primz_vs_bbcz->Fill(svxprim[2],bbcz);
    if(svxseed[2] != bbcz) h_primz_vs_seedz->Fill(svxprim[2],svxseed[2]);
  }

  //beam center check
  if(verbosity>0)  cout << "filling beam center info." << endl;
  h_primx->Fill(svxprim[0]);
  h_primy->Fill(svxprim[1]);
  h_primx_minus_dbcenter->Fill(svxprim[0]-d_beamcenterFromDB_x);
  h_primy_minus_dbcenter->Fill(svxprim[1]-d_beamcenterFromDB_y);
  h_primxy2d_minus_dbcenter->Fill(svxprim[0]-d_beamcenterFromDB_x,svxprim[1]-d_beamcenterFromDB_y);

  PHPoint vtxpw = vtxout->get_Vertex("SVX_PRECISEW");
  PHPoint vtxpe = vtxout->get_Vertex("SVX_PRECISEE");
  float vtxpW[3] = {(float) vtxpw.getX(), (float) vtxpw.getY(), (float) vtxpw.getZ()};
  float vtxpE[3] = {(float) vtxpe.getX(), (float) vtxpe.getY(), (float) vtxpe.getZ()};
  float vtxp_wediff[3];
  for(int i=0; i<3; i++) {
    vtxp_wediff[i] = ( isnan(vtxpW[i])||isnan(vtxpE[i]) ) ?  -9999. : (vtxpW[i] - vtxpE[i]);
  }
  h_EWoffset_x->Fill(vtxp_wediff[0]);
  h_EWoffset_y->Fill(vtxp_wediff[1]);
  h_EWoffset_z->Fill(vtxp_wediff[2]);

  (svxcluslist!=NULL) ? nsvxcluster = svxcluslist ->get_nClusters():0;
  (svxtracks!=NULL)   ? nsvxtracks = svxtracks->get_nSegments():0;

  h_nsegment_bbcqbin->Fill(bbcq,nsvxtracks);
  if(fabs(bbcz)<bbczcut){
    h_nsegment_bbcqbin_zcut->Fill(bbcq,nsvxtracks);
  }
  //number of primary vertex
  if((!(vtxout->isVtx("SVX_PRECISE")))  || fabs(svxprim[2]) >100 ){
    if(verbosity>0){
      cout <<"no primary vertex" << endl;
    }
  }else{
    h_nprimary_bbcqbin->Fill(bbcq);
    if(fabs(bbcz)<bbczcut){
      h_nprimary_bbcqbin_zcut->Fill(bbcq);
    }
  }

  //number of svx seed
  if((!(vtxout->isVtx("SVX"))) || fabs(svxseed[2]) >100  || svxseed[2] == bbcz ){
    if(verbosity>0){
      cout <<"no seed vertex" << endl;
    }
  }else{
    h_nseed_bbcqbin->Fill(bbcq);
    if(fabs(bbcz)<bbczcut){
      h_nseed_bbcqbin_zcut->Fill(bbcq);
    }
  }

  /////////////////////////////////
  static const int n_ptbin   = N_PTBIN;
  static const int n_hitbin  = N_HITBIN;

  /////////////////////////////////
  //
  // Segment (StandAloneTracking)
  //

  if(verbosity>0) cout << "filling StandAloneTracking info." << endl;
  if(fabs(bbcz)<bbczcut){
    for(int i=0; i<nsvxtracks; i++) {
      SvxSegment* seg0 = svxtracks->get_segment(i);
      int seg_nhit[4]={-1};
      seg_nhit[0] = seg0->getNhits(0);
      seg_nhit[1] = seg0->getNhits(1);
      seg_nhit[2] = seg0->getNhits(2);
      seg_nhit[3] = seg0->getNhits(3);
      float seg_quality = exp(seg0->getQuality());

      float seg_p[3]={-9999};
      seg_p[0]  = seg0->get3Momentum(0);
      seg_p[1]  = seg0->get3Momentum(1);
      seg_p[2]  = seg0->get3Momentum(2);
      float seg_pt  = sqrt(seg_p[0]*seg_p[0]+seg_p[1]*seg_p[1]);

      h_seg_pt->Fill(seg_pt);
      h_seg_pz->Fill(seg_p[2]);
      if (seg_nhit[2]>0  && seg_nhit[3]>0 ){
        h_seg_4hitprobability->Fill(seg_quality);
        for(int i=0 ;i<n_ptbin;++i){
          if(i< seg_pt && seg_pt<i+1 ){
            if(verbosity>10) cout << "filling 4 hit track in segment  "<< i  << endl;
            h_seg_4hitprobability_pt[i]->Fill(seg_quality);
          }
        }
      }else{
        h_seg_3hitprobability->Fill(seg_quality);
        for(int i=0 ;i<n_ptbin;++i){
          if(i< seg_pt && seg_pt<i+1 ){
            if(verbosity>10) cout << "filling 3 hit track in segment  "<< i  << endl;
            h_seg_3hitprobability_pt[i]->Fill(seg_quality);
          }
        }
      }
    }//for nsvxstrack
  }//bbczcut
  if(verbosity>0) cout << "END:filling StandAloneTracking info." << endl;
//////////////////////////////////////////
//central info.
/////////////////////////////////////////

  int gl_ntrk=0;
  if(verbosity>0) cout << "filling Svxcentral info." << endl;
  (trk!=NULL) ? gl_ntrk = trk->get_npart():0;
  for(int itrk=0;itrk<gl_ntrk;itrk++){
    float gl_mom = trk->get_mom(itrk);
    //float gl_phi0 = trk->get_phi0(itrk);
    ////range [-PI/2,3/2PI]
    //if (gl_phi0 < -M_PI/2.) gl_phi0 = gl_phi0 + 2.*M_PI;

    float gl_phi = trk->get_phi(itrk);
    //range [-PI/2,3/2PI]
    if (gl_phi < -M_PI/2.) gl_phi = gl_phi + 2.*M_PI;
    float gl_the0 = trk->get_the0(itrk); //theta at the vertex
    float gl_pt = fabs(gl_mom) * sin(gl_the0);//pt ok
    float gl_emcdz = trk->get_emcdz(itrk);//emcz
    float gl_emcdphi = trk->get_emcdphi(itrk);//emcdphi
    float gl_pc3dz = trk->get_pc3dz(itrk);//pc3dz
    float gl_pc3dphi = trk->get_pc3dphi(itrk);//pc3dphi
    int gl_trk_quality = trk->get_quality(itrk);

    if(fabs(gl_phi)<1.5){
      m_hquality_cnt_west->Fill(gl_trk_quality);
      if( (gl_trk_quality==63) || (gl_trk_quality==31)){
        m_hpt_goodcnt_west->Fill(gl_pt);
        m_hemcdphi_west->Fill(gl_emcdphi);
        m_hemcdz_west->Fill(gl_emcdz);
        m_hpc3dphi_west->Fill(gl_pc3dphi);
        m_hpc3dz_west->Fill(gl_pc3dz);
      }
    }else{
      m_hquality_cnt_east->Fill(gl_trk_quality);
      if( (gl_trk_quality==63) || (gl_trk_quality==31)){
        m_hpt_goodcnt_east->Fill(gl_pt);
        m_hemcdphi_east->Fill(gl_emcdphi);
        m_hemcdz_east->Fill(gl_emcdz);
        m_hpc3dphi_east->Fill(gl_pc3dphi);
        m_hpc3dz_east->Fill(gl_pc3dz);
      }//quality
    }//east
  }//ntrk


  int sc_ntrk=0;
  (svxcnttrklist!=NULL) ? sc_ntrk = svxcnttrklist->get_nCentralTracks():0 ;

  h_nsvxc_bbcqbin->Fill(bbcq,sc_ntrk);
  h_nsvxc_vs_ncnt->Fill(sc_ntrk,gl_ntrk);

  //---------------SVXCentral Track-----------------------------
  //static const int n_ptbin = N_PTBIN;
  for(int itrk=0; itrk<sc_ntrk; itrk++){
    SvxCentralTrack *sctrk = svxcnttrklist->getCentralTrack(itrk);

    float sc_chi2 = sctrk->getChiSquare();
    int sc_nhit = sctrk->getNhits();
    float sc_dca2d = sctrk->getDCA2D();
    float sc_dcaz = sctrk->getDCAZ();
    float sc_dca2dp = sctrk->getDCA2Dprimary();
    float sc_dcazp = sctrk->getDCAZprimary();
    
    int cntidx=sctrk->getDchIndex();
    if(trk==NULL) continue;

    float gl_mom = trk->get_mom(cntidx);
    float gl_the0 = trk->get_the0(cntidx);
    float sc_pt = fabs(gl_mom)*sin(gl_the0);//pt ok
    float sc_phi = trk->get_phi0(cntidx);
    int gl_quality = trk->get_quality(cntidx);
    if((gl_quality !=31) && (gl_quality !=63)) continue;

    h_svxc_nhit->Fill(sc_nhit);

    for(int i=0;i<n_hitbin;i++){
      if(i == sc_nhit){
        if(fabs(sc_phi) < 1.5){
          h_svxc_chi2_W[i]->Fill(sc_chi2);
        }else{
          h_svxc_chi2_E[i]->Fill(sc_chi2);
        }
      }
    }

    for(int i=0 ;i<n_ptbin;i++){
      if(i< sc_pt && sc_pt<i+1 ){
        if(fabs(sc_phi) < 1.5){
          h_svxc_dca2d_pt_W[i]->Fill(sc_dca2d);
          h_svxc_dcaz_pt_W[i]->Fill(sc_dcaz);
          h_svxc_dca2dp_pt_W[i]->Fill(sc_dca2dp);
          h_svxc_dcazp_pt_W[i]->Fill(sc_dcazp);
        }else{
          h_svxc_dca2d_pt_E[i]->Fill(sc_dca2d);
          h_svxc_dcaz_pt_E[i]->Fill(sc_dcaz);
          h_svxc_dca2dp_pt_E[i]->Fill(sc_dca2dp);
          h_svxc_dcazp_pt_E[i]->Fill(sc_dcazp);
        }
      }
    }
  }// for sc_ntrk

  if(verbosity>0) cout << "End::filling SVXCentral info." << endl;
  if(verbosity>0) cout << "filling pixel chip map info." << endl;

  if(verbosity>0) cout << "SvxQAForProduction::fill_histo() start filling cluster" << endl;

  //fill the all clusters
  int nclus_counter[4][24][6][4];
  memset(nclus_counter,0,sizeof(nclus_counter));

  if(svxcluslist!=NULL){
    for(int i = 0; i < nsvxcluster; i++){
      SvxCluster *cls = svxcluslist->get_Cluster(i);

      float cls_x = cls->get_xyz_global(0);
      float cls_y = cls->get_xyz_global(1);
      float cls_z = cls->get_xyz_global(2);

      int cls_layer  = cls->get_layer();
      int cls_ladder = cls->get_ladder();
      int cls_sensor = cls->get_sensor();

      float local_z  = cls->get_xyz_local(2);
      int chipSS = -1;
      if(cls_layer<2){
        if(1.400<=local_z) chipSS=0;
        else if( 0<=local_z && local_z<1.400) chipSS=1;
        else if( -1.400<=local_z && local_z<0) chipSS=2;
        else if( local_z<-1.400) chipSS=3;
      }else{
        if(local_z<0) chipSS=0;//left SS
        else          chipSS=1;//right SS
      }


      nclus_counter[cls_layer][cls_ladder][cls_sensor][chipSS]++;

      float cls_phi = atan2(cls_y,cls_x);
      if(cls_phi <-M_PI/2.) cls_phi = cls_phi+2.*M_PI;

      if(cls_layer == 0 || cls_layer == 1){
        h2_pixelcluster[cls_layer]->Fill(cls_z,cls_phi);
      }else{
        h2_stripcluster[cls_layer-2]->Fill(cls_z,cls_phi);
      }
    }
  }
  //check bbc vs ncluster
  const int nladder[4]={10,20,16,24};
  const int nsensor[4]={4,4,5,6};
  const int nchipSS[4]={4,4,2,2};
  for(int ilr=0;ilr<4;ilr++){
    for(int ild=0;ild<nladder[ilr];ild++){
      for(int isn=0;isn<nsensor[ilr];isn++){
        for(int icpSS=0;icpSS<nchipSS[ilr];icpSS++){
          if(ilr<2) pixel_bbcq_vs_nhit[ilr][ild][isn][icpSS]->Fill(bbcq,nclus_counter[ilr][ild][isn][icpSS]);
          else      strip_bbcq_vs_nhit[ilr-2][ild][isn][icpSS]->Fill(bbcq,nclus_counter[ilr][ild][isn][icpSS]);
        }
      }
    }
  }

  if(verbosity>0) cout << "SvxQAForProduction::fill_histo() End " << endl;
}

void SvxQAForProduction::set_BeamCenter(float xeast,float yeast){
  d_beamcenterFromDB_x = xeast;
  d_beamcenterFromDB_y = yeast;
  m_readbeamcenterFromDB = false;

  cout << "SvxQAForProduction :: set beam center east (" << d_beamcenterFromDB_x <<  "," << d_beamcenterFromDB_y << ")" << endl;
}

