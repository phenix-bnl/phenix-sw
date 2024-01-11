#include <cassert>

//  GENERAL PHENIX tools
#include <Fun4AllServer.h>
#include <getClass.h>
#include <PHCompositeNode.h>
#include <phool.h>
//#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>

//  Data classes I am using in analysis
//#include "PHCentralTrack.h"
//#include "PreviousEvent.h"
#include <TriggerHelper.h>
#include <TrigLvl1.h>
#include <PHGlobal.h>
#include <VtxOut.h>
#include <BbcOut.h>
#include <SyncObject.h>
#include <EventHeader.h>
#include <MpcTriggerStudy.h>
#include <mpcClusterContainer.h>
#include <mpcSampleContainer.h>
#include <mpcSample.h>
#include <mpcClusterContent.h>
#include <mpcSample.h>
#include <mpcSampleContainer.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcRawContainer.h>
#include <mpcRawContent.h>
#include <MpcMap.h>
#include <MpcCalib.h>
#include <MpcTrigEmulator.h>

//  Root histogram types
#include <TH1.h>
#include <TH2.h>
#include <TCanvas.h>
#include <TTree.h>
#include <TFile.h>
#include <TDirectory.h>

using namespace std;
using namespace findNode;

MpcTriggerStudy::MpcTriggerStudy(const char* outfile) : SubsysReco("MpcTriggerStudy Analyzer")
{
  //  Take the output file name as an argument
  OutFileName = outfile;

  return;
}

/*
int MpcTriggerStudy::Init(PHCompositeNode *topNode)
{
  return EVENT_OK;
}
*/

int MpcTriggerStudy::GetTriggerNameList( const int RunNumber )
{

  if ( RunNumber > 364500 && RunNumber < 500000 )	// Run12 p+p 510
    {
      trig_list[0] = "ALL";
      trig_list[1] = "BBCLL1(>0 tubes)";
      trig_list[2] = "BBCLL1(>0 tubes) novertex";
      trig_list[3] = "BBCLL1(>0 tubes) narrowvtx";
      trig_list[4] = "ZDCLL1wide";
      trig_list[5] = "MPCANY";
      trig_list[6] = "MPC_A";
      trig_list[7] = "MPC_B";
      trig_list[8] = "(MPCS_C & MPCS_C)||(MPCN_C & MPCN_C)";
      trig_list[9] = "MPC_C&ERT_2x2";
    }
  else if ( RunNumber > 358513 && RunNumber < 364500 )	// Run12 p+p 200
    {
      trig_list[0] = "ALL";
      trig_list[1] = "BBCLL1(>0 tubes)";
      trig_list[2] = "BBCLL1(>0 tubes) novertex";
      trig_list[3] = "BBCLL1(>0 tubes) narrowvtx";
      trig_list[4] = "ZDCLL1wide";
      trig_list[5] = "MPCANY";
      trig_list[6] = "MPC_A";
      trig_list[7] = "MPC_B";
      trig_list[8] = "(MPCS_C & MPCS_C)";
      trig_list[9] = "MPCN_C&ERT_2x2";
      trig_list[10] = "MPCS_C&ERT_2x2";

/*
      // List of MPC triggers
      mpc_trig_list[0] = 6;	// MPC_A
      mpc_trig_list[1] = 7;	// MPC_B
      mpc_trig_list[2] = 8;	// (MPCS_C & MPCS_C)
*/
    }
  else if ( RunNumber > 205240 && RunNumber < 206500 )	// Run06 62
    {
      trig_list[0] = "ALL";
      trig_list[1] = "BBCLL1(>0 tubes)";
      trig_list[2] = "BBCLL1(noVertexCut)";
      trig_list[3] = "ZDCLL1narrow";
      trig_list[4] = "ZDCLL1wide";
      trig_list[5] = "MPCANY";
      trig_list[6] = "MPCS 4x4c";
      trig_list[7] = "MPCS 4x4a";
      trig_list[8] = "MPCS_2x2";
    }
  else if ( RunNumber > 228000 && RunNumber < 240200 )	// Run07
    {
      trig_list[0] = "ALL";
      trig_list[1] = "BBCLL1(>0 tubes)";
      trig_list[2] = "BBCLL1(noVertexCut)";
      trig_list[3] = "ZDCLL1narrow";
      trig_list[4] = "ZDCLL1wide";
      trig_list[5] = "MPCANY";
      trig_list[6] = "MPC_4x4";
      trig_list[7] = "MPC_2x2";
    }
  else if ( RunNumber > 246000 && RunNumber < 253750 )	// Run08 d+Au
    {
      trig_list[0] = "ALL";
      trig_list[1] = "BBCLL1(>0 tubes)";
      trig_list[2] = "BBCLL1(noVertexCut)";
      trig_list[3] = "ZDCLL1narrow";
      trig_list[4] = "ZDCLL1wide";
      trig_list[5] = "MPCANY";
      trig_list[6] = "MPC_4x4";
      trig_list[7] = "MPC_2x2";
      trig_list[8] = "MPC_4x4&BBCLL1";
      trig_list[9] = "MPC_4x4&BBCLL1(noVertexCut)";
    }
  else if ( RunNumber > 256000 && RunNumber < 260000 )	// Run08 p+p
    {
      trig_list[0] = "ALL";
      trig_list[1] = "BBCLL1(>0 tubes)";
      trig_list[2] = "BBCLL1(noVertexCut)";
      trig_list[3] = "ZDCLL1narrow";
      trig_list[4] = "ZDCLL1wide";
      trig_list[5] = "MPCANY";
      trig_list[6] = "MPC_4x4A";
      trig_list[7] = "MPC_4x4B";
      trig_list[8] = "MPC_4x4C&(ERTLL1_4x4c|ERTLL1_4x4a)";
    }
  else
    {
      cout << PHWHERE << " ERROR, unknown run " << RunNumber << endl;
    }

  return trig_list.size();
}

int MpcTriggerStudy::InitRun(PHCompositeNode *topNode)
{
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");
  if ( mpcmap==0 )
    {
      cout << "MPCMAP = 0" << endl;
    }

/*
  mpcmap = MpcMap::instance();
  if ( mpcmap==0 ) return ABORTEVENT;
*/

  mpccalib = MpcCalib::instance();
  if ( mpccalib==0 )
    {
      cout << PHWHERE << " Couldn't find mpccalib" << endl;
      return ABORTEVENT;
    }
  //mpccalib->Print("DEADHOT");

  // Get Trigger Info
  trighelp = new TriggerHelper(topNode);
  if ( trighelp==0 )
    {
      cout << "MpcTriggerStudy::InitRun, TriggerHelper not found" << endl;
      return ABORTRUN;
    }

  mpctrigemulator = new MpcTrigEmulator();
  mpctrigemulator->SetSumTrigger();
  mpctrigemulator->SetPtTrigger();
  mpctrigemulator->SetAdcSumThreshold( 0, 66 );
  mpctrigemulator->SetAdcSumThreshold( 1, 100 );
  mpctrigemulator->SetAdcSumThreshold( 2, 40 );
  mpctrigemulator->SetTrigChThreshold( -1, 2 );
  mpctrigemulator->SetNhitThreshold( 0, 2 );
  mpctrigemulator->SetNhitThreshold( 1, 2 );
  mpctrigemulator->SetNhitThreshold( 2, 2 );
  //if ( verbosity ) mpctrigemulator->Verbosity(1);

  recoConsts *rc = recoConsts::instance();
  f_run = rc->get_IntFlag("RUNNUMBER");
  int ntrig = GetTriggerNameList( f_run );
  if ( ntrig==0 ) 
    {
      cout << PHWHERE << " ERROR, ntrig==0!!!! " << endl;
      return ABORTRUN;
    }

  float scaledown[MAXTRIG] = {1};
  cout << "Scaledowns" << endl;
  for( map<int, string>::iterator ii=trig_list.begin(); ii!=trig_list.end(); ++ii)
   {
     scaledown[(*ii).first] = trighelp->getLevel1Scaledown((*ii).second) + 1.0;
     cout << (*ii).second << " " << scaledown[(*ii).first] << endl;
   }

  TString name;

  TDirectory *orig_directory = gDirectory;

  savefile = new TFile(OutFileName.c_str(),"RECREATE");

  for (int itrig=0; itrig<MAXTRIG; itrig++)
    {
      for (int ich=0; ich<576; ich++)
        {
          int xgrid = mpcmap->getGridX(ich);
          int ygrid = mpcmap->getGridY(ich);
          if ( xgrid<0 ) continue;
          if ( ygrid<0 ) continue;

          int side = 0;
          if ( ich>287 ) side = 1;

          name = "hch_pt_"; name += side; name += "_"; name += xgrid; name += "_"; name += ygrid; name += "_"; name += itrig;
          hch_pt[itrig][ich] = new TH1F(name,name,200,0.,20.);

          //name = "hch_all_pt_"; name += side; name += "_"; name += xgrid; name += "_"; name += ygrid; name += "_"; name += itrig;
          //hch_all_pt[itrig][ich] = new TH1F(name,name,200,0.,20.);

          name = "hch_energy_"; name += side; name += "_"; name += xgrid; name += "_"; name += ygrid; name += "_"; name += itrig;
          hch_energy[itrig][ich] = new TH1F(name,name,5000,0.,500.);

          //name = "hch_all_energy_"; name += side; name += "_"; name += xgrid; name += "_"; name += ygrid; name += "_"; name += itrig;
          //hch_all_energy[itrig][ich] = new TH1F(name,name,5000,0.,500.);
        }
    }

  h_ntrig = new TH1F("h_ntrig","Trigger Counts",MAXTRIG,-0.5,MAXTRIG-0.5);
  h_ntrigcorr = new TH1F("h_ntrigcorr","Trigger Counts, Prescale Corrected",MAXTRIG,-0.5,MAXTRIG-0.5);
  h2_ntrig = new TH2F("h2_ntrig","Trigger Correlations",MAXTRIG,-0.5,MAXTRIG-0.5,MAXTRIG,-0.5,MAXTRIG-0.5);
  h_prescale = new TH1I("h_prescale","Trigger Prescales",MAXTRIG,-0.5,MAXTRIG-0.5);
  h_nfiles = new TH1I("h_nfiles","Number of Files",MAXTRIG,-0.5,MAXTRIG-0.5);

  h2_ecent[0] = new TH2F("h2_ecent0","ecent south towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_ecent[1] = new TH2F("h2_ecent1","ecent north towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_e9[0] = new TH2F("h2_e90","e9 south towers",18,-0.5,17.5,18,-0.5,17.5);
  h2_e9[1] = new TH2F("h2_e91","e9 north towers",18,-0.5,17.5,18,-0.5,17.5);
  h_erat[0] = new TH1F("h_erat0","e8/ecent south towers",300,0,3);
  h_erat[1] = new TH1F("h_erat1","e8/ecent north towers",300,0,3);
  h_e9rat[0] = new TH1F("h_erat90","ecent/e9, south",300,0,3);
  h_e9rat[1] = new TH1F("h_erat91","ecent/e9, north",300,0,3);
  h_dispx = new TH1F("h_dispx","dispx",10000,0,10);
  h_dispy = new TH1F("h_dispy","dispy",10000,0,10);
  h_corrdispx = new TH1F("h_corrdispx","corr dispx",10000,0,10);
  h_corrdispy = new TH1F("h_corrdispy","corr dispy",10000,0,10);

  TH1F *temphist = 0;
  TH2F *temp2hist = 0;

  for (int itrig=0 ;itrig<MAXTRIG; itrig++)
    {
      // level-1 rbits
      name = "h_rbits"; name += itrig;
      temphist = new TH1F(name,name,130,-0.5,129.5);
      temphist->SetLineColor(itrig+1);
      h_rbits.push_back( temphist );

      // crossing distributions
      name = "h_cross"; name += itrig;
      temphist = new TH1F(name,name,120,-0.5,119.5);
      temphist->SetLineColor(itrig+1);
      h_cross.push_back( temphist );

      name = "h_ncross"; name += itrig;
      temphist = new TH1F(name,name,120,-0.5,119.5);
      temphist->SetLineColor(itrig+1);
      h_ncross.push_back( temphist );

      name = "h_scross"; name += itrig;
      temphist = new TH1F(name,name,120,-0.5,119.5);
      temphist->SetLineColor(itrig+1);
      h_scross.push_back( temphist );

      // 2x2 tile distribution
      name = "h2x2_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      h2x2.push_back(temphist);

      name = "hn_2x2_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hn_2x2.push_back(temphist);

      name = "hs_2x2_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hs_2x2.push_back(temphist);

      name = "h2x2max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      h2x2max.push_back(temphist);

      name = "hn_2x2max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hn_2x2max.push_back(temphist);

      name = "hs_2x2max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,10000.);
      temphist->SetLineColor(itrig+1);
      hs_2x2max.push_back(temphist);

      name = "h2_2x2max_trig"; name += itrig;
      temp2hist = new TH2F(name,name,10,-0.5,19.5,10,-0.5,9.5);
      h2_2x2max.push_back(temp2hist);

      // 4x4 tile distribution
      name = "h4x4_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      h4x4.push_back(temphist);

      name = "hn_4x4_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hn_4x4.push_back(temphist);

      name = "hs_4x4_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hs_4x4.push_back(temphist);

      name = "h4x4max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      h4x4max.push_back(temphist);

      name = "hn_4x4max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hn_4x4max.push_back(temphist);

      name = "hs_4x4max_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,20000.);
      temphist->SetLineColor(itrig+1);
      hs_4x4max.push_back(temphist);

      name = "h2_4x4max_trig"; name += itrig;
      temp2hist = new TH2F(name,name,9,-0.5,17.5,9,-0.5,8.5);
      h2_4x4max.push_back(temp2hist);

      // pt distribution
      name = "hpt_trig"; name += itrig;
      temphist = new TH1F(name,name,2000,0.,20.);
      temphist->SetLineColor(itrig+1);
      hpt.push_back(temphist);

      name = "hn_pt_trig"; name += itrig;
      temphist = new TH1F(name,name,2000,0.,20.);
      temphist->SetLineColor(itrig+1);
      hn_pt.push_back(temphist);

      name = "hs_pt_trig"; name += itrig;
      temphist = new TH1F(name,name,2000,0.,20.);
      temphist->SetLineColor(itrig+1);
      hs_pt.push_back(temphist);

      name = "henergy_trig"; name += itrig;
      temphist = new TH1F(name,name,3000,0.,300.);
      temphist->SetLineColor(itrig+1);
      henergy.push_back(temphist);

      name = "hn_energy_trig"; name += itrig;
      temphist = new TH1F(name,name,3000,0.,300.);
      temphist->SetLineColor(itrig+1);
      hn_energy.push_back(temphist);

      name = "hs_energy_trig"; name += itrig;
      temphist = new TH1F(name,name,3000,0.,300.);
      temphist->SetLineColor(itrig+1);
      hs_energy.push_back(temphist);

      //*****************************
      // No CUTS for high pt background
      //*****************************

      // pt distribution, 
      name = "hpt_all_trig"; name += itrig;
      temphist = new TH1F(name,name,2000,0.,20.);
      temphist->SetLineColor(itrig+1);
      hpt_all.push_back(temphist);

      name = "hn_pt_all_trig"; name += itrig;
      temphist = new TH1F(name,name,2000,0.,20.);
      temphist->SetLineColor(itrig+1);
      hn_pt_all.push_back(temphist);

      name = "hs_pt_all_trig"; name += itrig;
      temphist = new TH1F(name,name,2000,0.,20.);
      temphist->SetLineColor(itrig+1);
      hs_pt_all.push_back(temphist);

      name = "henergy_all_trig"; name += itrig;
      temphist = new TH1F(name,name,3000,0.,300.);
      temphist->SetLineColor(itrig+1);
      henergy_all.push_back(temphist);

      name = "hn_energy_all_trig"; name += itrig;
      temphist = new TH1F(name,name,3000,0.,300.);
      temphist->SetLineColor(itrig+1);
      hn_energy_all.push_back(temphist);

      name = "hs_energy_all_trig"; name += itrig;
      temphist = new TH1F(name,name,3000,0.,300.);
      temphist->SetLineColor(itrig+1);
      hs_energy_all.push_back(temphist);

      name = "hesum_trig"; name += itrig;
      temphist = new TH1F(name,name,1000,0.,2000.);
      temphist->SetLineColor(itrig+1);
      hesum.push_back(temphist);

      // energy in each tower for each trigger
      name = "h2_energy_trig"; name += itrig;
      temp2hist = new TH2F(name,name,36,-0.5,35.5,18,-0.5,17.5);
      h2_energy.push_back(temp2hist);

      // number of hits in each tower for each trigger
      name = "h2_nhit_trig"; name += itrig;
      temp2hist = new TH2F(name,name,36,-0.5,35.5,18,-0.5,17.5);
      h2_nhit.push_back(temp2hist);
    }

  orig_directory->cd();

  treefile = new TFile("mpctrig.root","RECREATE");
  ttree = new TTree("t","Mpc Trig Study Tree");
  ttree->Branch("run",&f_run,"run/i");
  ttree->Branch("evt",&f_evt,"evt/i");
  ttree->Branch("cross",&f_cross,"cross/I");
  ttree->Branch("trig",&f_trig,"trig/i");
  ttree->Branch("zvtx",&f_zvtx,"zvtx/F");

  orig_directory->cd();

  return 0;
}

int MpcTriggerStudy::process_event(PHCompositeNode *topNode)
{
  // Reset Tree Variables
  f_evt = 0;
  f_cross = -1;

  // informational message...
  static int ncalls = 0;

  if ( ncalls == 0 )
    {
       // Fill in prescales.
       unsigned int scaledown = 0;
       scaledown = trighelp->getLevel1Scaledown("BBCLL1(>0 tubes) novertex");
       //h_prescale->Fill(static_cast<double>(TRIG_MBIAS),static_cast<double>(scaledown));
       h_prescale->Fill(static_cast<double>(TRIG_MBIAS), scaledown);
       h_nfiles->Fill(static_cast<double>(TRIG_MBIAS), 1 );

       scaledown = trighelp->getLevel1Scaledown("MPC_A");
       h_prescale->Fill(static_cast<double>(TRIG_MPC_A), scaledown );
       h_nfiles->Fill(static_cast<double>(TRIG_MPC_A), 1 );

       scaledown = trighelp->getLevel1Scaledown("MPC_B");
       h_prescale->Fill(static_cast<double>(TRIG_MPC_B), scaledown );
       h_nfiles->Fill(static_cast<double>(TRIG_MPC_B), 1 );

       scaledown = trighelp->getLevel1Scaledown("MPC_C&ERT_2x2");	// run12
       h_prescale->Fill(static_cast<double>(TRIG_MPC_C_ERT2), scaledown );
       h_nfiles->Fill(static_cast<double>(TRIG_MPC_C_ERT2), 1 );

       scaledown = trighelp->getLevel1Scaledown("(MPCS_C & MPCS_C)||(MPCN_C & MPCN_C)");
       h_prescale->Fill(static_cast<double>(TRIG_MPC_C_C), scaledown );
       h_nfiles->Fill(static_cast<double>(TRIG_MPC_C_C), 1 );
    }

  ncalls++;

  if (ncalls % 100000 == 0 && verbosity)
    {
      cout << "MpcTriggerStudy Ncalls = " << ncalls << endl;
    }

  //  Get the data I need...
  mpcClusterContainer *mpcclus = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  mpcTowerContainer *mpctowers = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  mpcRawContainer *mpcraw = findNode::getClass<mpcRawContainer>(topNode,"MpcRaw");
  mpcSampleContainer *mpcsamples = findNode::getClass<mpcSampleContainer>(topNode,"mpcSampleContainer");
  SyncObject *sync = findNode::getClass<SyncObject>(topNode,"Sync");
  mpcmap = findNode::getClass<MpcMap>(topNode,"MpcMap");

  PHGlobal *phglobal = getClass<PHGlobal>(topNode, "PHGlobal");
  //VtxOut *vtxout = getClass<VtxOut>(topNode, "VtxOut");
  //BbcOut *bbcout = getClass<BbcOut>(topNode, "BbcOut");
  //EventHeader *evtheader = getClass<EventHeader>(topNode, "EventHeader");
  f_evt = sync->EventNumber();

  //if ( mpcclus==0 || mpctowers==0 || mpcraw==0 || evtheader==0 || bbcout==0 )
  if ( mpcclus==0 || mpctowers==0 || mpcraw==0 || phglobal==0 || sync==0 )
    {
      cout << "MpcTriggerStudy::process_event, mpcclus or eventheader or bbcout not found" << endl;
      cout << "\t" << (unsigned int)mpcclus
           << "\t" << (unsigned int)mpctowers
           << "\t" << (unsigned int)mpcraw
           << "\t" << (unsigned int)phglobal
           << "\t" << (unsigned int)sync
           << endl;
      return ABORTEVENT;
    }

  // get trigger information
  int trigger[MAXTRIG] = { 0 };	// whether trigger fired (scaled)
  int triglive[MAXTRIG] = { 0 };	// whether trigger fired (live)

  //trigger[TRIG_MBIAS] = trighelp->IsEventMinBias() ? 1 : 0;
  trigger[TRIG_MBIAS] = trighelp->trigScaled("BBCLL1(>0 tubes) novertex") ? 1 : 0;
  trigger[TRIG_MPC_A] = trighelp->trigScaled("MPC_A") ? 1 : 0;
  trigger[TRIG_MPC_B] = trighelp->trigScaled("MPC_B") ? 1 : 0;
  trigger[TRIG_MPC_C_ERT2] = trighelp->trigScaled("MPC_C&ERT_2x2") ? 1 : 0;
  trigger[TRIG_MPC_C_C] = trighelp->trigScaled("(MPCS_C & MPCS_C)||(MPCN_C & MPCN_C)") ? 1 : 0;

  triglive[TRIG_MBIAS] = trighelp->trigLive("BBCLL1(>0 tubes) novertex") ? 1 : 0;
  triglive[TRIG_MPC_A] = trighelp->trigLive("MPC_A") ? 1 : 0;
  triglive[TRIG_MPC_B] = trighelp->trigLive("MPC_B") ? 1 : 0;
  triglive[TRIG_MPC_C_ERT2] = trighelp->trigScaled("MPC_C&ERT_2x2") ? 1 : 0;
  triglive[TRIG_MPC_C_C] = trighelp->trigLive("(MPCS_C & MPCS_C)||(MPCN_C & MPCN_C)") ? 1 : 0;

  if ( trigger[TRIG_MPC_A] || trigger[TRIG_MPC_B] || trigger[TRIG_MPC_C_ERT2] || trigger[TRIG_MPC_C_C] )
    {
      trigger[TRIG_MPCANY] = 1;
    }

  if ( trigger[TRIG_MBIAS]==1 )
    {
      //if ( triglive[TRIG_MPCANY]==1 ) trigger[TRIG_MPCANY_BBC] = 1;
      if ( triglive[TRIG_MPC_A]==1 ) trigger[TRIG_MPC_A_BBC] = 1;
      if ( triglive[TRIG_MPC_B]==1 ) trigger[TRIG_MPC_B_BBC] = 1;
      if ( triglive[TRIG_MPC_C_ERT2]==1 ) trigger[TRIG_MPC_C_ERT2_BBC] = 1;
      if ( triglive[TRIG_MPC_C_C]==1 ) trigger[TRIG_MPC_C_C_BBC] = 1;
    }

  TrigLvl1 *triglvl1 = trighelp->get_trigLvl1();
  f_cross = triglvl1->get_lvl1_clock_cross();
  //unsigned int gl1_strig = triglvl1->get_lvl1_trigscaled();

  // whether the mpc rbit fired or not
  int rbits[MAX_RBITS] = { 0 };		// 
  unsigned int rbitmask[128] = {0};
  for (int ibit=64; ibit<96; ibit++)
    {
      rbitmask[ibit] = static_cast<unsigned int>( 0x1<<(ibit%32) );
    }
  unsigned int rbits_word = triglvl1->get_lvl1_rbits(2);
/*
  if ( (rbits_word&rbitmask[78]) == rbitmask[78] ) rbits[RBIT_A_N] = 1;
  if ( (rbits_word&rbitmask[78]) == rbitmask[78] ) rbits[RBIT_A_S] = 1;
  if ( (rbits_word&rbitmask[77]) == rbitmask[77] ) rbits[RBIT_B_N] = 1;
  if ( (rbits_word&rbitmask[77]) == rbitmask[77] ) rbits[RBIT_B_S] = 1;
*/

  // Use Trigger Emulator if rbits were merged
  int n[3], s[3], a[12], b[12], c[12];
  mpctrigemulator->CalcSumTrigger(mpcsamples);
  mpctrigemulator->TriggerDecision(n,s,a,b,c);
  if ( n[0]==1 ) rbits[RBIT_A_N] = 1;	// North A fired
  if ( s[0]==1 ) rbits[RBIT_A_S] = 1;	// South A fired
  if ( n[1]==1 ) rbits[RBIT_B_N] = 1;	// North B fired
  if ( s[1]==1 ) rbits[RBIT_B_S] = 1;	// South B fired
  // if neither fired, then we should just assume both fired...
  if ( n[0]==0 && s[0]==0 )
    {
      rbits[RBIT_A_N] = 1;
      rbits[RBIT_A_S] = 1;
    }
  if ( n[1]==0 && s[1]==0 )
    {
      rbits[RBIT_B_N] = 1;
      rbits[RBIT_B_S] = 1;
    }

  if ( (rbits_word&rbitmask[67]) == rbitmask[67] ) rbits[RBIT_C0_S] = 1;
  if ( (rbits_word&rbitmask[68]) == rbitmask[68] ) rbits[RBIT_C1_S] = 1;
  if ( (rbits_word&rbitmask[69]) == rbitmask[69] ) rbits[RBIT_C2_S] = 1;
  if ( (rbits_word&rbitmask[70]) == rbitmask[70] ) rbits[RBIT_C3_S] = 1;
  if ( (rbits_word&rbitmask[71]) == rbitmask[71] ) rbits[RBIT_C4_S] = 1;
  if ( (rbits_word&rbitmask[72]) == rbitmask[72] ) rbits[RBIT_C5_S] = 1;
  if ( (rbits_word&rbitmask[73]) == rbitmask[73] ) rbits[RBIT_C0_N] = 1;
  if ( (rbits_word&rbitmask[75]) == rbitmask[75] ) rbits[RBIT_C1_N] = 1;
  if ( (rbits_word&rbitmask[90]) == rbitmask[90] ) rbits[RBIT_C2_N] = 1;
  if ( (rbits_word&rbitmask[91]) == rbitmask[91] ) rbits[RBIT_C3_N] = 1;
  if ( (rbits_word&rbitmask[94]) == rbitmask[94] ) rbits[RBIT_C4_N] = 1;
  if ( (rbits_word&rbitmask[95]) == rbitmask[95] ) rbits[RBIT_C5_N] = 1;

  // Kludge Definitions, due to bad rbit
  //if ( (rbits_word&rbitmask[74]) == rbitmask[74] ) rbits[RBIT_A_S] = 1;
  //if ( (rbits_word&rbitmask[85]) == rbitmask[85] ) rbits[RBIT_A_N] = 1;

  for (int itrig=0; itrig<MAXTRIG; itrig++)
    {
      if ( trigger[itrig] )
        {
          h_ntrig->Fill(itrig);

          // fill crossing distributions
          h_cross[itrig]->Fill( f_cross );
          if ( itrig==TRIG_MPC_A && rbits[RBIT_A_N]==1 ) h_ncross[itrig]->Fill( f_cross );
          if ( itrig==TRIG_MPC_A && rbits[RBIT_A_S]==1 ) h_scross[itrig]->Fill( f_cross );
          if ( itrig==TRIG_MPC_B && rbits[RBIT_B_N]==1 ) h_ncross[itrig]->Fill( f_cross );
          if ( itrig==TRIG_MPC_B && rbits[RBIT_B_S]==1 ) h_scross[itrig]->Fill( f_cross );
          //if ( itrig==TRIG_MPC_C_ERT2 && rbits[RBIT_C_N]==1 ) h_ncross[itrig]->Fill( f_cross );

          // fill rbits distributions (lvl1_rbits is 5 words long)
          int rbit = 0;
          for (int iword=0; iword<5; iword++)
            {
              unsigned int rbits_word = triglvl1->get_lvl1_rbits(iword);
              unsigned int rbit_mask = 0;
              for (int ibit=0; ibit<32; ibit++)
                {
                  rbit_mask = static_cast<unsigned int>(0x1<<ibit);
                  if ( (rbits_word&rbit_mask)==rbit_mask )
                    {
                      h_rbits[itrig]->Fill( rbit );
                    }

                  ++rbit;
                }
            }

          for (int itrig2=0; itrig2<MAXTRIG; itrig2++)
            {
              if (trigger[itrig2]) h2_ntrig->Fill(itrig,itrig2);
            }
        }

    }

  // Figure Out Which Triggers Fired
  f_trig = 0;
  for (unsigned int itrig=0; itrig<trig_list.size(); itrig++)
    {
      if ( trighelp->trigScaled( trig_list[itrig] ) )
        {
          f_trig |= (0x1 << itrig);
        }
    }

  f_zvtx = phglobal->getBbcZVertex();
  //f_zvtx = bbcout->get_VertexPoint();

  unsigned int nclus = mpcclus->size();
  //cout << "nclus " << nclus << endl;

  // fill 2d energy grid
  h2_ecent[0]->Reset();
  h2_ecent[1]->Reset();
  for (unsigned int itow = 0; itow < mpctowers->size(); itow++)
    {
      mpcTowerContent *tower = mpctowers->getTower( itow );
      Short_t fee576_ch = tower->get_ch();
      if ( mpcmap->isCrystal( fee576_ch ) != 1 ) continue;      // skip if not a crystal

      // Skip noisy driver board
      if ( mpcmap->getDriver( fee576_ch ) == 19 ) continue;

      int arm = mpcmap->getArm( fee576_ch );
      int ix  = mpcmap->getGridX( fee576_ch );
      int iy  = mpcmap->getGridY( fee576_ch );

      float e  = tower->get_energy( fee576_ch );

      h2_ecent[arm]->SetBinContent( ix+1, iy+1, e );
    }
  //h2_ecent[0]->Print("ALL");

/*
  int nsamples = mpcsamples->size();
  for (int isamp=0; isamp<nsamples; isamp++)
    {
      mpcSample *adcsamp = mpcsamples->GetSample(isamp);
      if ( adcsamp==0 ) continue;
 
      int feech = adcsamp->get_ch();     // fee576ch
      int samp = adcsamp->get_sample();  // sample number

      if ( mpcmap->isCrystal(feech)==0 ) continue;

      if ( feech<0||feech>575||samp<0||samp>11 )
        {
          cout << PHWHERE << "ERROR, feech samp out of bounds "
               << feech << "\t" << samp << endl;
          continue;
        }

      //for (int impctrig=0; impctrig<mpctrig_list.size(); impctrig++)
      for (int impctrig=0; impctrig<1; impctrig++)
        {
          int itrig = mpc_triglist[impctrig];
          if ( trigger[itrig]==1 && rbits[RBIT_A_S]==1 )
            {
              int arm = 0;
              for (int idelay=0; idelay<12; idelay++)
                {
                  h2_adc[arm][itrig][0]->Fill(feech,adc);
                }
            }
        }
    }
*/

  // Now fill 3x3 sum, for the veto
  h2_e9[0]->Reset();
  h2_e9[1]->Reset();
  for (int iarm = 0; iarm < 2; iarm++)
    {
      for (int ix = 0; ix < 18; ix++)
        {
          for (int iy = 0; iy < 18; iy++)
            {
              double e9 = 0.;
              for (int ixsum=ix; ixsum<=ix+2; ixsum++)
                {
                  for (int iysum=iy; iysum<=iy+2; iysum++)
                    {
                      e9 += h2_ecent[iarm]->GetBinContent(ixsum,iysum);
                      //cout << "e9sum " << ixsum << "\t" << iysum << "\t"
                      //     << h2_ecent[iarm]->GetBinContent(ixsum,iysum) << "\t" << e9 << endl;
                    }
                }
              h2_e9[iarm]->SetBinContent( ix+1, iy+1, e9 );
            }
        }
    }

  float mpc_esum[MAXTRIG] = {0.};

  for (unsigned int iclus=0; iclus<nclus; iclus++)
    {
      mpcClusterContent *clus = mpcclus->getCluster(iclus);

      int ixpos = clus->ixpos();
      int iypos = clus->iypos();
      int arm = clus->arm();

      // cut out non-existing towers
      if ( ixpos<0 ) continue;

      float mpc_ecore = clus->ecore();
      //cout << "ecore " << arm << "\t" << ixpos << "\t" << iypos << "\t" << mpc_ecore << endl;

      // make a minimum and maximum energy cut
      if ( mpc_ecore<0.5 || mpc_ecore>500. ) continue;

      float mpc_x = clus->x();
      float mpc_y = clus->y();
      float mpc_z = clus->z() - f_zvtx;
      float mpc_r = sqrt(mpc_x*mpc_x+mpc_y*mpc_y);

      int fee576ch = mpcmap->getFeeCh(ixpos,iypos,arm);

/*
      // Cut out hot towers, driver boards
      int driver = mpcmap->getDriver(fee576ch);
      if ( driver == 19 )
        {
          continue;
        }
*/

      // Cut out single tower background
      //float mye9 = 0.;
      //float myecent = 0.;
      //float myerat = 0.;
      float e9 = clus->e9();
      float ecent = clus->ecent();
      //cout << "XXX\t" << arm << "\t" << ixpos << "\t" << iypos << "\t" << mpc_ecore << "\t" << e9 << "\t" << ecent << endl;
      //IsSingleTower( fee576ch, mye9, myecent, myerat );

      // Make Dispersion Cut
      float mpc_dispx = clus->dispx();
      float mpc_dispy = clus->dispy();
      float mpc_corrdispx = clus->corrdispx();
      float mpc_corrdispy = clus->corrdispy();
      h_dispx->Fill( mpc_dispx );
      h_dispy->Fill( mpc_dispy );
      h_corrdispx->Fill( mpc_corrdispx );
      h_corrdispy->Fill( mpc_corrdispy );
      h_e9rat[arm]->Fill( ecent/e9 );
      h_corrdispy->Fill( mpc_corrdispy );

/*
      if ( mpc_corrdispx>4.0 || mpc_corrdispy>4.0 )
        {
          continue;
        }
*/

      int ishighptnoise = 0;

      if ( e9>0. )
        {
          if ( ecent/e9 > 0.95 )
            {
              ishighptnoise = 1;
            }
        }

      if ( mpc_corrdispx<0.001 && mpc_corrdispy<0.001 )
        {
          ishighptnoise = 1;
        }

      if ( mpc_r<11.0 || mpc_r>19.0 )
        {
          ishighptnoise = 1;
        }

/*
if ( mpc_ecore>100. )
{
  cout << "e " << mpc_ecore << "\t" << driver << "\t" << arm << "\t" << ixpos << "\t" << iypos
       << "\t" << mpc_dispx << "\t" << mpc_corrdispx
       << "\t" << mpc_dispy << "\t" << mpc_corrdispy << endl;
  cout << "\t" << e9 << "\t" << mye9-e9 << "\t" << ecent << "\t" << myecent-ecent << "\t" << ecent/e9 << "\t" << myerat << endl;
}
*/

      for (int itrig=0; itrig<MAXTRIG; itrig++)
        {
//if ( itrig==TRIG_MPC_B ) cout << "itrig " << itrig << " " << trigger[itrig] << endl;
          if ( trigger[itrig]!=0 )
            {
              henergy_all[itrig]->Fill( mpc_ecore );
              if ( ishighptnoise==0 ) henergy[itrig]->Fill( mpc_ecore );

              int offset = 0;		// x offset in grid for north arm
              if ( fee576ch>287 ) offset = 18;
              h2_energy[itrig]->Fill( ixpos + offset, iypos, mpc_ecore );

              h2_nhit[itrig]->Fill( ixpos + offset, iypos );

              float mpc_pt = mpc_ecore*fabs(mpc_r/mpc_z);
              hpt_all[itrig]->Fill( mpc_pt );
              if ( ishighptnoise==0 ) hpt[itrig]->Fill( mpc_pt );

              if ( fee576ch>=0 && fee576ch<288 )
                {
                  if ( itrig==TRIG_MPC_A )
                    {
                      //if ( rbits[RBIT_A_S]==1 )
                        {
                          hs_energy_all[itrig]->Fill( mpc_ecore );
                          hs_pt_all[itrig]->Fill( mpc_pt );
                          if ( ishighptnoise==0 )
                            {
                              hs_energy[itrig]->Fill( mpc_ecore );
                              hs_pt[itrig]->Fill( mpc_pt );
                            }
                        }
                    }
                  else if ( itrig==TRIG_MPC_B )
                    {
                      //if ( rbits[RBIT_B_S]==1 )
                        {
                          hs_energy_all[itrig]->Fill( mpc_ecore );
                          hs_pt_all[itrig]->Fill( mpc_pt );
                          if ( ishighptnoise==0 )
                            {
                              hs_energy[itrig]->Fill( mpc_ecore );
                              hs_pt[itrig]->Fill( mpc_pt );
                            }
                        }
                    }
                  else
                    {
                      hs_energy_all[itrig]->Fill( mpc_ecore );
                      hs_pt_all[itrig]->Fill( mpc_pt );
                      if ( ishighptnoise==0 )
                        {
                          hs_energy[itrig]->Fill( mpc_ecore );
                          hs_pt[itrig]->Fill( mpc_pt );
                        }
                    }
                }
              else if ( fee576ch>=288 && fee576ch<576 )
                {
                  if ( itrig==TRIG_MPC_A )
                    {
                      //if ( rbits[RBIT_A_N]==1 )
                        {
                          hn_energy_all[itrig]->Fill( mpc_ecore );
                          hn_pt_all[itrig]->Fill( mpc_pt );
                          if ( ishighptnoise==0 )
                            {
                              hn_energy[itrig]->Fill( mpc_ecore );
                              hn_pt[itrig]->Fill( mpc_pt );
                            }
                        }
                    }
                  else if ( itrig==TRIG_MPC_B )
                    {
                      //if ( rbits[RBIT_B_N]==1 )
                        {
                          hn_energy_all[itrig]->Fill( mpc_ecore );
                          hn_pt_all[itrig]->Fill( mpc_pt );
                          if ( ishighptnoise==0 )
                            {
                              hn_energy[itrig]->Fill( mpc_ecore );
                              hn_pt[itrig]->Fill( mpc_pt );
                            }
                        }
                    }
                  else if ( itrig==TRIG_MPC_C_ERT2 )
                    {
                      //if ( rbits[RBIT_C_N]==1 )
                      if ( 1 )
                        {
                          hn_energy_all[itrig]->Fill( mpc_ecore );
                          hn_pt_all[itrig]->Fill( mpc_pt );
                          if ( ishighptnoise==0 )
                            {
                              hn_energy[itrig]->Fill( mpc_ecore );
                              hn_pt[itrig]->Fill( mpc_pt );
                            }
                        }
                    }
                  else
                    {
                      hn_energy_all[itrig]->Fill( mpc_ecore );
                      hn_pt_all[itrig]->Fill( mpc_pt );
                      if ( ishighptnoise==0 )
                        {
                          hn_energy[itrig]->Fill( mpc_ecore );
                          hn_pt[itrig]->Fill( mpc_pt );
                        }
                    }
                }

              hch_energy[itrig][fee576ch]->Fill( mpc_ecore );
              hch_pt[itrig][fee576ch]->Fill( mpc_pt );
            }
        }
    }

  // Get Tile distributions
  //Fill_Tile_Arrays(mpcraw);

  for (int itrig=0; itrig<MAXTRIG; itrig++)
    {
      if ( trigger[itrig]!=0 )
        {
          mpc_esum[itrig] = mpctowers->get_esum();

          hesum[itrig]->Fill( mpc_esum[itrig] );

          // sanity check on the calibrations
/*
// need to put this back in?
          if ( itrig==TRIG_MPC4X4 && mpc_esum[itrig]<1 ) 
            {
              cout << "triggered but low esum " << trigger[itrig] << "\t" << mpc_esum[itrig]
                   << "\t" << nclus << "\t" << mpctowers->size() << endl;
            }
*/

        }
    }

  ttree->Fill();

  // any other return code might lead to aborting the event or analysis for everyone
  return EVENT_OK;
}

int MpcTriggerStudy::End(PHCompositeNode *topNode)
{
  Int_t nbinsx = h_ntrig->GetNbinsX();
  for (int itrig=1; itrig<=nbinsx; itrig++)
    {
      Float_t nevents = h_ntrig->GetBinContent(itrig);
      Float_t prescale = h_prescale->GetBinContent(itrig) + 1;
      h_ntrigcorr->SetBinContent(itrig,nevents*prescale);
    }
  //HistoManager->dumpHistos(OutFileName.c_str());

  savefile->Write();
  savefile->Close();

  treefile->Write();
  treefile->Close();

  if ( trighelp!=0 ) delete trighelp;

  return 0;
}

int MpcTriggerStudy::Fill_Tile_Arrays(mpcRawContainer *mraw)
{
  // Reset the arrays
  for (int ix=0; ix<10; ix++)
    {
      for (int iy=0; iy<10; iy++)
        {
          sums_2x2[0][ix][iy] = 0.;
          sums_2x2[1][ix][iy] = 0.;
        }
    }
  for (int ix=0; ix<9; ix++)
    {
      for (int iy=0; iy<9; iy++)
        {
          sums_4x4[0][ix][iy] = 0.;
          sums_4x4[1][ix][iy] = 0.;
        }
    }

  // reset the max tile
  max2x2 = 0.;
  max2x2n = 0.;
  max2x2s = 0.;
  max4x4 = 0.;
  max4x4n = 0.;
  max4x4s = 0.;

  max2x2n_xpos = -1;
  max2x2n_ypos = -1;
  max2x2s_xpos = -1;
  max2x2s_ypos = -1;
  max4x4n_xpos = -1;
  max4x4n_ypos = -1;
  max4x4s_xpos = -1;
  max4x4s_ypos = -1;

  // Note that these sums assume a simple geometry mapping
  // of the channels.  However, there are a few exceptions...
  int ntowers = mraw->size();
  for (int itow=0; itow<ntowers; itow++)
    {
      mpcRawContent *mraw_tower = mraw->getTower(itow);

      int fee576ch = mraw_tower->get_ch();
      int gridx = mpcmap->getGridX( fee576ch );
      if ( mpcmap->getGridX( fee576ch ) < 0 ) continue;
      int gridy = mpcmap->getGridY( fee576ch );

      int lopost = mraw_tower->get_lopost();
      int lopre = mraw_tower->get_lopre();
      //int adc = lopost - lopre;

      // the 2x2 tiles form a 10x10 grid
      int xcol = static_cast<int>((gridx+1)/2);
      int yrow = static_cast<int>((gridy+1)/2);

      int arm = 0;
      if ( fee576ch>=288 ) arm = 1;

      assert( arm==0 || arm==1 );
/*
      if ( ch2x2<0 || ch2x2>=200 )
        {
          cerr << PHWHERE << " ERROR, illegal ch2x2 " << ch2x2 << endl;
          continue;
        }
*/

      sums_2x2[arm][xcol][yrow] += (lopost-lopre);
    }

  // south 4x4 tiles
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ix=0; ix<9; ix++)
        {
          for (int iy=0; iy<9; iy++)
            {
              sums_4x4[iarm][ix][iy] = sums_2x2[iarm][ix][iy] + sums_2x2[iarm][ix+1][iy]
                                     + sums_2x2[iarm][ix][iy+1] + sums_2x2[iarm][ix+1][iy+1];
            }
        }
    }

  // get maximum 2x2
  for (int ix=0; ix<10; ix++)
    {
      for (int iy=0; iy<10; iy++)
        {
          if ( sums_2x2[0][ix][iy] > max2x2s )
            {
              max2x2s = sums_2x2[0][ix][iy];
              max2x2s_xpos = ix;
              max2x2s_ypos = iy;
            }
          if ( sums_2x2[1][ix][iy] > max2x2n )
            {
              max2x2n = sums_2x2[1][ix][iy];
              max2x2n_xpos = ix;
              max2x2n_ypos = iy;
            }

          if ( sums_2x2[0][ix][iy] > max2x2 ) max2x2 = sums_2x2[0][ix][iy];
          if ( sums_2x2[1][ix][iy] > max2x2 ) max2x2 = sums_2x2[1][ix][iy];
        }
    }

  // get maximum 4x4
  for (int ix=0; ix<9; ix++)
    {
      for (int iy=0; iy<9; iy++)
        {
          if ( sums_4x4[0][ix][iy] > max4x4s )
            {
              max4x4s = sums_4x4[0][ix][iy];
              max4x4s_xpos = ix;
              max4x4s_ypos = iy;
            }
          if ( sums_4x4[1][ix][iy] > max4x4n )
            {
              max4x4n = sums_4x4[1][ix][iy];
              max4x4n_xpos = ix;
              max4x4n_ypos = iy;
            }

          if ( sums_4x4[0][ix][iy] > max4x4 ) max4x4 = sums_4x4[0][ix][iy];
          if ( sums_4x4[1][ix][iy] > max4x4 ) max4x4 = sums_4x4[1][ix][iy];
        }
    }

  return 0;
}

// Single Tower Background Cut
int MpcTriggerStudy::IsSingleTower(const int fee576_ch, float& e9, float& ecent, float& erat)
{
  int arm = mpcmap->getArm( fee576_ch );
  int ix  = mpcmap->getGridX( fee576_ch );
  int iy  = mpcmap->getGridY( fee576_ch );

  e9 = (float)h2_e9[arm]->GetBinContent(ix+1,iy+1);
  ecent = (float)h2_ecent[arm]->GetBinContent(ix+1,iy+1);

/*
  cout << "bad " << arm << "\t" << ix << "\t" << iy << "\t" << e9 << "\t" << ecent << endl;

  if ( e9 <= 0. ) { cout << "e9 bad " << e9 << endl; return 1; }
  if ( ecent <= 0. ) { cout << "ecent bad " << ecent << endl; return 1; }
*/

  erat = (e9-ecent)/ecent;
  if ( e9>20. )
    {
      h_erat[arm]->Fill( erat );
    }

  //if ( e9ratio > 0.95 ) return 1;
  if ( erat < 0.14 ) return 1;

  return 0;
}

