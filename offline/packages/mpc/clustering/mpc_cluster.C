//#include <libgen.h>

void mpc_cluster(int runnumber = 205249, const int nevents = 0, const int show_eventdisplay = 0)
{
  gSystem->Load("libfun4allfuncs.so");	// framework + reco modules
  gSystem->Load("libmpc.so");

  /////////////////////////////////////////////////////////////////
  //  Server...
  Fun4AllServer *se = Fun4AllServer::instance();

  recoConsts *rc = recoConsts::instance();

  /////////////////////////////////////////////////////////////////
  //  Reconstruction Modules...
  
  // Now not needed since phglobal is in the ndst's
  //SubsysReco *global  = new GlobalReco();
  //se->registerSubsystem(global);

  SubsysReco *mpc_cluster = new MpcReco("mpc_cluster.root");

  //rc->set_IntFlag("MPC_CLUSTER_ALG",1);		// Use Andrey's Clustering
  rc->set_IntFlag("MPC_RECO_MODE",0x6);
  rc->set_IntFlag("MPC_EVENT_DISPLAY",0);
  rc->set_IntFlag("MPC_VERBOSITY",0);

  TCanvas *evtdisplay = 0;
  if ( show_eventdisplay )
    {
      evtdisplay = new TCanvas("evtdisplay","Event Display",425,425);
      gStyle->SetOptStat(0);
      rc->set_IntFlag("MPC_RECO_MODE",0x6);
      rc->set_IntFlag("MPC_EVENT_DISPLAY",1);
      rc->set_IntFlag("MPC_VERBOSITY",1);
    }

  se->registerSubsystem(mpc_cluster);

  /////////////////////////////////////////////////////////////////
  //  Input Managers...
  Fun4AllDstInputManager *in1 = new Fun4AllDstInputManager("DSTin1", "DST");
  se->registerInputManager(in1);
  Fun4AllDstInputManager *in2 = new Fun4AllDstInputManager("DSTin2", "DST");
  if ( runnumber>10000 )
    {
      se->registerInputManager(in2);
    }

  TString OUTPUT = "/phenix/data61/chiu/mpc_62transverse/CLUS_MPC/MPC_"; OUTPUT += runnumber; OUTPUT += ".root";
  Fun4AllDstOutputManager *dst_output_mgr  = new Fun4AllDstOutputManager("MPC",OUTPUT.Data());
  dst_output_mgr->AddNode("EventHeader");
  dst_output_mgr->AddNode("Sync");
  dst_output_mgr->AddNode("TrigLvl1");
  dst_output_mgr->AddNode("PreviousEvent");
  dst_output_mgr->AddNode("PHGlobal");
  dst_output_mgr->AddNode("mpcClusterContainer");
  se->registerOutputManager(dst_output_mgr);

/*
  TString TOWER_OUTPUT = "/phenix/data61/chiu/mpc_62transverse/CLUS_MPC/MPCTower_";
  TOWER_OUTPUT += runnumber;
  TOWER_OUTPUT += ".root";
  Fun4AllDstOutputManager *tower_output_mgr  = new Fun4AllDstOutputManager("MpcTower",TOWER_OUTPUT.Data());
  tower_output_mgr->AddNode("EventHeader");
  tower_output_mgr->AddNode("Sync");
  tower_output_mgr->AddNode("TrigLvl1");
  tower_output_mgr->AddNode("PreviousEvent");
  tower_output_mgr->AddNode("PHGlobal");
  tower_output_mgr->AddNode("mpcTowerContainer");
  se->registerOutputManager(tower_output_mgr);
*/

  ////////////////////////////////////////////////////////////////
  //  OK, now loop over all the input files...

  // First open up the mpc dst files
  TString filelist1name = "/phenix/u/chiu/runlists/run06_62_transverse/";
  filelist1name += runnumber; filelist1name += ".list";
  string flist1name = filelist1name.Data();
  in1->AddListFile( flist1name ); // load the filelist into the Input Manager

  if ( runnumber>10000 )
    {
      TString filelist2name = "/phenix/u/chiu/runlists/run06_62_transverse/ewg";
      filelist2name += runnumber; filelist2name += ".list";
      string flist2name = filelist2name.Data();
      in2->AddListFile( flist2name ); // load the filelist into the Input Manager
    }

  se->run(nevents);  // run over all events
  se->End();
}

