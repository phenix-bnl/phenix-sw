#include <string>
#include <sstream>
#include <iostream>
#include <stdio.h>
using namespace std;

void Fun4Muons( int nEvents = 10, const char * inputfile = "EVENTDATA_P00-0000366586-0003.PRDFF")
{
  // Tell root to not to start signal handling but crash
  for (int i = 0; i < kMAXSIGNALS; i++)
    {
      gSystem->IgnoreSignal((ESignals)i);
    }

  gSystem->Exec("/bin/env");

  std::cout << "Processing file: " << inputfile << std::endl;

  // set the production name
  std::string prodtag(gSystem->Getenv("PRODTAG"));
  std::string launch(gSystem->Getenv("PRODROOT"));
  launch = launch + "/launch";

  char ifile[strlen(inputfile)+1]; // + 1 for the \0 which marks the end of string
  strcpy(ifile, inputfile);
  strtok(ifile, "-");
  int runnumber = atoi(strtok(0, "-"));
  int segnumber = atoi(strtok(strtok(0, "-"), "."));
  //cout << "runnumber: " <<   runnumber << " segment " << segnumber << endl;

  // Loading libraries
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs.so");
  gSystem->Load("libMWGOO");
  gSystem->Load("libmutrg");
  gSystem->Load("libnanoDST"); // 2013 for pDST output
  gSystem->Load("libfvtx_subsysreco");
  gSystem->Load("librpc_subsysreco" );
  gSystem->Load("librpc_muotrackreco" );
  gSystem->Load("libMWGpico" ); // 2013 pDST output
  gSystem->Load("libdstqa_muons.so");
  gSystem->Load("libpicodst_object.so");

  //IO manager
  gROOT->ProcessLine(".L OutputManager.C");
  gROOT->ProcessLine(".L rawdatacheck.C");

  SetCvsTag();

  Fun4AllServer* se = Fun4AllServer::instance();
  se->Verbosity(0);

  // get pointer to raw data checker to pass to reco modules
  RawDataCheck *raw = rawdatacheck();

  HeadReco *head = new HeadReco();
  // add the rawdatacheck pointer so a list of
  // bad packets is added to the EventHeader
  head->SetRawDataCheck(raw);

  SubsysReco *sync = new SyncReco();

  // RecoConsts setup
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS", 0);

  //************
  rc->set_IntFlag( "MUTOO_ERRORSTATS", 0 ); // to stop TMutErrorStats
  // use # of strips with hard-coded number
  MutrgPar::IS_READ_NSTRIP_DB = true;
  rc->set_IntFlag( "RpcGeomType", 3 );//NEEDED for run 12+ RPC geometry...
  //***********

  rc->Print();

  // Print database readouts
  // May have to use local dead channel file .. don't know yet?
  TMutDatabaseCntrl::print();

  //muon timeserver instance
  PHTimeServer* timeserv = PHTimeServer::get();

  //Define and configure all subsystems
  SubsysReco *trig = new TrigReco();
  SubsysReco *peve = new PreviousEventReco();
  BbcReco *bbc     = new BbcReco();
  bbc->setBbcVtxError( 1.0 );

  // BBC multi needed for FVTX multivertex finder
  BbcMultipleVtxReco * bbc_multi = new BbcMultipleVtxReco() ;

  SubsysReco *zdc  = new ZdcReco();
  SubsysReco *t0   = new T0Reco();
  SubsysReco *vtx  = new VtxReco();

  //////////////////////////////////
  // Accounting
  //////////////////////////////////
  SubsysReco *trigacc = new TriggerAccounting();
  SubsysReco *outacc  = new OutputAccounting();

  SubsysReco *unpack = new MuonUnpackPRDF();
  SubsysReco *muioo  = new MuiooReco(); // 20Feb2013 notice it will be added to Fun4All server after FVTX
  SubsysReco *mutoo  = new MuonDev();

  // FVTX reconstruction
  FvtxUnpackPRDF *Fvtx_unpack = new FvtxUnpackPRDF();// 20Feb2013
  FvtxReco* fvtxreco = new FvtxReco();               // 20Feb2013
  fvtxreco->set_use_svx_cluster(true);               // 20Feb2013
  fvtxreco->set_finder(1);                           // 20Feb2013 1:Columbia's tracking, 2:Fisrt tracking code
  fvtxreco->set_do_mutr_matching(false);             // 20Feb2013 matching will be performed after

  // FVTX primary vertex determination.
  FvtxPrimVertex* fvtxprimvtx = new FvtxPrimVertex(); // 20Feb2013
  fvtxprimvtx->set_fvtx_Rres(0.5);                    // 20Feb2013 crossing window (0.1 for CuAu, 0.5 for pp)
  fvtxprimvtx->set_source( FvtxPrimVertex::Coordinate ); // 20Feb2013 or FvtxPrimVertex::Tracks
  fvtxprimvtx->set_bbcz_window(2.0); // 20Feb 2013 FVTX-BBC vertex matching window

  // FVTX-MuTr module must be registered after muon reconstruction
  FvtxRecoWithMut * fvtxrecowithmut = new FvtxRecoWithMut();

  // in 16 feb 2013
  SubsysReco *mwg = new MWGFvtxReco( new MWGInclusiveNanoCutsv2() );//muon nDST module

  // module which counts tracklets and clusters withing 8 cone ranges
  // 20Feb2013 undefined argument
  // (Need to run after MWGFvtxReco)
  // eval_file arg replaced in FvtxConeTracklets
  FvtxConeTracklets* fvtxcone = new FvtxConeTracklets(); // 20Feb2013

  // out 16 feb 2013
  // SubsysReco *mwg = new MWGOOReco( new MWGInclusiveNanoCutsv2() );//muon nDST module

  // store trk ass adc 4 sample info (for hits in track ass clus) (what is an ASS clus?)
  SubsysReco* ad_adc = new PHMuoTrackAdcReco();

  MuidEffic *muid_effic = new MuidEffic(); // muid efficiency module
  muid_effic->set_filename(muid_eff_IOManager(runnumber, segnumber));

  // mutrg unpacker & reconstruction -- from Yoshi
  MutrgReco* mutrgreco = new MutrgReco();
  mutrgreco->DoClustering(true, 4);

  // rpc unpacker
  SubsysReco* unpackrpc = new RpcUnpackPRDF();
  SubsysReco* unpackrpchodo = new RpcHodoUnpackPRDF();

  // rpc reconstruction
  SubsysReco* rpcreco = new RpcReco();

  SubsysReco *global = new GlobalReco();
  SubsysReco *global_muons = new GlobalReco_muons();
  SubsysReco *rpcmuoreco = new RpcMuoReco();


  se->registerSubsystem(head);
  se->registerSubsystem(sync);
  se->registerSubsystem(trig);

  se->registerSubsystem(trigacc);
  se->registerSubsystem(outacc);

  se->registerSubsystem(peve);
  se->registerSubsystem(bbc);
  se->registerSubsystem( bbc_multi );
  se->registerSubsystem(zdc);
  se->registerSubsystem(t0);
  se->registerSubsystem(vtx);

  se->registerSubsystem(unpack);

  // FVTX
  se->registerSubsystem(Fvtx_unpack);
  se->registerSubsystem(fvtxreco);
  se->registerSubsystem(fvtxprimvtx);

  //20Feb2013 - Notice change in order w.r.t. FVTX
  se->registerSubsystem(muioo);
  se->registerSubsystem(mutoo);

  se->registerSubsystem( fvtxrecowithmut); // 20Feb2013

  se->registerSubsystem(mwg);
  se->registerSubsystem(fvtxcone); // 20Feb2013

  se->registerSubsystem(ad_adc);
  se->registerSubsystem(muid_effic);
  se->registerSubsystem(mutrgreco);

  // RPC
  se->registerSubsystem(unpackrpc);
  se->registerSubsystem(unpackrpchodo);
  se->registerSubsystem(rpcreco);

  se->registerSubsystem(global);
  se->registerSubsystem(global_muons);
  se->registerSubsystem(rpcmuoreco);

  ///////fvtx QA & GOLDENEVENT FILTERING
  se->registerSubsystem(new QAMut());
  se->registerSubsystem( new QAFvtx() );

  ///// fvtx golden prdf filtering
  // //Golden FVTX PRDFs
  //MuonTrigFilter * filter_fvtx = new MuonTrigFilter("GOLD_FVTX_EVTS",MuonTrigFilter::FVTX, MuonTrigFilter::DISCARD_EVENT);
  //filter_fvtx->set_z_vertex_window(-15, 15);
  //se->registerSubsystem(filter_fvtx);

  //PRDF_IOManager(runnumber,segnumber,"GOLDENEVENT_FVTX","GOLD_FVTX_EVTS");

  //Golden dimuon PRDFs
  MuonTrigFilter * filter_dimuon = new MuonTrigFilter("GOLD_DIMUON_EVTS", MuonTrigFilter::RECO_DIMUON, MuonTrigFilter::DISCARD_EVENT);
  filter_dimuon->set_mass_min(1.0);
  se->registerSubsystem(filter_dimuon);

  PRDF_IOManager(runnumber, segnumber, "GOLDENEVENT_DIMUON", "GOLD_DIMUON_EVTS");

  ////////// end Golden PRDF fitlering

  ////////////////////////
  // pico DST output
  std::string pDSTOutputFilename(MakeOutput(runnumber, segnumber, "pDST"));

  MWGpico * picoDST = new MWGpico();
  picoDST->set_run_type(MWGpico::RUN12);
  picoDST->set_rpcnotracks_flag(true);
  picoDST->MakePico( MWGpico::DIMUONSOO, pDSTOutputFilename.c_str());
  picoDST->MakePico( MWGpico::NEWSNGMUONS, pDSTOutputFilename.c_str() );
  picoDST->InitCuts( "nocuts" );
  se->registerSubsystem( picoDST );
  // Done with pico DST output setup.

  //---------------------------------------------- Xiaorong fvtx sigleMuonContainer---------------------------------------------
  //add sngmuonscontainer //fvtx
  std::string smpDSTOutputFilename = MakeOutput(runnumber, segnumber, "smpDST");
  std::string dmpDSTOutputFilename = MakeOutput(runnumber, segnumber, "dmpDST");
  mFillSingleMuonContainer* msngl = new mFillSingleMuonContainer();
  se->registerSubsystem(msngl);
  msngl->set_bbcz_cut(50);
  msngl->set_pz_cut(2.0);

  Fun4AllOutputManager *outsmu = new Fun4AllDstOutputManager("Outsmu", smpDSTOutputFilename.c_str());
  outsmu->AddNode("Sync");
  outsmu->AddNode("SingleMuonContainer");
  outsmu->AddNode("TrigLvl1");
  //outsmu->AddNode("PHMuoTracksOO"); // tmp entry for debugging
  outsmu->AddEventSelector("mFillSingleMuonContainer");
  se->registerOutputManager(outsmu);

  //--------------------------------------------- on 15th feb-------------------------------------------------------------
  // New Dimuon Output
  mFillDiMuonContainer* mdi = new mFillDiMuonContainer(false); // do not make mixed events
  se->registerSubsystem(mdi);
  mdi->set_mass_cut(0.5);
  mdi->set_is_sim(true);

  Fun4AllOutputManager *outdimu = new Fun4AllDstOutputManager("Outdimu", dmpDSTOutputFilename.c_str());
  outdimu->AddNode("Sync");
  outdimu->AddNode("DiMuonContainer");
  outsmu->AddNode("VtxOut");
  outsmu->AddNode("PHGlobal");
  outsmu->AddNode("PHPythiaHeader");
  outsmu->AddNode("PHPythia");
  outdimu->AddEventSelector("mFillDiMuonContainer");
  se->registerOutputManager(outdimu);
  // End new Dimuon Output

  Fun4AllInputManager *in = new Fun4AllPrdfInputManager("PRDFin");
  in->fileopen(inputfile);
  se->registerInputManager(in);

  // run the profiler if its flags are set
  // typically it is 
  // setenv JPROF_FLAGS "JP_START JP_PERIOD=0.005"
  // after running create html with 
  // jprof root jprof-log > myProfile.html
  if (gSystem->Getenv("JPROF_FLAGS"))
    {
      cout << "JPROF_FLAGS Env var set, adding profiler" << endl;
      gSystem->Load("libjprof.so");
      prof *Pr = new prof;
    }


  se->run(nEvents);

  QA_IOManager(runnumber, segnumber);

  se->End();


  int evts = se->PrdfEvents();
  std::cout << "Total Events:  " << evts << std::endl;

  //////////////////////////////////
  //Cleanup our toys and go home.
  //////////////////////////////////

  ostringstream sqls;
  std::string sql;
  sqls << launch << "/recoDB_update.pl ";
  sqls << "\" nevents=" << evts;
  sqls << " where runnumber=" << runnumber;
  sqls << " and sequence=" << segnumber << " and prodtag =\'" << prodtag << "\'\"";
  sql  = sqls.str();
  sqls.str("");
  cout << "executing " << sql << endl;
  gSystem->Exec(sql.c_str());

  FileSummary();
  delete se;
  std::cout << " se cleaned up " << std::endl;

  //delete all singletons to make valgrind happy :)
  delete timeserv;
  TMutParameterDB &mutdb = TMutParameterDB::get();
  delete &mutdb;


  std::cout << "Reco exiting." << std::endl;
  gSystem->Exit(0);
}
