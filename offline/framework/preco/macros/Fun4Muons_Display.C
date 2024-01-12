// $Id: Fun4Muons_Display.C,v 1.18 2009/12/03 23:54:32 hpereira Exp $
/*!
  dst analysis loop for real data/MC dst
  read nodes from the DST
  opens either 2D or 3DDisplay
*/

class MuonDisplay;
class Muon3DDisplay;
class Fun4AllServer;

MuonDisplay *muon_display_2D = 0;
Muon3DDisplay *muon_display_3D = 0;
Fun4AllServer *se = 0;

//_________________________________________________________________________
void Fun4Muons_Display( char *inputfile = "dst.root" )
{

  bool use_RPC = false;
  bool use_3DDisplay = true;
  bool use_2DDisplay = false;
  if( !(use_3DDisplay||use_2DDisplay) ) {
    cerr << "at least one from 2D/3D display must be selected. Aborting." << endl;
    return;
  }

  // libraries
  gSystem->Load("libfun4all");
  gSystem->Load("libmutoo_subsysreco");
  gSystem->Load("libfun4allfuncs_muons");
  gSystem->Load("libmuon_subsysreco");
  gSystem->Load("liblvl2");
  gSystem->Load("libMWG_interface");

  ///////////////////////////////////////////
  // recoConsts setup
  //////////////////////////////////////////
  recoConsts *rc = recoConsts::instance();
  rc->set_IntFlag("PRINT_MUTOO_PARAMETERS",1);

  // mutoo vertex source configuration
  // this allows to print which vertex is used and its value
  TMutExtVtx::get().set_verbosity( MUTOO::SOME );

  ///////////////////////////////////////////
  // Make the Server
  //////////////////////////////////////////
  se = Fun4AllServer::instance();
  se->Verbosity(0);

  ///////////////////////////////////////////
  // Subsystems
  //////////////////////////////////////////

  se->registerSubsystem( new MuonCounter() );

  MuonReadbackDST *readback_dst = new MuonReadbackDST();
  readback_dst->set_do_dbinit( true );
  se->registerSubsystem( readback_dst );

  if(use_RPC){
    gSystem->Load("librpc_subsysreco");
    se->registerSubsystem( new RpcReadbackDST() );
  }

  if( use_3DDisplay ) {
    muon_display_3D = new Muon3DDisplay();
    se->registerSubsystem( muon_display_3D );
  }

  if( use_2DDisplay ) {
    muon_display_2D = new MuonDisplay();
    se->registerSubsystem( muon_display_2D );
  }

  ///////////////////////////////////////////
  // Input manager
  ///////////////////////////////////////////
  Fun4AllInputManager *signal = new Fun4AllNoSyncDstInputManager("IMDST","DST");
  se->registerInputManager(signal);
  se->fileopen(signal->Name(),inputfile);

  ///////////////////////////////////////////
  // Analyze the Data.
  //////////////////////////////////////////

  // run first event to make all initializations
  se->run(1);

  // dump help
  if( use_3DDisplay ) muon_display_3D->help("muon_display_3D");
  if( use_2DDisplay ) muon_display_2D->help("muon_display_2D");
  cout << "To get next event type: se->run(1)" << endl;

}
