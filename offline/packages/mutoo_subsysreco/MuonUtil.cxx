// $Id: MuonUtil.cxx,v 1.74 2021/10/19 12:20:20 pinkenbu Exp $

/*!
  \file   MuonUtil.cxx
  \brief  utility methods for mutoo supermodules
  \author  Hugo Pereira
  \version $Revision: 1.74 $
  \date    $Date: 2021/10/19 12:20:20 $
*/

#include <Bbc.hh>
#include <BbcOut.h>
#include <EventHeader.h>
#include <getClass.h>
#include <L2DecisionHelper.h>
#include <mMfmMT.h>
#include <mMuiInitModule.h>
#include <mMuiFastRoadFinderPar.h>
#include <PHGeant.hh>
#include <PHGlobal.h>

#include <PHTimeServer.h>
#include <PHTFileServer.h>

#include <recoConsts.h>
#include <RunHeader.h>
#include <RunNumberRanges.h>
#include <RunToTime.hh>
#include <sstream>
#include <TMuiGeometry.hh>
#include <TMuiPseudoLL1Map.h>
#include <TMuiHVMask.h>
#include <TMutBPUtil.h>
#include <TMutDatabaseInit.h>
#include <utiCentrality.h>
#include <ZdcOut.h>

#include <MutGeom.h>
#include <MutArm.h>
#include <MutStrip.h>
#include <MutWire.h>

#include <fstream>

#include <TTree.h>

#include <headerWrapper.h>
#include <VtxOut.h>

#include "MuonUtil.h"

using namespace std;

//__________________________________________________________
// return 0, -1 or +1 depending on integer value
template < typename T >
int get_sign( const T& value )
{
  if( !value ) return 0;
  return (value > 0) ? 1:-1;
}

//______________________________________________________
bool MuonUtil::_check_mapfile_scale( true );
bool MuonUtil::_loaded( MuonUtil::_init() );

//______________________________________________________
void MuonUtil::check_timestamp( void )
{

  recoConsts *rc( recoConsts::instance() );

  int run_number( rc->get_IntFlag( "RUNNUMBER", 0 ) );
  PHTimeStamp time_stamp( rc->get_TimeStamp() );

  // over write timestamp from run number if valid
  if( time_stamp.getTics() == 0)
  {

    cout << "MuonUtil::check_timestamp - timeStamp not set. Try use value derived from run number" << endl;
    if( run_number != 0 )
    {

      RunToTime *runTime = RunToTime::instance();
      PHTimeStamp *time_stamp_ptr = runTime->getBeginTime( abs(run_number) );
      if ( time_stamp_ptr )
      {

        time_stamp = *time_stamp_ptr;
        rc->set_TimeStamp( time_stamp );
        delete time_stamp_ptr;

      }
    }
  }

  if( run_number ) cout << "MuonUtil::check_timestamp - run: " << run_number << endl;
  else cout << "MuonUtil::check_timestamp - invalid run number (0) " << endl;

  if( time_stamp.getTics() ) cout << "MuonUtil::check_timestamp - timestamp: " << time_stamp << " (" << time_stamp.getTics() << ")" << endl;
  else cout << "MuonUtil::check_timestamp - invalid time_stamp " << endl;

  return;

}

//______________________________________________________
void MuonUtil::initialize_database( PHCompositeNode *top_node, const unsigned int& flag )
{

  MUTOO::PRINT(cout, "MUTOO/MUIOO database initialization");

  // single shot timer to monitor initialization
  PHTimeServer::timer single_shot( PHTimeServer::get()->insert_new_single_shot( "MuonUtil::initialize_database" ) );
  single_shot.get()->restart();

  // geant initialization
  if( flag & GEANT ) initialize_geant( top_node );

  // check run number and time-stamp
  check_timestamp();
  cout << endl;

  // magnetic field
  if( flag & MAGNETIC_FIELD ) initialize_magnetic_field( top_node );

  // mutr and muid geometry
  if( flag & MUTR ) _initialize_mut_database( top_node );
  if( flag & MUID ) _initialize_mui_database( top_node );

  // stop the single_shot timer
  single_shot.get()->stop();

  MUTOO::PRINT(cout, "**");

}

//________________________________________________________
void MuonUtil::initialize_magnetic_field( PHCompositeNode *top_node )
{

  int current_south( 0 );
  int current_north( 0 );
  int current_central( 0 );
  int current_inner( 0 );

  try {

    RunHeader *run_header = TMutNode<RunHeader>::find_io_node(top_node, "RunHeader");
    if( !run_header->isValid() ) throw runtime_error( DESCRIPTION( "invalid RunHeader node" ) );

    current_south = run_header->get_currentSouth();
    current_north = run_header->get_currentNorth();
    current_central = run_header->get_currentCentral();
    current_inner = run_header->get_currentInner();

    // some dump
    cout << "MuonUtil::initialize_magnetic_field - south arm magnet current = " << current_south << endl;
    cout << "MuonUtil::initialize_magnetic_field - north arm magnet current = " << current_north << endl;
    cout << "MuonUtil::initialize_magnetic_field - central magnet current = " << current_central << endl;
    cout << "MuonUtil::initialize_magnetic_field - inner magnet current = " << current_inner << endl;

  } catch( exception &e ) {
    cout << e.what() << endl;
    cout << "MuonUtil::initialize_magnetic_field - set _checkmapfile_scale to false." << endl;
    _check_mapfile_scale = false;
  }

  // get sign from currents
  /* this is used to decide the magnetic field map and scale */
  int south_sign( get_sign( current_south ) );
  int north_sign( get_sign( current_north ) );
  int central_sign( get_sign( current_central ) );
  int inner_sign( get_sign( current_inner ) );

  // initialize magnet currents from RunHeader node
  if( _check_mapfile_scale )
  {

    // check consistency between muon arms
    if( south_sign != north_sign )
    { cout << "MuonUtil::initialize_magnetic_field - inconsistant magnet current signs between south and north muon arms.\n"; }

    // check consistency with central arm
    if( south_sign != central_sign )
    { cout << "MuonUtil::initialize_magnetic_field - inconsistant magnet current signs between south muon arm and central magnet.\n"; }

    // retrieve current map field scale
    double scale( mMfmMT::getMapFileScale() );
    if( get_sign( scale ) != south_sign ) {
      cout << "MuonUtil::initialize_magnetic_field - mapfile scale changed to match magnet currents sign" << endl;
      if( !south_sign ) mMfmMT::setMapFileScale( 0 );
      else mMfmMT::setMapFileScale( -scale );
    }

  } else cout << "MuonUtil::initialize_magnetic_field - _check_mapfile_scale is false, map file scale unchanged.\n";

  // changes TMutBPUtil mode according to map file scale
  double scale( mMfmMT::getMapFileScale() );
  if( scale < 0 ) TMutBPUtil::set_mode( TMutBPUtil::REVERSE );
  else if( scale > 0 ) TMutBPUtil::set_mode( TMutBPUtil::NORMAL );
  else TMutBPUtil::set_mode( TMutBPUtil::FIELD_OFF );

  cout
    << "MuonUtil::initialize_magnetic_field - mapfile scale="
    << mMfmMT::getMapFileScale() << endl;
  TMutBPUtil::print_mode();

  // set magnetic field map scale based on run number
  if( mMfmMT::getMapFileFlag() == mMfmMT::UNKNOWN )
  {

    cout << "MuonUtil::initialize_magnetic_field - getting mapFileFlag from run number." << endl;
    int run_number( recoConsts::instance()->get_IntFlag( "RUNNUMBER" ) );
    mMfmMT::MapFileFlag flag( mMfmMT::UNKNOWN );

    // untill Run9 (2009) hard coded map file flags are used, based on the Run number
    // after Run9, the map file flag is deduced from the magnet currents sign, which is in turn
    // set in HeadReco, based on what is read from the "run" table of the "daq" database
    if( run_number < static_cast<int>(BEGIN_OF_RUN3) ) flag = mMfmMT::MAP_3D_2001;
    else if( run_number < static_cast<int>(BEGIN_OF_RUN4) ) flag = mMfmMT::MAP_3D_2003;
    else {

      // get the magnetic field map from magnet currents
      // possibly compare to hard-coded value
      mMfmMT::MapFileFlag local_flag( mMfmMT::UNKNOWN );
      if( central_sign != 0 && inner_sign != 0 && (central_sign != inner_sign) ) local_flag = mMfmMT::MAP_3D_PLUS_MINUS;
      else local_flag = mMfmMT::MAP_3D_PLUS_PLUS;

      bool check = true;
      if( run_number < static_cast<int>(BEGIN_OF_RUN7) ) flag = mMfmMT::MAP_3D_PLUS_PLUS;
      else if( run_number < static_cast<int>(BEGIN_OF_RUN8) ) flag = mMfmMT::MAP_3D_PLUS_MINUS;
      else if( run_number < static_cast<int>(BEGIN_OF_RUN9) ) flag = mMfmMT::MAP_3D_PLUS_PLUS;
      else {

        // starting from run9, magnetic field deduced from magnet currents is used
        cout << "MuonUtil::initialize_magnetic_field - getting mapFileFlag flag from magnets currents." << endl;
        check = false;
        flag = local_flag;

      }

      if( check && (flag != local_flag) )
      {
        // for runs prior to run9, the magnet currents sign might not be reliable.
        // the 'local_flag' based on magnet currents is used for a cross-check with respect to the hard-coded value
        cout << endl;
        cout << "MuonUtil::initialize_magnetic_field - inconsistent magnetic maps are found." << endl;
        cout << "MuonUtil::initialize_magnetic_field - hard-coded: " << mMfmMT::getMapFileFlagName( flag ) << endl;
        cout << "MuonUtil::initialize_magnetic_field - from currents: " << mMfmMT::getMapFileFlagName( local_flag ) << endl;
        cout << endl;
      }

    }

    cout << "MuonUtil::initialize_magnetic_field - selected mapFileFlag: " << mMfmMT::getMapFileFlagName( flag ) << " (" << flag << ")" << endl;
    mMfmMT::setMapFileFlag( flag );

  } else {

    cout << "MuonUtil::initialize_magnetic_field - selected mapFileFlag: " << mMfmMT::getMapFileFlagName() << " (" << mMfmMT::getMapFileFlag() << ")" << endl;

  }

  // initialize magnetic field
  mMfmMT::initialize();

}

//________________________________________________________
void MuonUtil::initialize_geant( PHCompositeNode *top_node )
{
  if( PHGeant::Initialized() )
  {
    cout << "MuonUtil::initialize_geant - already initialized." << endl;
    return;
  }

  // GEANT initialization
  PHGeant::Init( recoConsts::instance()->get_IntFlag("GEANT_QUIET_INIT", 1 )  );

  // Material map
  // check if local file exist. Load from afs if not
  std::string pisafile( "pisafile.dat.cZ" );
  if( access( pisafile.c_str(), R_OK ) != 0 )
  {

		cout << "MuonUtil::initialize_geant - WARNNING!! " << endl;
		cout << "Can not find a local pisaFile. Use a pisaFile in afs" << endl;
		cout << "Recommend to use a script /opt/phenix/bin/LuxorLinker.pl" << endl;

    // note here we should generate other files for Run4, Run5, Run6, etc,
    // in case there was changes in the detector geometry.
    int run_number( recoConsts::instance()->get_IntFlag("RUNNUMBER") );
    if( run_number < static_cast<int>(BEGIN_OF_RUN3) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2001/pisafile.dat.cZ";
    else if( run_number < static_cast<int>(BEGIN_OF_RUN4) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2003/pisafile.dat.cZ";
    else if( run_number < static_cast<int>(BEGIN_OF_RUN7) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2004/pisafile.dat.cZ";
    else if( run_number < static_cast<int>(BEGIN_OF_RUN9) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2007/pisafile.dat.cZ";
    else if( run_number < static_cast<int>(BEGIN_OF_RUN10) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2009/pisafile.dat.cZ";
    else if( run_number < static_cast<int>(BEGIN_OF_RUN11) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2007/pisafile.dat.cZ"; // for run10, same material map is used as for Run7 and Run8
    else if( run_number < static_cast<int>(BEGIN_OF_RUN12) ) pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run2011/pisafile.dat.cZ"; 
    else pisafile = "/afs/rhic.bnl.gov/phenix/software/calibration/run12/pisafile.dat.cZ";

  }

  cout << endl;
  cout << "MuonUtil::initialize_geant - loading pisaFile " << pisafile << endl;

  // initialize geant (quiet mode)
  // load pisa file.
  int phgeant_lun = 82;
  PHGeant::ReadFile(phgeant_lun, const_cast<char*>(pisafile.c_str()));
  cout << endl;

}

//________________________________________________________
void MuonUtil::_initialize_mut_database( PHCompositeNode *top_node )
{

  // perform database initialization
  TMutDatabaseInit::reset();
  TMutDatabaseInit::initialize(top_node);

}

//________________________________________________________
void MuonUtil::_initialize_mui_database( PHCompositeNode *top_node )
{

  // retrieve timestamp from recoConsts
  PHTimeStamp time_stamp( recoConsts::instance()->get_TimeStamp() );

  // initialize geometry and HV maps
  cout << endl;
  mMuiInitModule mMuiInitMod;

  // set time stamp, reset, and initialize
  mMuiInitMod.SetSearchTimeStamp(time_stamp);
  mMuiInitMod.reset();
  mMuiInitMod.initialize();

  // set search time stamp for the muid tube efficiencies
  TMuiHVMask::set_search_timestamp( time_stamp );

}

//______________________________________________________
void MuonUtil::dump_process_time( void )
{ dump_process_time( std::cout ); }

//______________________________________________________
void MuonUtil::dump_process_time( ostream& out )
{

  int pid = getpid();
  ostringstream what;
  what << "ps -o sid,ppid,pid,user,comm,vsize,rssize,time | grep " << pid;

  // run command, get output
  FILE *tmp = popen( what.str().c_str(), "r" );

  // parse output line/line
  static const int linesize( 128 );
  char buf[linesize];

  MUTOO::PRINT( out, "dump_process_time" );
  while( fgets( buf, linesize, tmp ) )
  out << buf;
  MUTOO::PRINT( out, "**" );
  
  pclose(tmp);

}

//__________________________________________________
bool MuonUtil::get_l2_trigger_decision( PHCompositeNode* top_node, const string& algo_name )
{

  // initialize
  bool accepted( false );
  bool found( false );

  try {

    // retrieve Lvl2DecisionOut object (first try from L2DecisionCal node, then L2Decision)
    Lvl2DecisionOut* l2_decision = 0;
    try {
      l2_decision = TMutNode<Lvl2DecisionOut>::find_io_node(top_node,"L2DecisionCal");
    } catch ( exception &e ){
      l2_decision = TMutNode<Lvl2DecisionOut>::find_io_node(top_node,"L2Decision");
    }

    // try load TrigLvl2 object. Twicked since it can have two names: TrigRunLvl2Cal and TrigRunLvl2
    TrigRunLvl2* l2_triggers = 0;
    try {
      l2_triggers = TMutNode<TrigRunLvl2>::find_io_node(top_node,"TrigRunLvl2Cal");
    } catch ( exception &e ){
      l2_triggers = TMutNode<TrigRunLvl2>::find_io_node(top_node,"TrigRunLvl2");
    }

    // create trigger helper
    boost::shared_ptr< L2DecisionHelper > help( new L2DecisionHelper( l2_decision , l2_triggers ) );
    if( !help.get() ) throw std::runtime_error( DESCRIPTION( "no decision helper" ));

    for( unsigned int i_lvl2=0; i_lvl2<32; i_lvl2++ ) {
      if( !( algo_name == l2_triggers->get_lvl2_trig_name( i_lvl2 ) ) ) continue;
      found = true;

//       Lvl2DecisionOut *l2_decision = help.get()->get_lvl2DecisionOut();
//       for (int i_lvl1 = 0; i_lvl1 < 32; i_lvl1++) {

//         Lvl2Decision decision = l2_decision->getLvl1AlgorithmDecision( i_lvl1, i_lvl2 );
//         if( decision.isEmpty() ) continue;
//         accepted = ( decision.wasExecuted() && decision.wasAlgorithmAccepted() );
//         break;

//       }

    }


    if( !found ) {
      static unsigned int count( 0 );

      if( count < 10 ) {
        count++;
        cout << "MuonUtil::get_l2_trigger_decision - \"" << algo_name << "\" not found.\n";
        if( count == 10 ) cout << "MuonUtil::get_l2_trigger_decision - message disabled.\n";
      }

    }

  } catch( exception& e ) {
    static unsigned int count( 0 );

    if( count <= 10 ) {
      count++;
      cout << "MuonUtil::get_l2_trigger_decision - " << e.what() << endl;
      if( count == 10 ) cout << "MuonUtil::get_l2_trigger_decision - message disabled" << endl;
    }

  }

  return accepted;
}

//_______________________________________________
bool MuonUtil::get_LL1trigger_decision( PHCompositeNode *top_node, const unsigned int& arm, const mMuiFastRoadFinderPar::Mode& mode )
{

  static unsigned int count( 0 );
  try {

    TMuiPseudoLL1Map* Pseudoll1_map = TMutNode<TMuiPseudoLL1Map>::find_node(top_node,"TMuiPseudoLL1Map");
    TMuiPseudoLL1Map::const_iterator ll1_iter = Pseudoll1_map->range();
    while( TMuiPseudoLL1Map::const_pointer ll1_ptr = ll1_iter.next() )
    {
      if( ll1_ptr->get()->get_arm() == arm )
      { return ll1_ptr->get()->get_emulator_decision() & (1<<mode); }
    }

    if( count <= 10 )
    {
      count++;
      cout << " MuonUtil::get_LL1trigger_decision - unable to find trigger decision for arm " << arm << endl;
    }

  } catch( exception& e ){

    if( count <= 10 ) {
      count++;
      cout << " MuonUtil::get_LL1trigger_decision - " << e.what() << endl;
    }
  }
  if( count == 10 ) cout << "MuonUtil::get_1D_LL1trigger_decision - message disabled" <<endl;
  return false;
}


//_______________________________________________
bool MuonUtil::get_BLTtrigger_decision( PHCompositeNode *top_node, const unsigned int& arm, const mMuiBLTEmulator::Mode& mode, const bool& use_reco )
{

  static unsigned int count( 0 );
  try {
    TMuiPseudoBLTMapO* Pseudoblt_map = TMutNode<TMuiPseudoBLTMapO>::find_node(top_node,"TMuiPseudoBLTMapO");
    TMuiPseudoBLTMapO::const_iterator blt_iter = Pseudoblt_map->range();
    while( TMuiPseudoBLTMapO::const_pointer blt_ptr = blt_iter.next() )
    if( blt_ptr->get()->get_arm() == arm )
    {
      if( mode == mMuiBLTEmulator::DEEP_DEEP ) return (use_reco) ? blt_ptr->get()->is_reco_2D_fired():blt_ptr->get()->is_2D_fired();
      else if( mode == mMuiBLTEmulator::DEEP_SHALLOW ) return (use_reco) ? blt_ptr->get()->is_reco_1D1S_fired():blt_ptr->get()->is_1D1S_fired();
      else if( mode & (mMuiBLTEmulator::SINGLE_SHALLOW|mMuiBLTEmulator::SINGLE_DEEP ) )
      {

        for( unsigned int quad = 0; quad < mMuiBLTEmulator::max_quad; quad ++ )
        {
          if( mode == mMuiBLTEmulator::SINGLE_SHALLOW && ( (use_reco && blt_ptr->get()->is_reco_1S_fired( quad ) ) || blt_ptr->get()->is_1S_fired( quad ) ) ) return true;
          if( mode == mMuiBLTEmulator::SINGLE_DEEP    && ( (use_reco && blt_ptr->get()->is_reco_1D_fired( quad ) ) || blt_ptr->get()->is_1D_fired( quad ) ) ) return true;
        }
      }

      // trigger did not fire
      return false;

    }

    if( count <= 10 )
    {
      count++;
      cout << " MuonUtil::get_BLTtrigger_decision - unable to find trigger decision for arm " << arm << endl;
    }

  } catch( exception& e ){

    if( count <= 10 ) {
      count++;
      cout << " MuonUtil::get_BLTtrigger_decision - " << e.what() << endl;
    }
  }
  if( count == 10 ) cout << "MuonUtil::get_BLTtrigger_decision - message disabled" <<endl;
  return false;
}

//_______________________________________________
double MuonUtil::get_bbc_charge( PHCompositeNode *top_node )
{
  double out( 0 );
  try{
    BbcOut *bbc = TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" );
    out = bbc->get_ChargeSum(Bbc::South) + bbc->get_ChargeSum(Bbc::North);
  } catch( exception &e ) {
    static unsigned int count( 0 );
    count++;
    if( count <= 10 ) cout << "MuonUtil::get_bbc_charge - " << e.what() << endl;
    if( count == 10 ) cout << "MuonUtil::get_bbc_charge - message disabled" << endl;
  }
  return out;
}

//_______________________________________________
double MuonUtil::get_bbc_z( PHCompositeNode *top_node )
{
  double out( 0 );
  static unsigned int counts = 0;
  try {
    BbcOut *bbc = TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" );
    out = bbc->get_VertexPoint();
  } catch( exception &e ) {
    counts ++;
    if( counts <= 10 ) cout << "MuonUtil::get_bbc_z - unable to read from BbcOut. Trying PHGlobal. " << endl;
    try {
      PHGlobal *phglobal = TMutNode<PHGlobal>::find_io_node( top_node, "PHGlobal" );
      out = phglobal->getBbcZVertex();
    } catch( exception &e ) {
      if( counts <= 10 ) cout << "MuonUtil::get_bbc_z - unable to read from PHGlobal. " << endl;
    }

    if( counts == 10 ) cout << "MuonUtil::get_bbc_z - message disabled." << endl;

  }
  return out;
}

//_______________________________________________
double MuonUtil::get_centrality( PHCompositeNode *top_node )
{

  // retrieve run number from recoConsts
  int run_number = recoConsts::instance()->get_IntFlag("RUNNUMBER", 0);

  if( run_number < static_cast<int>(BEGIN_OF_RUN3) ) return 0;
  else if( run_number < static_cast<int>(BEGIN_OF_RUN4) ) return _get_run3_centrality( top_node );
  else if( run_number < static_cast<int>(BEGIN_OF_RUN5) ) return _get_run4_centrality( top_node );
  else if( run_number < static_cast<int>(BEGIN_OF_RUN6) ) return _get_generic_centrality( top_node );
  else if( run_number < static_cast<int>(BEGIN_OF_RUN7) ) return 0;
  else return _get_generic_centrality( top_node );

}

//_______________________________________________
double MuonUtil::_get_run3_centrality( PHCompositeNode *top_node )
{

  try{

    PHGlobal *global = TMutNode<PHGlobal>::find_io_node(top_node,"PHGlobal");
    return global->get_dAuBbcCentrality();

  } catch( exception &e ) {

    static unsigned int count( 0 );
    count++;
    if( count <= 10 ) cout << "MuonUtil::get_run3_centrality - " << e.what() << endl;
    if( count == 10 ) cout << "MuonUtil::get_run3_centrality - message disabled" << endl;

  }

  return 0;

}

//_______________________________________________
double MuonUtil::_get_run4_centrality( PHCompositeNode *top_node )
{

  try {

    BbcOut* bbcout = TMutNode<BbcOut>::find_io_node( top_node, "BbcOut" );
    ZdcOut* zdcout = TMutNode<ZdcOut>::find_io_node( top_node, "ZdcOut" );

    // retrieve run number from recoConsts
    int run_number = recoConsts::instance()->get_IntFlag("RUNNUMBER", 0);

    // retrieve centrality from uti
    return static_cast<double>(
      PhUtilities::getCentralityByClockRun4(
        bbcout->get_ChargeSum(Bbc::North),bbcout->get_ChargeSum(Bbc::South),
        zdcout->get_Energy( Zdc::North ), zdcout->get_Energy( Zdc::South ),
        run_number
      ) );

  } catch( exception &e ) {

    // try use PHGlobal
    return static_cast<double>( PhUtilities::getCentralityByClockRun4( top_node ) );

  }

}

//_______________________________________________
double MuonUtil::_get_generic_centrality( PHCompositeNode *top_node )
{

  try{

    PHGlobal *global = TMutNode<PHGlobal>::find_io_node(top_node,"PHGlobal");
    return global->getCentrality();

  } catch( exception &e ) {

    static unsigned int count( 0 );
    count++;
    if( count <= 10 ) cout << "MuonUtil::_get_generic_centrality - " << e.what() << endl;
    if( count == 10 ) cout << "MuonUtil::_get_generic_centrality - message disabled" << endl;

  }

  return 0;

}

//_______________________________________________
void MuonUtil::dump_evtN_bbcZ_run5centrality( PHCompositeNode *top_node, int fraction )
{

  // make local event counter
  static int local_event_number( 0 );
  local_event_number++;

  if( local_event_number % fraction ) return;

  // retrieve bbc_z
  double bbc_z = get_bbc_z( top_node );
  double centrality = get_centrality( top_node );
  ostringstream what;
  what << "Evt : "<< local_event_number <<" " <<  bbc_z << " " << centrality;
  MUTOO::PRINT( cout, what.str().c_str() );

}

//_______________________________________________
bool MuonUtil::_init( void )
{

  return true;

}

//_______________________________________________
void MuonUtil::_check_mut_geometry( void )
{
  // print anode positions
  MUTOO::PRINT( cout, "MuonUtil::_check_mut_geometry" );

  // open TFile
  std::string filename( "mut_geometry.root" );
  PHTFileServer::get().open( filename );

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  int arm(0);
  int station(0);
  int octant(0);
  int half(0);
  int gap(0);
  int index(0);
  int wire(0);

  boost::array<double,4> distances;
  distances.assign(0);

  // create ntuple
  TTree* tree = new TTree( "global", "event global informations" );
  tree->Branch( "arm", &arm, "arm/I", BUFFER_SIZE );
  tree->Branch( "station", &station, "station/I", BUFFER_SIZE );
  tree->Branch( "octant", &octant, "octant/I", BUFFER_SIZE );
  tree->Branch( "half", &half, "half/I", BUFFER_SIZE );
  tree->Branch( "gap", &gap, "gap/I", BUFFER_SIZE );
  tree->Branch( "index", &index, "index/I", BUFFER_SIZE );
  tree->Branch( "wire", &wire, "wire/I", BUFFER_SIZE );
  tree->Branch( "distance", &distances[0], "distance[4]/D", BUFFER_SIZE );


  // fill ntuple
  for( arm = 0; arm < MUTOO::MAX_ARM; arm++ )
  {
    for( station = 0; station < MUTOO::MAX_STATION; station++ )
    {


      // print anode plane z position
      MutArm* arm_pointer( arm == MUTOO::South ? SouthArm() : NorthArm() );
      MutStation* station_pointer( arm_pointer->f_pMutStations[station] );

      for( octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
      {
        for( half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
        {
          for( gap = 0; gap < MUTOO::MAX_GAP; gap++ )
          {

            // only two gaps in station 3
            if( station == MUTOO::Station3 && gap == MUTOO::Gap3 ) continue;

            MutGap* gap_pointer( station_pointer
              ->f_pMutOctants[octant]
              ->f_pMutHalfOctants[half]
              ->f_pMutGaps[gap] );

            // first cathode
            int num_elements_0( gap_pointer->f_pMutPlanes[0]->getNumElements() );
            PHPlane plane_cathode_0 = PHPlane(
              gap_pointer->f_pMutPlanes[0]->f_pMutStrips[0]->getGlobalPositionBegin(),
              gap_pointer->f_pMutPlanes[0]->f_pMutStrips[0]->getGlobalPositionEnd(),
              gap_pointer->f_pMutPlanes[0]->f_pMutStrips[num_elements_0-1]->getGlobalPositionEnd() );
            PHPoint point_cathode_0( gap_pointer->f_pMutPlanes[0]->f_pMutStrips[0]->getGlobalPositionBegin() );

            // second cathode
            int num_elements_1( gap_pointer->f_pMutPlanes[2]->getNumElements() );
            PHPlane plane_cathode_1 = PHPlane(
              gap_pointer->f_pMutPlanes[2]->f_pMutStrips[0]->getGlobalPositionBegin(),
              gap_pointer->f_pMutPlanes[2]->f_pMutStrips[0]->getGlobalPositionEnd(),
              gap_pointer->f_pMutPlanes[2]->f_pMutStrips[num_elements_1-1]->getGlobalPositionEnd() );
            PHPoint point_cathode_1( gap_pointer->f_pMutPlanes[2]->f_pMutStrips[0]->getGlobalPositionBegin() );

            // first distance is from strip in cathode plane 0 to cathode plane 1
            distances[0] = PHVector( point_cathode_1 - point_cathode_0 ).dot( plane_cathode_0.getNormal() );
            distances[1] = PHVector( point_cathode_1 - point_cathode_0 ).dot( plane_cathode_1.getNormal() );

            // anode
            int num_elements_wire( gap_pointer->f_pMutPlanes[1]->getNumElements() );
            for( wire = 0; wire < num_elements_wire; wire++ )
            {

              MutWire *wire_pointer = gap_pointer->f_pMutPlanes[1]->f_pMutWires[wire];
              assert( wire_pointer );

              distances[2] = PHVector( wire_pointer->getGlobalPositionBegin() - point_cathode_0 ).dot( plane_cathode_0.getNormal() );
              distances[3] = PHVector( wire_pointer->getGlobalPositionBegin() - point_cathode_1 ).dot( plane_cathode_1.getNormal() );
              index = 0;
              tree->Fill();

              distances[2] = PHVector( wire_pointer->getGlobalPositionEnd() - point_cathode_0 ).dot( plane_cathode_0.getNormal() );
              distances[3] = PHVector( wire_pointer->getGlobalPositionEnd() - point_cathode_1 ).dot( plane_cathode_1.getNormal() );
              index = 1;
              tree->Fill();

            }

          }
        }
      }
    }
  }

  PHTFileServer::get().write( filename );

  MUTOO::PRINT( cout, "**" );
}

//_______________________________________________
void MuonUtil::_print_mut_st3_wires( void )
{
  MUTOO::PRINT( cout, "MuonUtil::_print_mut_st3_wires" );

  // open TFile
  std::string filename( "mut_geometry.txt" );
  cout << "writing to " << filename << endl;

  ofstream out( filename.c_str() );
  char line[512];

  for( int arm = 0; arm < MUTOO::MAX_ARM; arm++ )
  {
    for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    {

      MutArm* arm_pointer( arm == MUTOO::South ? SouthArm() : NorthArm() );
      MutStation* station_pointer( arm_pointer->f_pMutStations[station] );

      for( int octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
      {
        for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
        {
          for( int gap = 0; gap < MUTOO::MAX_GAP; gap++ )
          {

            // only two gaps in station 3
            if( station == MUTOO::Station3 && gap == MUTOO::Gap3 ) continue;

            MutGap* gap_pointer( station_pointer
              ->f_pMutOctants[octant]
              ->f_pMutHalfOctants[half]
              ->f_pMutGaps[gap] );

            // anode
            int num_elements_wire( gap_pointer->f_pMutPlanes[1]->getNumElements() );
            for( int i_wire = 0; i_wire < num_elements_wire; i_wire++ )
            {

              MutWire *wire = gap_pointer->f_pMutPlanes[1]->f_pMutWires[i_wire];
              assert( wire );

              PHPoint wireBegin( wire->getGlobalPositionBegin() );
              PHPoint wireEnd( wire->getGlobalPositionEnd() );

              double angle = atan2( wireEnd.getY() - wireBegin.getY(), wireEnd.getX() - wireBegin.getX() );

              sprintf( line, "%2i %2i %2i %2i %2i %10i", arm, station, octant, half, gap, i_wire );
              out << line;

              sprintf( line, " %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f",
                angle,
                wireBegin.getX(), wireBegin.getY(), wireBegin.getZ(),
                wireEnd.getX(), wireEnd.getY(), wireEnd.getZ() );
              out << line;
              out << endl;

            }

          }
        }
      }
    }
  }

  out.close();
  MUTOO::PRINT( cout, "**" );
}

//_______________________________________________
void MuonUtil::_print_dead_strips( void )
{
  MUTOO::PRINT( cout, "MuonUtil::_print_dead_strips" );

  // open TFile
  std::string filename( "mut_geometry.txt" );
  cout << "writing to " << filename << endl;

  ofstream out( filename.c_str() );
  for( int arm = 0; arm < MUTOO::MAX_ARM; arm++ )
  {
    for( int station = 0; station < MUTOO::MAX_STATION; station++ )
    {

      MutArm* arm_pointer( arm == MUTOO::South ? SouthArm() : NorthArm() );
      MutStation* station_pointer( arm_pointer->f_pMutStations[station] );

      for( int octant = 0; octant < MUTOO::MAX_OCTANT; octant++ )
      {
        for( int half = 0; half < MUTOO::MAX_HALF_OCTANT; half++ )
        {
          for( int gap = 0; gap < MUTOO::MAX_GAP; gap++ )
          {

            // only two gaps in station 3
            if( station == MUTOO::Station3 && gap == MUTOO::Gap3 ) continue;

            MutGap* gap_pointer( station_pointer
              ->f_pMutOctants[octant]
              ->f_pMutHalfOctants[half]
              ->f_pMutGaps[gap] );

            for( int cathode = 0; cathode < MUTOO::MAX_CATHODE; cathode++ )
            {

              int cathode_id = ( cathode == 0 ) ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2;

              // loop over strips
              MutPlane* plane_pointer( gap_pointer->f_pMutPlanes[cathode_id] );
              int num_elements( plane_pointer->getNumElements() );
              for( int i_strip = 0; i_strip < num_elements; i_strip++ )
              {

                MutStrip *strip = plane_pointer->f_pMutStrips[i_strip];
                assert( strip );

                if( strip->ChannelIsDead() )
                { out << arm << " " << station << " " << octant << " " << half << " " << gap << " " << cathode << " " << i_strip << endl; }
              }
            }
          }
        }
      }
    }
  }

  out.close();
  MUTOO::PRINT( cout, "**" );
}

//___________________________________________________________
void MuonUtil::check_vertex_matching( PHCompositeNode* signal_node, PHCompositeNode* background_node, PHCompositeNode* top_node )
{

  double sim_vertex(0);
  double out_vertex(0);

  // MC
  try {
    // retrieve header wrapper
    headerWrapper* header = TMutNode<headerWrapper>::find_io_node(signal_node,"header");
    HEADER_ST*	 header_table = 0;

    if( !( header && ( header_table	= header->TableData() ) ) ) cout << "MuonUtil::check_vertex_matching - MC: invalid." << endl;
    else cout << "MuonUtil::check_vertex_matching - MC: " << ( sim_vertex = header_table[0].vertex[2] ) << endl;

  } catch (std::exception& e) { MUTOO::TRACE( e.what() ); }

  // VTXOut
  try
  {
    // retrieve VtxOut object
    VtxOut* vtx_out( TMutNode<VtxOut>::find_io_node( top_node, "VtxOut" ) );
    out_vertex = vtx_out->get_Vertex("FVTX").getZ();
    if ( !(fabs(out_vertex)<200) )
      out_vertex = vtx_out->get_Vertex("SVX_PRECISE").getZ();
    if ( !(fabs(out_vertex)<200) )
      out_vertex = vtx_out->get_Vertex("BBC").getZ();
    cout << "MuonUtil::check_vertex_matching - VTXOUT: " << out_vertex << endl;

  } catch (std::exception& e) { MUTOO::TRACE( e.what() ); }

  if( fabs( out_vertex-sim_vertex ) > 1e-3  ) cout << "MuonUtil::check_vertex_matching - WARNING: inconsistant vertex" << endl;
}
