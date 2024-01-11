// $Id: MuonUnpackSim.cxx,v 1.67 2019/05/08 17:02:01 slash Exp $

/*!
\file MuonUnpackSim.cxx
\ingroup supermodules
\brief reads muid/mutr mc hit/track maps from a simulated DST
runs the response to build muid/mutr hit maps for later reconstruction.
\author Sean Kelly
\version $Revision: 1.67 $
\date $Date: 2019/05/08 17:02:01 $
*/

#include"MuonUnpackSim.h"
#include"MuonUtil.h"

#include<recoConsts.h>
#include<Fun4AllServer.h>
#include<RunHeader.h>
#include <getClass.h>
#include<boost/array.hpp>

// MUTOO IOC includes
#include<TMutHitMap.h>
#include<TMutMCHitMap.h>
#include<TMutMCTrkMap.h>
#include<TMCPrimaryMap.h>

// MUIOO IOC includes
#include<TMuiMCHitMapO.h>
#include<TMuiHitMapO.h>
#include<TMuiHVMask.h>

// MUTOO Module includes
#include<mMutResponse.h>
#include<mMutResponsePar.h>
#include<mMutCalibrate.h>
#include<mMutCalibratePar.h>
#include<mMutEmbed.h>
#include<mMutEmbedPar.h>
#include<mMutZeroSup.h>
#include<mMutZeroSupPar.h>

// MUIOO Module includes
#include<mMuiResponse.h>
#include<mMuiResponsePar.h>
#include<mMuiEmbed.h>
#include<mMuiEmbedPar.h>

#include<PHTimeServer.h>
#include<TMutExtVtx.h>

#include <odbc++/connection.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>

#include<iostream>
#include <sstream>
#include<fstream>

using namespace odbc;
using namespace std;

//______________________________________________________
MuonUnpackSim::MuonUnpackSim( const char* name, unsigned int mode, unsigned int vtx_to_use ) :
  MuonSubsysReco( name ),
  _signalNodeName( "SIGNAL" ),
  _backgroundNodeName( "BACKGROUND" ),
  _mutoo_node(0),
  _signal_node(0),
  _ioc_signal_node(0),
  _background_node(0),
  _ioc_background_node(0),
  _timer(PHTimeServer::get()->insert_new(name) ),
  _mode(mode),
  _vtx_to_use(vtx_to_use),
  _flags( NONE ),
  _check_bbc_rate( false )
  {

    for(int i=0;i<1024;i++)
      {
	_mutr_base_efficiency[i]=1;
	_chamber_eff0[i]=0.98;
	_chamber_eff1[i]=0;
      }
  }

//______________________________________________________
MuonUnpackSim::~MuonUnpackSim( void )
{}

//______________________________________________________
int MuonUnpackSim::Init(PHCompositeNode *top_node)
{

  // call base class initialization
  // this is needed to get the module row (in list of registered modules) set properly
  MuonSubsysReco::Init( top_node );

  MUTOO::PRINT(cout, "MuonUnpackSim::Init");
  cout << "NO_ZERO_SUP : " << ( get_flag( NO_ZERO_SUP ) ? "true":"false" ) << endl;
  cout << "NO_CALIBRATE : " << ( get_flag( NO_CALIBRATE ) ? "true":"false" ) << endl;
  cout << "NO_CHARGE_SMEAR : " << ( get_flag( NO_CHARGE_SMEAR ) ? "true":"false" ) << endl;
  cout << "NO_RMS_SCALE : " << ( get_flag( NO_RMS_SCALE ) ? "true":"false" ) << endl;
  cout << "NO_MC_PRIMARY    : " << ( get_flag( NO_MC_PRIMARY ) ? "true":"false" ) << endl;
  MUTOO::PRINT(cout, "**");
  return 0;
}

//______________________________________________________
int MuonUnpackSim::InitRun(PHCompositeNode *top_node)
{
  MUTOO::PRINT(cout, "MuonUnpackSim::InitRun");

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "MuonUnpackSim::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  if ( rc->FlagExist("EMBED_REAL_TOPNODE") )
  {
    cout << "MuonUnpackSim::InitRun - reading _backgroundNodeName from recoConst EMBED_REAL_TOPNODE" << endl;
    SetBackgroundNodeName( rc->get_CharFlag("EMBED_REAL_TOPNODE") );
  }

  cout << "MuonUnpackSim::InitRun - _signalNodeName : " << _signalNodeName << endl;
  cout << "MuonUnpackSim::InitRun - _backgroundNodeName: " << _backgroundNodeName << endl;

  //extract the BBC rate of this run for chamber efficiency calculation
  if (_check_bbc_rate)
    {
      float bbcrate = get_bbcrate(top_node);
      for (int arm=0; arm<2; arm++)
	for(int station=0;station<MUTOO::NumberOfStations;station++)
	  for(int gap=0;gap<MUTOO::NumberOfGaps;gap++)
	    for(int pla=0;pla<MUTOO::NumberOfPlanes;pla++)
	      for (int octant=0; octant<MUTOO::MAX_OCTANT; octant++)
		for (int hoct=0; hoct<MUTOO::MAX_HALF_OCTANT; hoct++)
		  {

		    int index = pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(octant + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
		    float eff = _chamber_eff0[index] + _chamber_eff1[index]*bbcrate;
		    set_mutr_base_efficiency(arm, station, gap, pla, octant, hoct, eff);
		  }
    }

  // create nodes
  CreateNodeTree(top_node);

  // database initialization
  MuonUtil::initialize_database( top_node );

  /*
     Check that MUIOO/MUTOO simulation has not been
     turned off at the script level.
  */
  if(rc->get_IntFlag("MUTOO_SIM_OFF",0)) set_flag( SKIP_MUTOO, true );

  if( get_flag( SKIP_MUTOO ) ) cout << "MUTOO simulations are disabled" << endl;
  else cout << "MUTOO simulations are active" << endl;

  if(rc->get_IntFlag("MUIOO_SIM_OFF",0)) set_flag( SKIP_MUIOO, true );

  if( get_flag( SKIP_MUIOO ) ) cout << "MUIOO simulations are disabled" << endl;
  else cout << "MUIOO simulations are active" << endl;

  // dump configuration flag
  cout << endl;
  cout << "flags: " << _flags << endl;
  cout << "SKIP_MUIOO: " << ( get_flag( SKIP_MUIOO ) ? "true":"false" ) << endl;
  cout << "SKIP_MUTOO: " << ( get_flag( SKIP_MUTOO ) ? "true":"false" ) << endl;

  MUTOO::PRINT(cout, "**");
  return 0;
}

float MuonUnpackSim::get_bbcrate(PHCompositeNode* top_node) const
{
  // gtetting BBC rate (MHz) from database 
  float bbcrate = 0;
  RunHeader* runh = findNode::getClass<RunHeader>(top_node,"RunHeader");
  if (runh)
    {
      int runnumber = runh->get_RunNumber();
      Connection* con = 0;
      Statement* stmt = 0;
      ResultSet* rs = 0;
      try
	{
	  con = DriverManager::getConnection("daq", "phnxrc", "");
	}
      catch (SQLException& e)
	{
	  cout << PHWHERE
	       << " Exception caught during DriverManager::getConnection" << endl;
	  cout << e.getMessage() << endl;
	  return false;
	}
      stmt = con->createStatement();
      try
	{
	  string cmd = Form("select trigger.name,scalerupdatescaled*(scaledown+1)/extract(epoch from (run.ertimestamp-run.brtimestamp)) as bbcrate from trigger,run where trigger.runnumber=%d and run.runnumber=%d and trigger.name='BBCLL1(>0 tubes)'",runnumber,runnumber);
	  rs = stmt->executeQuery(cmd.c_str());
	}
      catch (SQLException& e)
	{
	  cout << e.getMessage() << endl;
	  cout << PHWHERE << "No database entries" << endl;
	  delete stmt;
	  delete con;
	  return false;
	}
      while (rs->next())
	{
	  bbcrate = rs->getFloat("bbcrate")/1e6;
	  cout << PHWHERE << " BBC rate used for MuTr chamber efficiency = " << bbcrate << " MHz" << endl;
	}
    }
  else
    cout << PHWHERE << "Could not find the run header" << endl;
  return bbcrate;
}

//______________________________________________________
void MuonUnpackSim::SetNodePtrs(PHCompositeNode* top_node)
{

  // Note: The node tree topology is a little confusing -- we need 2 nodes per
  // DST. One has the actual DST resident objects the other has the mutoo/muioo maps.
  // Once per event the objects in the DST are read in a used to poplulate the
  // "smart" mutoo/muioo containers.  We put module parameter tables in the same node
  // as the containers it interacts with, eg mMutResponsePar goes in IOC_SIGNAL.

  // get signal and background top nodes
  Fun4AllServer* se = Fun4AllServer::instance();
  PHCompositeNode* signal_top_node = se->topNode( _signalNodeName );
  PHCompositeNode* background_top_node = se->topNode( _backgroundNodeName );

  // create relevant node iterators
  PHNodeIterator signal_nodeItr( signal_top_node );
  PHNodeIterator background_nodeItr( background_top_node );
  PHNodeIterator nodeItr(top_node);

  // Merged node -- MUTOO maps used for reconstruction
  _mutoo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_mutoo_node){
    _mutoo_node = new PHCompositeNode("MUTOO");
    top_node->addNode(_mutoo_node);
  }

  // Merged node -- MUTOO maps used for reconstruction
  _muioo_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "MUIOO"));
  if(!_muioo_node){
    _muioo_node = new PHCompositeNode("MUIOO");
    top_node->addNode(_muioo_node);
  }

  // Signal node -- Maps associated with signal DST
  _ioc_signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_ioc_signal_node)
  {
    cout << "MuonUnpackSim::SetNodePtrs - creating ioc_signal_node" << endl;
    _ioc_signal_node = new PHCompositeNode("MUTOO");
    signal_top_node->addNode(_ioc_signal_node);
  }

  // Background node -- Maps associated with background DST
  _ioc_background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "MUTOO"));
  if(!_ioc_background_node)
  {

    cout << "MuonUnpackSim::SetNodePtrs - creating ioc_background_node" << endl;
    _ioc_background_node = new PHCompositeNode("MUTOO");
    background_top_node->addNode(_ioc_background_node);

  }

  // DST Signal
  _signal_node = static_cast<PHCompositeNode*>(signal_nodeItr.findFirst("PHCompositeNode", "DST"));
  if (!_signal_node) throw runtime_error(DESCRIPTION("Cannot locate signal DST node"));

  // DST Background
  if(_mode != MC_SIGNAL_NO_BG)
  {

    _background_node = static_cast<PHCompositeNode*>(background_nodeItr.findFirst("PHCompositeNode", "DST"));
    if (!_background_node) throw runtime_error(DESCRIPTION("Cannot locate background DST node"));

  }

  return;

}

//______________________________________________________
int MuonUnpackSim::CreateNodeTree(PHCompositeNode *top_node)
{
  try {

    MUTOO::PRINT( cout, "MuonUnpackSim::CreateNodeTree" );
    SetNodePtrs(top_node);

    // MC objects to output DST
    PHNodeIterator nodeItr(top_node);
    PHCompositeNode* dst_node = static_cast<PHCompositeNode*>(nodeItr.findFirst("PHCompositeNode", "DST"));

    // Monte-carlo hits and tracks are read from the input SIGNAL node
    if (_mode != REAL_SIGNAL_REAL_BG)
    {

      TMutMCHitMap* mchit_map = TMutNode<TMutMCHitMap>::new_dst_input_node(_ioc_signal_node, "TMutMCHitMap", _signal_node, "TMutMCHit");
      TMuiMCHitMapO* mui_mchit_map = TMutNode<TMuiMCHitMapO>::new_dst_input_node(_ioc_signal_node, "TMuiMCHitMapO", _signal_node, "TMuiMCHitO");
      TMutMCTrkMap* mctrk_map = TMutNode<TMutMCTrkMap>::new_dst_input_node(_ioc_signal_node, "TMutMCTrkMap", _signal_node, "TMutMCTrk");

      // primary particles
      TMCPrimaryMap* mc_primary_map=0;
      try {
        mc_primary_map = TMutNode<TMCPrimaryMap>::new_dst_input_node(_ioc_signal_node, "TMCPrimaryMap", _signal_node, "TMCPrimary");
      } catch(...) {
        set_flag( NO_MC_PRIMARY, true );
      }

      if (dst_node)
      {

        // MC maps
        mchit_map->make_persistant(dst_node,"TMutMCHit");
        mui_mchit_map->make_persistant(dst_node,"TMuiMCHitO");
        mctrk_map->make_persistant(dst_node,"TMutMCTrk");
        if( !get_flag( NO_MC_PRIMARY) ) mc_primary_map->make_persistant(dst_node,"TMCPrimary");

        // Make sure EVA & GEA end up in the output DST if they are in the input signal DST
        PHNodeIterator dstItr(dst_node);
        PHNodeIterator signalItr(_signal_node);
        if( !dstItr.findFirst("PHCompositeNode", "GEA") )
        {
          cout << "Copying GEA node from SIGNAL to DST" << endl;
          PHCompositeNode* gea_node = static_cast<PHCompositeNode*>(signalItr.findFirst("PHCompositeNode", "GEA"));
          if( gea_node ) dst_node->addNode(gea_node);
        }

        if( !dstItr.findFirst("PHCompositeNode", "EVA") )
        {
          cout << "Copying EVA node from SIGNAL to DST" << endl;
          PHCompositeNode* eva_node = static_cast<PHCompositeNode*>(signalItr.findFirst("PHCompositeNode", "EVA"));
          if( eva_node ) dst_node->addNode(eva_node);
        }

      }

    }

    // Response module populates hit map on signal node from DST resident Monte-Carlo
    // hits and tracks.
    if (_mode != REAL_SIGNAL_REAL_BG)
    {

      TMutNode<TMutHitMap>::new_node(_ioc_signal_node, "TMutHitMap");
      TMutNode<TMuiHitMapO>::new_node(_ioc_signal_node, "TMuiHitMapO");

    }

    // mMuiEmbedPar goes in muioo node (mMuiEmbed get the true top node)
    mMuiEmbedPar* mMuiEmbed_par = TMutNode<mMuiEmbedPar>::new_node(_muioo_node,"mMuiEmbedPar");
    mMuiEmbed_par->set_verbosity(MUIOO::NONE);

    // mMutEmbedPar goes in mutoo node (mMutEmbed get the true top node)
    mMutEmbedPar* mMutEmbed_par = TMutNode<mMutEmbedPar>::new_node(_mutoo_node,"mMutEmbedPar");
    mMutEmbed_par->set_verbosity(MUTOO::NONE);

    // muid efficiency
    if( recoConsts::instance()->FlagExist("MUIOO_TUBE_EFF") )
    {

      MUTOO::PRINT( cout, "MuonUnpackSim::CreateNodeTree" );
      cout << "The use of setDoubleFlag( \"MUIOO_TUBE_EFF\", eff ) is obsolete" << endl;
      cout << "Better use: " << endl;
      cout << "  TMuiHVMask::set_mode( TMuiHVMask::FIXED_VALUE );" << endl;
      cout << "  TMuiHVMask::set_effic_twopack( eff );" << endl;
      cout << "in your macro." << endl;
      double efficiency = recoConsts::instance()->get_DoubleFlag( "MUIOO_TUBE_EFF", 1.0 );
      TMuiHVMask::set_mode( TMuiHVMask::FIXED_VALUE );
      TMuiHVMask::set_effic_twopack( efficiency );

    }

    if(_mode == MC_SIGNAL_MC_BG) {

      // Background hits are read from BACKGROUND node
      TMutNode<TMutMCHitMap>::new_dst_input_node(_ioc_background_node, "TMutMCHitMap", _background_node, "TMutMCHit");

      // Background hits are read from BACKGROUND node
      TMutNode<TMuiMCHitMapO>::new_dst_input_node(_ioc_background_node, "TMuiMCHitMapO", _background_node, "TMuiMCHitO");
      TMutNode<TMutMCTrkMap>::new_dst_input_node(_ioc_background_node, "TMutMCTrkMap", _background_node, "TMutMCTrk");

      TMutNode<TMutHitMap>::new_node(_ioc_background_node, "TMutHitMap");
      TMutNode<TMuiHitMapO>::new_node(_ioc_background_node, "TMuiHitMapO");

      // background muid response
      mMuiResponsePar* mMuiResponse_par = TMutNode<mMuiResponsePar>::new_node(_ioc_background_node,"mMuiResponsePar");
      mMuiResponse_par->set_verbosity(MUIOO::NONE);

      // background mutr response
      mMutResponsePar* mMutResponse_par = TMutNode<mMutResponsePar>::new_node(_ioc_background_node,"mMutResponsePar");
      mMutResponse_par->set_verbosity(MUTOO::NONE);
      if( get_flag( NO_CHARGE_SMEAR ) ) mMutResponse_par->set_smear_q( false );
      if( get_flag( NO_RMS_SCALE ) ) mMutResponse_par->set_use_rms_scale( false );

      for(int arm=0; arm<MUTOO::NumberOfArms; arm++)
	for(int station=0; station <MUTOO::NumberOfStations; station ++)
	  for(int gap=0; gap < MUTOO::NumberOfGaps; gap++)
	    for(int pla=0; pla < MUTOO::NumberOfPlanes; pla++)
	      for (int oct=0; oct < MUTOO::MAX_OCTANT; oct++)
	      for (int hoct=0; hoct < MUTOO::MAX_HALF_OCTANT; hoct++)
		{
		  if(station == 2 && gap == 2) continue;
		  int index = pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(oct + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
		    if( _mutr_base_efficiency[index]>=0 )
		      mMutResponse_par->set_chamber_efficiency( arm, station, gap, pla, oct, hoct, _mutr_base_efficiency[index]  );
		}
      
      if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) ) {
        MUTOO::PRINT( cout, "MC background parameters" );
        mMutResponse_par->print();
      }


    } else if(_mode == MC_SIGNAL_REAL_BG || _mode == REAL_SIGNAL_REAL_BG) {

      // Background hits are read from BACKGROUND node
      TMutNode<TMutHitMap>::new_dst_input_node(_ioc_background_node, "TMutHitMap", _background_node, "TMutHit");

      // Background hits are read from BACKGROUND node
      TMutNode<TMuiHitMapO>::new_dst_input_node(_ioc_background_node, "TMuiHitMapO", _background_node, "TMuiHitO");

      if (_mode == REAL_SIGNAL_REAL_BG) {

        // Signal hits are read from SIGNAL node
        TMutNode<TMutHitMap>::new_dst_input_node(_ioc_signal_node, "TMutHitMap", _signal_node, "TMutHit");

        // signal hits are read from signal node
        TMutNode<TMuiHitMapO>::new_dst_input_node(_ioc_signal_node, "TMuiHitMapO", _signal_node, "TMuiHitO");
      }

    } else if(_mode == MC_SIGNAL_NO_BG) {

      // Create the BG map anyway so downstream stuff doesn't have to deal
      // with the null pointer (its there but it will always be empty)
      TMutNode<TMutHitMap>::new_node(_ioc_background_node, "TMutHitMap");
      TMutNode<TMuiHitMapO>::new_node(_ioc_background_node, "TMuiHitMapO");
    }

    // Embedding module combines signal and background hits
    TMutHitMap* hit_map = TMutNode<TMutHitMap>::new_node(_mutoo_node, "TMutHitMap");
    TMuiHitMapO* muid_hit_map = TMutNode<TMuiHitMapO>::new_node(_muioo_node, "TMuiHitMapO");

    // Merged Hit Map written to output DST
    if (dst_node)
    {
      hit_map->make_persistant(dst_node,"TMutHit");
      muid_hit_map->make_persistant(dst_node,"TMuiHitO");
    }

    // signal muid response
    mMuiResponsePar* mMuiResponse_par = TMutNode<mMuiResponsePar>::new_node(_ioc_signal_node,"mMuiResponsePar");
    mMuiResponse_par->set_verbosity(MUIOO::NONE);

    // signal mutr response
    mMutResponsePar* mMutResponse_par = TMutNode<mMutResponsePar>::new_node(_ioc_signal_node,"mMutResponsePar");
    mMutResponse_par->set_verbosity(MUTOO::NONE);
    if( get_flag( NO_CHARGE_SMEAR ) ) mMutResponse_par->set_smear_q( false );
    if( get_flag( NO_RMS_SCALE ) ) mMutResponse_par->set_use_rms_scale( false );

    for(int arm=0; arm<MUTOO::NumberOfArms; arm++)
    for(int station=0; station <MUTOO::NumberOfStations; station ++)
    for(int gap=0; gap < MUTOO::NumberOfGaps; gap++)
    for(int pla=0; pla < MUTOO::NumberOfPlanes; pla++)
	    for (int oct=0; oct<MUTOO::MAX_OCTANT; oct++)
	      for (int hoct=0; hoct<MUTOO::MAX_HALF_OCTANT; hoct++)
    {
      if(station == 2 && gap == 2) continue;
      int index = pla + MUTOO::NumberOfPlanes*(hoct + MUTOO::MAX_HALF_OCTANT *(oct + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
      if( _mutr_base_efficiency[index]>=0 )
	{ mMutResponse_par->set_chamber_efficiency( arm, station, gap, pla, oct, hoct, _mutr_base_efficiency[index]  ); }

    }

    // zero suppression (goes to the top node as it is applied to merged hits)
    mMutZeroSupPar *mMutZeroSup_par = TMutNode<mMutZeroSupPar>::new_node( _mutoo_node, "mMutZeroSupPar" );

    /*
       calibration module parameters fall under the mutoo_node since the calibration module
       is ran from here after embedment
    */
    mMutCalibratePar* mMutCalibrate_par = TMutNode<mMutCalibratePar>::new_node(_mutoo_node,"mMutCalibratePar");
    mMutCalibrate_par->set_verbosity(MUTOO::NONE);

    // Default parameters are defined in parameter table constructor initialization list.
    // Change the default values here.
    if( recoConsts::instance()->get_IntFlag("PRINT_MUTOO_PARAMETERS", 1) )
    {
      MUTOO::PRINT( cout, "MC signal parameters" );
      mMutResponse_par->print();
      mMutZeroSup_par->print();
      mMutCalibrate_par->print();
    }

  } catch(exception& e) {

    MUTOO::TRACE(e.what());

  }
  return 0;
}


//______________________________________________________
int MuonUnpackSim::process_event(PHCompositeNode *top_node)
{

  _timer.get()->restart();

  // Call the response
  try {

    // Reset the node pointers
    SetNodePtrs(top_node);

    /*
      load external vertex
      load the vertex either from the signal node, the background node, or the top node
      (will give you the reconstructed vertex), according to the setting of vtx_to_use
    */
    if (_vtx_to_use == SIGNAL)
      load_vertex_if_needed( _signal_node );
    else if (_vtx_to_use == BACKGROUND )
      load_vertex_if_needed( _background_node );
    else if (_vtx_to_use == RECO )
      load_vertex_if_needed( top_node );
    else
      cout << "Invalid _vtx_to_use in MuonUnpackSim. No vertex will be loaded into TMutExtVtx" << endl;

    // vertex matching
    if( _mode == MC_SIGNAL_REAL_BG )
    { MuonUtil::check_vertex_matching( _signal_node, _background_node, top_node ); }

    // read signal nodes
    if (_mode == REAL_SIGNAL_REAL_BG) {

      TMutNode<TMutHitMap>::find_node(_ioc_signal_node,"TMutHitMap")->read_array(_signal_node);
      TMutNode<TMuiHitMapO>::find_node(_ioc_signal_node,"TMuiHitMapO")->read_array(_signal_node);

    } else {

      TMutNode<TMuiMCHitMapO>::find_node(_ioc_signal_node,"TMuiMCHitMapO")->read_array(_signal_node);
      TMutNode<TMutMCHitMap>::find_node(_ioc_signal_node,"TMutMCHitMap")->read_array(_signal_node);
      TMutNode<TMutMCTrkMap>::find_node(_ioc_signal_node,"TMutMCTrkMap")->read_array(_signal_node);

      if(!get_flag( NO_MC_PRIMARY) )
      {
        TMutNode<TMCPrimaryMap>::find_node(_ioc_signal_node,"TMCPrimaryMap")->read_array(_signal_node);
      }

    }

    // Note: The more complicated node tree topology means that PHMapManager::read will
    // not do the right thing here. We fill the maps from the DST objects by hand by
    // calling read_array
    if(_mode == MC_SIGNAL_REAL_BG || _mode == REAL_SIGNAL_REAL_BG) {

      TMutNode<TMutHitMap>::find_node(_ioc_background_node,"TMutHitMap")->read_array(_background_node);
      TMutNode<TMuiHitMapO>::find_node(_ioc_background_node,"TMuiHitMapO")->read_array(_background_node);

    } else if(_mode==MC_SIGNAL_MC_BG) {

      // TMuiMCHit, TMutMCHit, TMutMCTrk from background DST
      TMutNode<TMuiMCHitMapO>::find_node(_ioc_background_node,"TMuiMCHitMapO")->read_array(_background_node);
      TMutNode<TMutMCHitMap>::find_node(_ioc_background_node,"TMutMCHitMap")->read_array(_background_node);
      TMutNode<TMutMCTrkMap>::find_node(_ioc_background_node,"TMutMCTrkMap")->read_array(_background_node);

      // Response module in on background MC input
      if( !get_flag( SKIP_MUTOO ) ) _mMutResponse_mod.event( _ioc_background_node );
      if( !get_flag( SKIP_MUIOO ) ) _mMuiResponse_mod.event( _ioc_background_node );

    }

    // Response module in on signal MC input
    if( ( !get_flag( SKIP_MUTOO ) ) && _mode != REAL_SIGNAL_REAL_BG) _mMutResponse_mod.event( _ioc_signal_node );
    if( ( !get_flag( SKIP_MUIOO ) ) && _mode != REAL_SIGNAL_REAL_BG) _mMuiResponse_mod.event( _ioc_signal_node );

    // Embedding module get the top node since it
    // needs access to all signal, monte-carlo and
    // merged hit maps.
    _mMuiEmbed_mod.event( _ioc_signal_node, _ioc_background_node, top_node);
    _mMutEmbed_mod.event( _ioc_signal_node, _ioc_background_node, top_node);

    if( !get_flag( NO_ZERO_SUP ) ) _mMutZeroSup_mod.event( _mutoo_node );
    if( !get_flag( NO_CALIBRATE ) ) _mMutCalibrate_mod.event( _mutoo_node );

  } catch (exception& e) {
    MUTOO::TRACE(e.what());
  }

  /*
     write the maps to the DST node, if needed.
     this does not conflict with the other modules since PHMapManager
     is protected against multiple calls
  */
  write_maps_if_needed();

  _timer.get()->stop();
  return 0;
}

//______________________________________________________
int MuonUnpackSim::End(PHCompositeNode* top_node)
{

  // print this module timer statistics
//   _timer.get()->print_stat();

  // dump modules summary
  if( !get_flag( SKIP_MUIOO ) ) _mMuiEmbed_mod.print_summary();
  if( !get_flag( SKIP_MUTOO ) ) _mMutEmbed_mod.print_summary();
  if( !get_flag( NO_ZERO_SUP ) ) _mMutZeroSup_mod.print_summary();

  return 0;
}
//______________________________________________________
void MuonUnpackSim::SetMode(unsigned int mode)
{
  _mode = mode;
  boost::array<const char*,5> mode_string = {{
    "MC_SIGNAL_REAL_BG",
    "MC_SIGNAL_MC_BG",
    "MC_SIGNAL_NO_BG",
    "REAL_SIGNAL_REAL_BG"}};

  MUTOO::PRINT(cout, "MuonUnpackSim SetMode");
  cout << "Mode set to " << mode_string[_mode] << endl;
  MUTOO::PRINT(cout, "**");

}
//______________________________________________________
void MuonUnpackSim::SetVtxToUse(unsigned int vtx_to_use)
{
  _vtx_to_use = vtx_to_use;
  boost::array<const char*,5> vtx_to_use_string = {{
    "SIGNAL",
    "BACKGROUND",
    "RECO"}};

  MUTOO::PRINT(cout, "MuonUnpackSim SetVtxToUse");
  cout << "VtxToUse set to " << vtx_to_use_string[_vtx_to_use] << endl;
  MUTOO::PRINT(cout, "**");

}

//______________________________________________________
void MuonUnpackSim::set_bbcrate_dependent_efficiency( const std::string file )
{
  ifstream fin(file.c_str());
  int arm=-1, station=-1, gap=-1, plane=-1, octant=-1, half_octant=-1;
  float eff0=-999., eff1=-999.;
  cout << PHWHERE << "----- BBC rate dependent MuTr chamber efficiency ---" << endl;
  while (fin >> arm >> station >> gap >> plane >> octant >> half_octant >> eff0 >> eff1)
    {
      int index = plane + MUTOO::NumberOfPlanes*(half_octant + MUTOO::MAX_HALF_OCTANT *(octant + MUTOO::MAX_OCTANT *(gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm ))));
      _chamber_eff0[index] = eff0;
      _chamber_eff1[index] = eff1;
      cout << index << " " << eff0 << "+" << eff1 << "*bbcrate" << endl;
    }
  cout << "--------------------" << endl;
  fin.close();

  _check_bbc_rate = true;
}
