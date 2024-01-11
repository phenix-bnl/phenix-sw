// $Id: FvtxMCEval.cxx,v 1.70 2018/06/26 15:04:28 shlim Exp $

/*!
  \file FvtxMCEval.cxx
  \ingroup supermodules
  \brief reads TMutMCtrkMap and create TFvtxMCHit accordingly
  \author Hugo Pereira
  \version $Revision: 1.70 $
  \date $Date: 2018/06/26 15:04:28 $
  \
*/

#include <cmath>
#include <numeric>
#include <boost/bind.hpp>
#include <boost/mem_fn.hpp>

#include <MuonUtil.h>

#include <FVTXOO.h>
#include <PHTFileServer.h>
#include <PHTimer.h>
#include <recoConsts.h>
#include <TMutTrkMap.h>
#include <TMutHitMap.h>
#include <TMutCoordMap.h>
#include <TFvtxMCHitMap.h>
#include <TMutMCHitMap.h>
#include <TMutMCTrkMap.h>
#include <TMuiHitMapO.h>
#include <TMuiRoadMapO.h>
#include <TMuiClusterMapO.h>
#include <MuiCommon.hh>
#include <TFvtxTrkMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxClusMap.h>
#include <TFvtxCoordMap.h>
#include <TFvtxHitMap.h>
#include <TFvtxPisaHitMap.h>
#include <TFvtxEvalMap.h>
#include <TFvtxStraightTrkParMap.h>
#include <TFvtxSvxClusterMap.h>
#include <fkinWrapper.h>
#include <headerWrapper.h>
#include <pythiaWrapper.h>
#include <primaryWrapper.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <PHPythiaContainerV2.h>
#include <TMCParticle.h>
#include <VtxOut.h>
#include<TMutExtVtx.h>
//#include <PHMuoTracksv11.h>
//#include <TMutGapCoordMap.h>
//#include <TMutCoordMap.h>
//#include <MWGCuts.h>
//#include <Tools.h>

#include <PHCylPoint.h>
#include <PHGeometry.h>

#include <FvtxGeom.h>
#include "FvtxMCEval.h"

#include <mFvtxEval.h>

typedef PHIODataNode<PHTable> TableNode_t;

using namespace std;
using namespace findNode;

int geant2pdgId(const int id);

// Analytically perform a linear fit, using constant weights
//
template<typename T> std::pair<T,T>
linearFit(const std::vector<T>& x, const std::vector<T>& y)
{
  int n = x.size();

  std::vector<T> x2(x.size());
  std::vector<T> xy(x.size());
  std::transform(x.begin(),x.end(),x.begin(),x2.begin(),std::multiplies<T>());
  std::transform(x.begin(),x.end(),y.begin(),xy.begin(),std::multiplies<T>());

  T sumX  = std::accumulate(x.begin(),x.end(),0.0);
  T sumXY = std::accumulate(xy.begin(),xy.end(),0.0);
  T sumY  = std::accumulate(y.begin(),y.end(),0.0);
  T sumX2 = std::accumulate(x2.begin(),x2.end(),0.0);
  T delta = n*sumX2 - sumX*sumX;

  T slope  = 1.0/delta * (n*sumXY - sumX*sumY);
  T offset = 1.0/delta * (sumX2*sumY - sumX*sumXY);

  return std::pair<T,T>(slope,offset);
}

//______________________________________________________
FvtxMCEval::FvtxMCEval( const char* name, const char* file ) :
  SubsysReco( name ),
  _fit_model(0),
  _mc_radiograph_tree( 0 ),
  _event(0),
  _trk_eval_tree(0),
  _mc_trk_eval_tree(0),
  _mc_traj_eval_tree(0),
  _mc_cluster_tree(0),
  _mc_fvtx_hit_tree(0),
  _event_tree(0),
  _coord_eval_tree(0),
  _match_eval_tree(0),
  _straight_trk_eval_tree(0),
  _timer( new PHTimer(name) ),
  _file_name( file ),
	_do_save_file( false ),
  _signalNodeName("SIGNAL"), //_signalNodeName("TOP"),
  _fvtxNodeName("TOP"),
  _signal_top_node(0),
  _fvtx_mc_hit_map(0),
  _mut_mc_hit_map(0),
  _mut_mc_trk_map(0),
  _fvtx_trk_map(0),
  _fvtx_hit_map(0),
  _fvtx_clus_map(0),
  _fvtx_coord_map(0),
  _do_fkin_eval(false),
  _do_header_eval(false),
  _do_pythia_eval(false),
  //_do_primary_eval(false),
  _do_match_mod(false)
{}

//______________________________________________________
FvtxMCEval::~FvtxMCEval()
{}

//_____________________________________
int FvtxMCEval::Init(PHCompositeNode *top_node)
{
  FVTXOO::PRINT( cout, "FvtxMCEval::Init" );

  // Interface Object Containers (IOCS)
  TMutNode<TFvtxEvalMap>::new_node( top_node, "TFvtxEvalMap");

  // Module parameter tables
  TMutNode<mFvtxEvalPar>::new_node( top_node,"mFvtxEvalPar");
  TMutNode<mFvtxEvalPar>::find_node( top_node,"mFvtxEvalPar");

  //! open TFile
	if ( _do_save_file )
	{
		PHTFileServer::get().open( _file_name, "RECREATE" );
		cout << "FvtxMCEval::Init: writing to file \"" << _file_name << "\"" << endl;

		_eval_trees.clear();

		book_mc_radiograph_tree();
		FVTXOO::PRINT( cout, "**" );

		book_trk_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		book_mc_trk_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		book_mc_traj_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		book_mc_cluster_tree();
		FVTXOO::PRINT( cout, "**" );

		book_mc_fvtx_hit_tree();
		FVTXOO::PRINT( cout, "**" );

		book_event_tree();
		FVTXOO::PRINT( cout, "**" );

		book_coord_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		book_match_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		book_straight_trk_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		if ( _do_fkin_eval ) book_fkin_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		if ( _do_header_eval ) book_header_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		if ( _do_pythia_eval ) book_pythia_eval_tree();
		FVTXOO::PRINT( cout, "**" );

		//if ( _do_primary_eval ) book_primary_eval_tree();
		book_primary_eval_tree();
	}
  FVTXOO::PRINT( cout, "**" );

  return 0;
}

//_____________________________________
int FvtxMCEval::InitRun(PHCompositeNode *top_node)
{
  _event = 0;

  // Instantiate FVTXOO analysis modules
  _mFvtxEvalMod = new mFvtxEval();
  FVTXOO::TRACE("Done with setup_modules ");

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "FvtxMCEval::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  cout << "FvtxMCEval::InitRun - _signalNodeName : " << _signalNodeName << endl;

  return 0;
}


//______________________________________________________
int FvtxMCEval::process_event(PHCompositeNode *top_node)
{
  _event++;

  _timer->restart();

  try {

    set_interface_pointers( top_node );

    // Run the fvtxoo evaluation module
    if ( do_match_mod() ) _mFvtxMatchMod.event( _signal_top_node, top_node );
    else _mFvtxEvalMod->event( _signal_top_node, top_node );

    // Fill the primary filling first since there may be calculations
    // the follow which depend on the primary particle info.
		if ( _do_save_file )
		{
			fill_primary_eval_tree();
			fill_mc_radiograph_tree();
			fill_trk_eval_tree();
			fill_mc_trk_eval_tree();
			fill_mc_traj_eval_tree();
			fill_mc_cluster_tree();
			fill_mc_fvtx_hit_tree();
			fill_event_tree();
			fill_coord_eval_tree();
			fill_match_eval_tree();
			fill_straight_trk_eval_tree();
			if ( _do_fkin_eval ) fill_fkin_eval_tree();
			if ( _do_header_eval ) fill_header_eval_tree();
			if ( _do_pythia_eval ) fill_pythia_eval_tree();
			//if ( _do_primary_eval ) fill_primary_eval_tree();
		}

  } catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  _timer->stop();
  return 0;
}

//______________________________________________________
int FvtxMCEval::End(PHCompositeNode* top_node)
{
  _timer->print_stat();

  // Also save the trees
  //
  // Boost fun galore: the following one-liner executes the equivalent of
  //
  // For each ptr_to_TTree:
  //   TTree::AutoSave(ptr_to_TTree,opt);
  //
  //Option_t* opt = "";
  //std::for_each(_eval_trees.begin(),_eval_trees.end(),boost::bind(&TTree::AutoSave,_1,opt));
  if ( _mc_radiograph_tree ) _mc_radiograph_tree->AutoSave();
  if ( _mc_trk_eval_tree ) _mc_trk_eval_tree->AutoSave();
  if ( _trk_eval_tree ) _trk_eval_tree->AutoSave();
  if ( _mc_traj_eval_tree ) _mc_traj_eval_tree->AutoSave();
  if ( _mc_cluster_tree ) _mc_cluster_tree->AutoSave();
  if ( _mc_fvtx_hit_tree ) _mc_fvtx_hit_tree->AutoSave();
  if ( _event_tree ) _event_tree->AutoSave();
  if ( _fkin_eval_tree ) _fkin_eval_tree->AutoSave();
  if ( _header_eval_tree ) _header_eval_tree->AutoSave();
  if ( _pythia_eval_tree ) _pythia_eval_tree->AutoSave();
  if ( _primary_eval_tree ) _primary_eval_tree->AutoSave();
  if ( _coord_eval_tree ) _coord_eval_tree->AutoSave();
  if ( _match_eval_tree ) _match_eval_tree->AutoSave();
  if ( _straight_trk_eval_tree ) _straight_trk_eval_tree->AutoSave();
  return 0;
}

//______________________________________________________
void FvtxMCEval::set_interface_pointers( PHCompositeNode* top_node )
{
  _top_node = top_node;
  _fvtx_node = Fun4AllServer::instance()->topNode( _fvtxNodeName );
  _signal_top_node = Fun4AllServer::instance()->topNode( _signalNodeName );

  // maps
  // Assume that MC info can only be found on the "signal" node (when embedding a
  // MC signal into real background, for instance), and reconstruction info is
  // in the fvtx_node:
  try { _mut_mc_hit_map = TMutNode<TMutMCHitMap>::find_node( _signal_top_node, "TMutMCHitMap" ); }
  catch (const std::exception& e) { if ( verbosity ) FVTXOO::TRACE(e.what()); }

  try { _mut_mc_trk_map = TMutNode<TMutMCTrkMap>::find_node( _signal_top_node, "TMutMCTrkMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_mc_hit_map = TMutNode<TFvtxMCHitMap>::find_node( _signal_top_node, "TFvtxMCHitMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _vtx_mc_hit_map = TMutNode<TFvtxPisaHitMap>::find_node( _signal_top_node, "TFvtxPisaHitMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try {  _fvtx_trk_map = TMutNode<TFvtxTrkMap>::find_node( _fvtx_node, "TFvtxTrkMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_hit_map = TMutNode<TFvtxHitMap>::find_node( _fvtx_node, "TFvtxHitMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_clus_map = TMutNode<TFvtxClusMap>::find_node( _fvtx_node, "TFvtxClusMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_coord_map = TMutNode<TFvtxCoordMap>::find_node( _fvtx_node, "TFvtxCoordMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_eval_map = TMutNode<TFvtxEvalMap>::find_node( _fvtx_node, "TFvtxEvalMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // fkin node
  try { _fkin_ptr = TMutNode<fkinWrapper>::find_io_node( _signal_top_node, "fkin" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // header node
  try { _header_ptr = TMutNode<headerWrapper>::find_io_node( _signal_top_node, "header" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // primary node
  try { _primary_ptr = TMutNode<primaryWrapper>::find_io_node( _signal_top_node, "primary" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // pythia node
  try { _pythia_ptr = MuonUtil::find_io_node<pythiaWrapper>( "pythia" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // PHPythia node
  try { _phpythia_node = MuonUtil::find_io_node<PHPythiaContainer>("PHPythia"); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // vtxout node
  try { _vtxout_node = MuonUtil::find_io_node<VtxOut>( "VtxOut" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // muid node
  try { _road_mapO = TMutNode<TMuiRoadMapO>::find_node(_top_node,"TMuiRoadMapO"); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

}

//______________________________________________________
void FvtxMCEval::book_mc_radiograph_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _mc_radiograph_tree = new TTree( "mc_radiograph", "mc_radiograph" );
  _mc_radiograph_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "column", &_column, "column/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "size", &_size, "size/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "strip", &_strip[0], "strip[size]/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "strip_x_begin", &_strip_x_begin[0], "strip_x_begin[size]/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "strip_y_begin", &_strip_y_begin[0], "strip_y_begin[size]/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "strip_x_end", &_strip_x_end[0], "strip_x_end[size]/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "strip_y_end", &_strip_y_end[0], "strip_y_end[size]/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "d", &_d[0], "d[size]/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "q", &_q[0], "q[size]/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "eloss", &_eloss, "eloss/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "hit_id", &_hit_id, "hit_id/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "pid", &_pid, "pid/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "track_id", &_track_id, "track_id/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "parent_pid", &_parent_pid, "parent_pid/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "parent_track_id", &_parent_track_id, "parent_track_id/I", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "x", &_x, "x/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "y", &_y, "y/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "z", &_z, "z/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "px", &_px0mc, "px/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "py", &_py0mc, "py/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "pz", &_pz0mc, "pz/D", BUFFER_SIZE );
  _mc_radiograph_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _mc_radiograph_tree->SetAutoSave( AUTO_SAVE );

  cout << "_mc_radiograph_tree booked" << endl;

  _eval_trees.push_back(_mc_radiograph_tree);

}
//______________________________________________________
void FvtxMCEval::fill_mc_radiograph_tree( void )
{
  static int ievent = 0;
  ievent++;

  // loop over TFvtxMCHits
  if( _fvtx_mc_hit_map )
  {
    TFvtxMCHitMap::iterator iter( _fvtx_mc_hit_map->range() );
    while( TFvtxMCHitMap::const_pointer ptr = iter.next() )
    {

      // location
      _arm = ptr->get()->get_arm();
      _cage = ptr->get()->get_cage();
      _station = ptr->get()->get_station();
      _sector = ptr->get()->get_sector();
      _plane = ptr->get()->get_sector() % 2;
      _column = ptr->get()->get_column();
      _x = ptr->get()->get_x();
      _y = ptr->get()->get_y();
      _z = ptr->get()->get_z();

      _px0mc = ptr->get()->get_px();
      _py0mc = ptr->get()->get_py();
      _pz0mc = ptr->get()->get_pz();

      _hit_id = ptr->get()->get_index();
      _pid = 0;
      _parent_pid = 0;

      // get associated MC trk
      TMutMCTrkMap::key_iterator trk_iter = ptr->get()->get_associated<TMutMCTrk>();
      if( trk_iter.count() ) {
	_track_id = trk_iter.current()->get()->get_track_id();
	_pid = trk_iter.current()->get()->get_pidG4();
	_parent_pid = trk_iter.current()->get()->get_parent_id();
	_parent_track_id = trk_iter.current()->get()->get_parent_track_id();
      }

      _eloss = ptr->get()->get_eloss();

      // retrieve number of strips in cluster
      _size = min<int>( max_size, ptr->get()->get_n_strip() );

      // initialize arrays
      _q.assign(0);
      _strip.assign(0);
      _strip_x_begin.assign(0);
      _strip_y_begin.assign(0);
      _strip_x_end.assign(0);
      _strip_y_end.assign(0);
      _d.assign(0);

      // get associated MC strips
      for( int i=0; i < _size; i++ )
      {
        try {
          _strip[i] = ptr->get()->get_strip(i)->get_strip();
        } catch (const std::exception& e) {
          std::ostringstream s;
          s << "FvtxMCEval::fill_mc_radiograph_tree: Exception during get_strip(" << i << "): "
            << e.what();
          FVTXOO::TRACE(s.str());
          continue;
        }

        // retrieve geometry for fired strip
        int arm = ptr->get()->get_arm();
        int cage = ptr->get()->get_cage();
        int sta = ptr->get()->get_station();
        int sec = ptr->get()->get_sector();
        int col = ptr->get()->get_column();
        // Fetch the column separately as we are trying to debug the strip exception
        const FvtxColumn* col_ptr = FvtxGeom::get_arm( ptr->get()->get_arm() )
          ->get_cage( ptr->get()->get_cage() )
          ->get_station( ptr->get()->get_station() )
          ->get_sector( ptr->get()->get_sector() )
          ->get_column( ptr->get()->get_column() );
        try {
          const FvtxStrip* strip = col_ptr->get_strip( ptr->get()->get_strip(i)->get_strip() );
          if( !strip ) {
            cout << "FvtxMCEval::fill_mc_radiograph_tree - unable to retrieve strip." << endl;
            continue;
          }

          // retrieve strip begin and end x,y positions
          _strip_x_begin[i] = strip->get_position_begin().getX();
          _strip_y_begin[i] = strip->get_position_begin().getY();
          _strip_x_end[i] = strip->get_position_end().getX();
          _strip_y_end[i] = strip->get_position_end().getY();

          // strip vector
          PHVector strip_vect( strip->get_position_end() - strip->get_position_begin() );
          strip_vect.setZ(0);
          strip_vect.normalize();

          // point vector
          PHVector point_vect( PHPoint( _x, _y, _z ) - strip->get_position_begin() );
          point_vect.setZ(0);

          // distance from point to strip vector
          _d[i] = point_vect.cross(strip_vect).getZ();

          // charge on strip
          _q[i] = ptr->get()->get_strip(i)->get_q();

	} catch (const std::exception& e) {
          std::ostringstream s;
          s << "FvtxMCEval::fill_mc_radiograph_tree: Exception while processing strip "
            << i+1 << "/" << _size << " ("
            << arm << "," << cage << "," << sta << "," << sec << "," << col << "," << _strip[i]
            << "): " << e.what() << std::endl;
          col_ptr->print(s);
          FVTXOO::TRACE(s.str());
          continue;
        }
      }

      if( _mc_radiograph_tree ) _mc_radiograph_tree->Fill();

    }
  }


  if( _mut_mc_hit_map )
  {
    // loop over TMutMCHits
    _size = 0;
    _q.assign(0);
    _strip.assign(-999);

    TMutMCHitMap::iterator iter_mut( _mut_mc_hit_map->range() );
    while( TMutMCHitMap::const_pointer ptr_mut = iter_mut.next() )
    {

      // location
      _arm = ptr_mut->get()->get_arm();
      _station = ptr_mut->get()->get_station();
      _sector = 2*ptr_mut->get()->get_octant()+ptr_mut->get()->get_half_octant();
      _plane = ptr_mut->get()->get_gap();
      _column = 9999;
      _x = ptr_mut->get()->get_x();
      _y = ptr_mut->get()->get_y();
      _z = ptr_mut->get()->get_z();
      _px0mc = ptr_mut->get()->get_px();
      _py0mc = ptr_mut->get()->get_py();
      _pz0mc = ptr_mut->get()->get_pz();

      // get associated MC trk
      TMutMCTrkMap::key_iterator trk_iter = ptr_mut->get()->get_associated<TMutMCTrk>();
      if( trk_iter.count() ) {
	_track_id = trk_iter.current()->get()->get_track_id();
	_pid = trk_iter.current()->get()->get_pidG4();
	_parent_pid = trk_iter.current()->get()->get_parent_id();
	_parent_track_id = trk_iter.current()->get()->get_parent_track_id();
      }

      if( _mc_radiograph_tree ) _mc_radiograph_tree->Fill();
    }
  }

  return;

}
//______________________________________________________
// Evaluation tree to compare MC track information to reconstructed track information
//
void FvtxMCEval::book_trk_eval_tree( void )
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _trk_eval_tree = new TTree( "trk_eval", "trk_eval" );
  _trk_eval_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x0mc", &_x0mc, "x0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y0mc", &_y0mc, "y0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z0mc", &_z0mc, "z0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "px0mc", &_px0mc, "px0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "py0mc", &_py0mc, "py0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "pz0mc", &_pz0mc, "pz0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x0reco", &_x0reco, "x0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y0reco", &_y0reco, "y0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z0reco", &_z0reco, "z0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "px0reco", &_px0reco, "px0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "py0reco", &_py0reco, "py0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "pz0reco", &_pz0reco, "pz0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chargemc", &_chargemc, "chargemc/F", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chargereco", &_chargereco, "chargereco/F", BUFFER_SIZE );
  _trk_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chisqreco", &_chisqreco, "chisqreco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chisqrecopdf", &_chisqrecopdf, "chisqrecopdf/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x1mc", &_x1mc, "x1mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y1mc", &_y1mc, "y1mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1mc", &_z1mc, "z1mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x2mc", &_x2mc, "x2mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y2mc", &_y2mc, "y2mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2mc", &_z2mc, "z2mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x3mc", &_x3mc, "x3mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y3mc", &_y3mc, "y3mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3mc", &_z3mc, "z3mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x4mc", &_x4mc, "x4mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y4mc", &_y4mc, "y4mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4mc", &_z4mc, "z4mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "ittrk",  &_ittrk,  "ittrk/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "idtrk",  &_idtrk,  "idtrk/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x1reco", &_x1reco, "x1reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y1reco", &_y1reco, "y1reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1reco", &_z1reco, "z1reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x2reco", &_x2reco, "x2reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y2reco", &_y2reco, "y2reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2reco", &_z2reco, "z2reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x3reco", &_x3reco, "x3reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y3reco", &_y3reco, "y3reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3reco", &_z3reco, "z3reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x4reco", &_x4reco, "x4reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y4reco", &_y4reco, "y4reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4reco", &_z4reco, "z4reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x0mcproj", &_x0mcproj, "x0mcproj/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y0mcproj", &_y0mcproj, "y0mcproj/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z0mcproj", &_z0mcproj, "z0mcproj/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "nhitsb", &_nhitsb, "nhitsb/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x1mcb", &_x1mcb, "x1mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y1mcb", &_y1mcb, "y1mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1mcb", &_z1mcb, "z1mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x2mcb", &_x2mcb, "x2mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y2mcb", &_y2mcb, "y2mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2mcb", &_z2mcb, "z2mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x3mcb", &_x3mcb, "x3mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y3mcb", &_y1mcb, "y3mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3mcb", &_z3mcb, "z3mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x4mcb", &_x4mcb, "x4mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y4mcb", &_y4mcb, "y4mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4mcb", &_z4mcb, "z4mcb/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "nhitsf", &_nhitsf, "nhitsf/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "hitsfIds", &_hitstIds[0], "hitsfIds[nhitsf]/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x0recoerr", &_x0recoerr, "x0recoerr/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y0recoerr", &_y0recoerr, "y0recoerr/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "ghost", &_ghost, "ghost/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "recosuc", &_recosuc, "recosuc/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coord1", &_coord1, "coord1/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coord2", &_coord2, "coord2/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coord3", &_coord3, "coord3/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coord4", &_coord4, "coord4/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "ncoords", &_ncoords, "ncoords/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coordIds", &_coordIds[0], "coordIds[ncoords]/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coordPhiStart", &_coordPhiStart[0], "coordPhiStart[ncoords]/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coordPhiEnd", &_coordPhiEnd[0], "coordPhiEnd[ncoords]/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coordR", &_coordR[0], "coordPhiEnd[ncoords]/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "coordZ", &_coordZ[0], "coordPhiEnd[ncoords]/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r_slopeFit", &_r_slopeFit, "r_slopeFit/D", BUFFER_SIZE);
  _trk_eval_tree->Branch( "r_offsetFit", &_r_offsetFit, "r_offsetFit/D", BUFFER_SIZE);
  _trk_eval_tree->Branch( "ncoordsShared", &_ncoordsShared, "ncoordsShared/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "pxmutr", &_pxmutr, "pxmutr/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "pymutr", &_pymutr, "pymutr/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "pzmutr", &_pzmutr, "pzmutr/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chi2mutr", &_chi2mutr, "chi2mutr/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "evtx0mc", &_evtx0mc, "evtx0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "evty0mc", &_evty0mc, "evty0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "evtz0mc", &_evtz0mc, "evtz0mc/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_charge",   &_mu_charge,   "mu_charge/D",   BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_px",   &_mu_px,   "mu_px/D",   BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_py",   &_mu_py,   "mu_py/D",   BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_pz",   &_mu_pz,   "mu_pz/D",   BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_p",    &_mu_p,    "mu_p/D",    BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_pt",   &_mu_pt,   "mu_pt/D",   BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_chi2", &_mu_chi2, "mu_chi2/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_ghost", &_mu_ghost, "mu_ghost/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_lastGap",   &_mu_lastGap,   "mu_lastGap/D",   BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_muIDquad0", &_mu_muIDquad0, "mu_muIDquad0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_muIDchis0", &_mu_muIDchis0, "mu_muIDchis0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_muTRhits0", &_mu_muTRhits0, "mu_muTRhits0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_dS30", &_mu_dS30, "mu_dS30/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_DG0", &_mu_DG0, "mu_DG0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_DDG0", &_mu_DDG0, "mu_DDG0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_dS3ctp0", &_mu_dS3ctp0, "mu_dS3ctp0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_DS0", &_mu_DS0, "mu_DS0/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_mutr_nhits", &_mu_mutr_nhits, "mu_mutr_nhits/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_muid_nhits", &_mu_muid_nhits, "mu_muid_nhits/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "mu_eta", &_mu_eta, "mu_eta/D", BUFFER_SIZE );


  cout << "_trk_eval_tree booked" << endl;

  _eval_trees.push_back(_trk_eval_tree);

}

//______________________________________________________
// Evaluation tree to compare MC track information to reconstructed track information
//
void FvtxMCEval::book_mc_trk_eval_tree( void )
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _mc_trk_eval_tree = new TTree( "mc_trk_eval", "mc_trk_eval" );
  _mc_trk_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "ittrk", &_ittrk, "ittrk/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "idtrk", &_idtrk, "idtrk/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "parent_geantid", &_parent_pid, "parent_geantid/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "parent_pdgid", &_parent_pdgid, "parent_pdgid/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "hasAncB", &_hasAncB, "hasAncB/S", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "hasAncD", &_hasAncD, "hasAncD/S", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "primId", &_primId, "primId/S", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x0mc", &_x0mc, "x0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y0mc", &_y0mc, "y0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z0mc", &_z0mc, "z0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "px0mc", &_px0mc, "px0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "py0mc", &_py0mc, "py0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "pz0mc", &_pz0mc, "pz0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x0reco", &_x0reco, "x0reco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y0reco", &_y0reco, "y0reco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z0reco", &_z0reco, "z0reco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "px0reco", &_px0reco, "px0reco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "py0reco", &_py0reco, "py0reco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "pz0reco", &_pz0reco, "pz0reco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chargemc", &_chargemc, "chargemc/F", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chargereco", &_chargereco, "chargereco/F", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chisqreco", &_chisqreco, "chisqreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chisqrecopdf", &_chisqrecopdf, "chisqrecopdf/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nvtxf", &_nvtxf, "nvtxf/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nvtxmc", &_nvtxmc, "nvtxmc/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsb", &_nhitsb, "nhitsb/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsf", &_nhitsf, "nhitsf/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitst", &_nhitst, "nhitst/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitst_mut", &_nhitst_mut, "nhitst_mut/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsmade_mut", &_nhitsmade_mut, "nhitsmade_mut/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "ncoordst_mut", &_ncoordst_mut, "ncoordst_mut/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "foundmut", &_foundmut, "foundmut/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "hitstIds", &_hitstIds[0], "hitstIds[nhitst]/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "ntracks", &_ntracks, "ntracks/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfound", &_nhitsfound, "nhitsfound/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "hitsfoundIds", &_hitsFoundIds[0], "hitsFoundIds[nhitsfound]/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "coordfoundIds", &_coordFoundIds[0], "coordFoundIds[nhitsfound]/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsShared", &_ncoordsShared, "nhitsShared/I",  BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nstationst", &_nstationst, "nstationst/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nroadS", &_nroadS, "nroadS/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nroadN", &_nroadN, "nroadN/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsN", &_nhitsN, "nhitsN/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsS", &_nhitsS, "nhitsS/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfoundt", &_nhitsfoundt, "nhitsfoundt/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "ncoordsN", &_ncoordsN, "ncoordsN/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "ncoordsS", &_ncoordsS, "ncoordsS/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfoundt1", &_nhitsfoundt1, "nhitsfoundt1/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfoundt2", &_nhitsfoundt2, "nhitsfoundt2/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfoundt3", &_nhitsfoundt3, "nhitsfoundt3/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfoundt4", &_nhitsfoundt4, "nhitsfoundt4/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "recosuc", &_recosuc, "recosuc/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "recosuc_mut", &_recosuc_mut, "recosuc_mut/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mutmatch", &_mutmatch, "mutmatch/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "nhitsfound_mut", &_nhitsfound_mut, "nhitsfound_mut/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "muttrid", &_muttrid, "muttrid/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x1mc", &_x1mc, "x1mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y1mc", &_y1mc, "y1mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z1mc", &_z1mc, "z1mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x2mc", &_x2mc, "x2mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y2mc", &_y2mc, "y2mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z2mc", &_z2mc, "z2mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x3mc", &_x3mc, "x3mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y3mc", &_y3mc, "y3mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z3mc", &_z3mc, "z3mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x4mc", &_x4mc, "x4mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y4mc", &_y4mc, "y4mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z4mc", &_z4mc, "z4mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "pxmutr", &_pxmutr, "pxmutr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "pymutr", &_pymutr, "pymutr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "pzmutr", &_pzmutr, "pzmutr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chi2mutr", &_chi2mutr, "chi2mutr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evtx0mc", &_evtx0mc, "evtx0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evty0mc", &_evty0mc, "evty0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evtz0mc", &_evtz0mc, "evtz0mc/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evtx0error", &_evtx0error, "evtx0error/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evty0error", &_evty0error, "evty0error/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evtz0error", &_evtz0error, "evtz0error/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evtx0smear", &_evtx0smear, "evtx0smear/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evty0smear", &_evty0smear, "evty0smear/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "evtz0smear", &_evtz0smear, "evtz0smear/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_charge",   &_mu_charge,   "mu_charge/D",   BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_px",   &_mu_px,   "mu_px/D",   BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_py",   &_mu_py,   "mu_py/D",   BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_pz",   &_mu_pz,   "mu_pz/D",   BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_p",    &_mu_p,    "mu_p/D",    BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_pt",   &_mu_pt,   "mu_pt/D",   BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_chi2", &_mu_chi2, "mu_chi2/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_ghost", &_mu_ghost, "mu_ghost/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_lastGap",   &_mu_lastGap,   "mu_lastGap/D",   BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_muIDquad0", &_mu_muIDquad0, "mu_muIDquad0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_muIDchis0", &_mu_muIDchis0, "mu_muIDchis0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_muTRhits0", &_mu_muTRhits0, "mu_muTRhits0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_dS30", &_mu_dS30, "mu_dS30/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_DG0", &_mu_DG0, "mu_DG0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_DDG0", &_mu_DDG0, "mu_DDG0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_dS3ctp0", &_mu_dS3ctp0, "mu_dS3ctp0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_DS0", &_mu_DS0, "mu_DS0/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_mutr_nhits", &_mu_mutr_nhits, "mu_mutr_nhits/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_muid_nhits", &_mu_muid_nhits, "mu_muid_nhits/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mu_eta", &_mu_eta, "mu_eta/D", BUFFER_SIZE );


  // Add some Straight-line fit results for the "best" associated reco track
  _mc_trk_eval_tree->Branch( "x0sl", &_x0slreco, "x0sl/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y0sl", &_y0slreco, "y0sl/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mxsl", &_mxslreco, "mxsl/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mysl", &_myslreco, "mysl/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x0slerr", &_x0slerr, "x0slerr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y0slerr", &_y0slerr, "y0slerr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "mxslerr", &_mxslerr, "mxslerr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "myslerr", &_myslerr, "myslerr/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chi2sl", &_chi2slreco, "chi2sl/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "ndfsl", &_ndfslreco, "ndfsl/I", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "x0mutreco", &_x0mutreco, "x0mutreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "y0mutreco", &_y0mutreco, "y0mutreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "z0mutreco", &_z0mutreco, "z0mutreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "px0mutreco", &_px0mutreco, "pxmutreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "py0mutreco", &_py0mutreco, "pymutreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "pz0mutreco", &_pz0mutreco, "pzmutreco/D", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chargemutreco", &_chargemutreco, "chargemutreco/S", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chisqmutreco", &_chisqmutreco, "chisqmutreco/F", BUFFER_SIZE );
  _mc_trk_eval_tree->Branch( "chisqmutrecopdf", &_chisqmutrecopdf, "chisqmutrecopdf/F", BUFFER_SIZE );

  std::cout << "_mc_trk_eval_tree booked" << std::endl;

  _eval_trees.push_back(_mc_trk_eval_tree);
}

//______________________________________________________
// Evaluation tree to compare MC track information to reconstructed track information
//
void FvtxMCEval::book_mc_traj_eval_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _mc_traj_eval_tree = new TTree( "mc_traj_eval", "mc_traj_eval" );
  _mc_traj_eval_tree->Branch( "xmc", &_xmc, "xmc/D", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "ymc", &_ymc, "ymc/D", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "zmc", &_zmc, "zmc/D", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "xreco", &_xreco, "xreco/D", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "yreco", &_yreco, "yreco/D", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "zreco", &_zreco, "zreco/D", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _mc_traj_eval_tree->Branch( "itrack", &_itrack, "itrack/I", BUFFER_SIZE );
  cout << "_mc_traj_eval_tree booked" << endl;

  _eval_trees.push_back(_mc_traj_eval_tree);

}
//______________________________________________________
void FvtxMCEval::fill_trk_eval_tree( void )
{
  static int ievent = 0;
  ievent++;

  _evtx0mc = _evty0mc = _evtz0mc = -9999.0;

  if ( _vtxout_node )
  {
    //cout << "vertex (" << (_vtxout_node->get_Vertex()).getX() << ", " << (_vtxout_node->get_Vertex()).getY() << ", " << (_vtxout_node->get_Vertex()).getZ() << ")" << endl;
    //cout << "Simvertex " << (_vtxout_node->get_Vertex("SIM")).getX() << ", " << (_vtxout_node->get_Vertex("SIM")).getY() << ", " << (_vtxout_node->get_Vertex("SIM")).getZ() << endl;
    _evtx0mc = (_vtxout_node->get_Vertex("SIM")).getX();
    _evty0mc = (_vtxout_node->get_Vertex("SIM")).getY();
    _evtz0mc = (_vtxout_node->get_Vertex("SIM")).getZ();
  }
  else
  {
    cout << "_vtxout_node not found " << endl;
  }

  // loop over TFvtxMCHits
  if( _fvtx_trk_map )
  {
    TFvtxTrkMap::iterator iter( _fvtx_trk_map->range() );
    while( TFvtxTrkMap::const_pointer ptr = iter.next() )
    {

      _arm = -9999;
      _x0mc = _y0mc = _z0mc = -9999.0;
      _px0mc = _py0mc = _pz0mc = -9999.0;
      _x0reco = _y0reco = _z0reco = -9999.0;
      _px0reco = _py0reco = _pz0reco = -9999.0;
      _chargemc = 0.0;
      _chargereco = 0.0;
      //      _event = 0;
      _chisqreco = -9999.0;
      _chisqrecopdf = -9999.0;
      _x1mc = _y1mc = _z1mc = -9999.0;
      _x2mc = _y2mc = _z2mc = -9999.0;
      _x3mc = _y3mc = _z3mc = -9999.0;
      _x4mc = _y4mc = _z4mc = -9999.0;
      _ittrk = 99999; _idtrk = 99999;
      _x1reco = _y1reco = _z1reco = -9999.0;
      _x2reco = _y2reco = _z2reco = -9999.0;
      _x3reco = _y3reco = _z3reco = -9999.0;
      _x4reco = _y4reco = _z4reco = -9999.0;
      _nhitsb = _nhitsf = 0;
      _x1mcb = _y1mcb = _z1mcb = -9999.0;
      _x2mcb = _y2mcb = _z2mcb = -9999.0;
      _x3mcb = _y3mcb = _z3mcb = -9999.0;
      _x4mcb = _y4mcb = _z4mcb = -9999.0;
      _x0mcproj = _y0mcproj = _z0mcproj = -9999.0;
      _x0recoerr = _y0recoerr = -9999.0;
      _ncoords = 0;
      _ncoordsShared = 0;
      _parent_pdgid = 0;
      _pxmutr = _pymutr = _pzmutr = -9999.0;
      _chi2mutr = -9999.0;

//       if (ptr->get()->get_w_chi_square() > 10.0
//             || !ptr->get()->get_reco_success()) continue;
      TFvtxCoordMap::key_iterator trk_coord_iter = ptr->get()->get_associated<TFvtxCoord>();

      //if (trk_coord_iter.count() < 4) continue;

      _coordIds.assign(0);
      _coordPhiStart.assign(0);
      _coordPhiEnd.assign(0);
      _coordR.assign(0);
      _coordZ.assign(0);
      _ncoords = trk_coord_iter.count();

      int cnt = 0;
      std::vector<double> r, z;
      while( TFvtxCoordMap::const_pointer trk_coord_ptr = trk_coord_iter.next() ){
        if ( cnt >= (int)_coordIds.size() )
          {
            std::cout << "fill_trk_eval_tree: Number of fvtx coords("
                      << trk_coord_iter.count() << ") exceeds array bounds, skipping rest"
                      << std::endl;
            break; // protect against overflow
          }

        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station1)
          _coord1 = trk_coord_ptr->get()->get_key().get_obj_key();
        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station2)
          _coord2 = trk_coord_ptr->get()->get_key().get_obj_key();
        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station3)
          _coord3 = trk_coord_ptr->get()->get_key().get_obj_key();
        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station4)
          _coord4 = trk_coord_ptr->get()->get_key().get_obj_key();

	TFvtxTrkMap::key_iterator i = trk_coord_ptr->get()->get_associated<TFvtxTrk>();
	if ( i.count() > 1 ) _ncoordsShared++;

	_coordIds[cnt] = trk_coord_ptr->get()->get_index();
	PHCylPoint pnt0 = trk_coord_ptr->get()->get_coord_begin();
	PHCylPoint pnt1 = trk_coord_ptr->get()->get_coord_end();
	PHCylPoint mid = trk_coord_ptr->get()->get_coord_midpoint();
	_coordPhiStart[cnt] = FVTXOO::angle_normalize(pnt0.getPhi().getPhi());
	_coordPhiEnd[cnt] = FVTXOO::angle_normalize(pnt1.getPhi().getPhi());
	_coordR[cnt] = mid.getR();
	_coordZ[cnt] = mid.getZ();
	r.push_back(_coordR[cnt]);
	z.push_back(_coordZ[cnt]);
	cnt++;
      }

      boost::tie(_r_slopeFit,_r_offsetFit) = linearFit(z,r);

      add_muon_info( ptr->get() );

      // location
      _arm = ptr->get()->get_arm();
      _chargereco = ptr->get()->get_charge();
      _x0reco = ptr->get()->get_trk_par_vtx()->get_x();
      _y0reco = ptr->get()->get_trk_par_vtx()->get_y();
      _z0reco = ptr->get()->get_trk_par_vtx()->get_z();
      _px0reco = ptr->get()->get_trk_par_vtx()->get_px();
      _py0reco = ptr->get()->get_trk_par_vtx()->get_py();
      _pz0reco = ptr->get()->get_trk_par_vtx()->get_pz();
      _pxmutr  = ptr->get()->get_trk_par_mutr()->get_px();
      _pymutr  = ptr->get()->get_trk_par_mutr()->get_py();
      _pzmutr  = ptr->get()->get_trk_par_mutr()->get_pz();
      _chi2mutr = ptr->get()->get_trk_par_mutr()->get_chi_square();

      if (ptr->get()->get_ghost()) _ghost = 1;
      else _ghost = 0;

      _recosuc = ptr->get()->get_reco_success();

      // Get errors on x and y projection:

      _x0recoerr = ptr->get()->get_trk_par_vtx()->get_covar(0,0);
      _y0recoerr = ptr->get()->get_trk_par_vtx()->get_covar(1,1);

      const vector<TMutTrkPar>& trk_par_list( *ptr->get()->get_trk_par_list() );
      for( vector<TMutTrkPar>::const_iterator par_iter = trk_par_list.begin(); par_iter != trk_par_list.end(); par_iter++ ){
        if (fabs(par_iter->get_z()) > 18.0 && fabs(par_iter->get_z()) < 19.5){
          _x1reco = par_iter->get_x();
          _y1reco = par_iter->get_y();
          _z1reco = par_iter->get_z();
        }
        if (fabs(par_iter->get_z()) > 23.0 && fabs(par_iter->get_z()) < 28){
          _x2reco = par_iter->get_x();
          _y2reco = par_iter->get_y();
          _z2reco = par_iter->get_z();
        }
        if (fabs(par_iter->get_z()) > 28.0 && fabs(par_iter->get_z()) < 35.0){
          _x3reco = par_iter->get_x();
          _y3reco = par_iter->get_y();
          _z3reco = par_iter->get_z();
        }
        if (fabs(par_iter->get_z()) > 35.0 && fabs(par_iter->get_z()) < 41.0){
          _x4reco = par_iter->get_x();
          _y4reco = par_iter->get_y();
          _z4reco = par_iter->get_z();
        }
      }

      _chisqreco = ptr->get()->get_w_chi_square();
      _chisqrecopdf = ptr->get()->get_w_chi_square_pdf();
      //      _event = ievent;


      // get associated MC trk
      TMutMCTrkMap::key_iterator trk_iter = ptr->get()->get_associated<TMutMCTrk>();
      if( trk_iter.count() > 0){
        _x0mc = trk_iter.current()->get()->get_x_orig();
        _y0mc = trk_iter.current()->get()->get_y_orig();
        _z0mc = trk_iter.current()->get()->get_z_orig();
        _px0mc = trk_iter.current()->get()->get_px_orig();
        _py0mc = trk_iter.current()->get()->get_py_orig();
        _pz0mc = trk_iter.current()->get()->get_pz_orig();
        _chargemc = trk_iter.current()->get()->get_charge();
        _ittrk  = trk_iter.current()->get()->get_track_id();
        _idtrk  = trk_iter.current()->get()->get_pidG4();
      }

      _x1mc = _y1mc = _z1mc = -9999.0;
      _x2mc = _y2mc = _z2mc = -9999.0;
      _x3mc = _y3mc = _z3mc = -9999.0;
      _x4mc = _y4mc = _z4mc = -9999.0;
      _x1mcb = _y1mcb = _z1mcb = -9999.0;
      _x2mcb = _y2mcb = _z2mcb = -9999.0;
      _x3mcb = _y3mcb = _z3mcb = -9999.0;
      _x4mcb = _y4mcb = _z4mcb = -9999.0;
      _x0mcproj = _y0mcproj = _z0mcproj = -9999.0;

      // Get MC hits associated with track

      if (trk_iter.count() > 0 ) {

        TFvtxMCHitMap::key_iterator mchit_iter = trk_iter->get()->get_associated<TFvtxMCHit>();
//        _nhitsf = mchit_iter.count();
        _nhitsf = 0;
        _hitstIds.assign(0);
        while( TFvtxMCHitMap::const_pointer mchit_ptr = mchit_iter.next() )
	  {
	    if ( _nhitsf >= (int)_hitstIds.size() )
	      {
		std::cout << "fill_trk_eval_tree: Number of true MC hits ("
			  << mchit_iter.count() << ") exceeds array bounds, skipping rest"
			  << std::endl;
		break; // protect against overflow
	      }
	    _hitstIds[_nhitsf++] = mchit_ptr->get()->get_index();
	    if (mchit_ptr->get()->get_station()==0){
	      _x1mc = mchit_ptr->get()->get_x();
	      _y1mc = mchit_ptr->get()->get_y();
	      _z1mc = mchit_ptr->get()->get_z();
	    }
	    else if (mchit_ptr->get()->get_station()==1){
	      _x2mc = mchit_ptr->get()->get_x();
	      _y2mc = mchit_ptr->get()->get_y();
	      _z2mc = mchit_ptr->get()->get_z();
	    }
	    else if (mchit_ptr->get()->get_station()==2){
	      _x3mc = mchit_ptr->get()->get_x();
	      _y3mc = mchit_ptr->get()->get_y();
	      _z3mc = mchit_ptr->get()->get_z();
	    }
	    else if (mchit_ptr->get()->get_station()==3){
	      _x4mc = mchit_ptr->get()->get_x();
	      _y4mc = mchit_ptr->get()->get_y();
	      _z4mc = mchit_ptr->get()->get_z();
	    }
	  }
      }
      else _nhitsf = 0;


      // Get barrel silicon MC hits associated with track:
      TFvtxPisaHitMap::const_key_iterator pisa_iter = ptr->get()->get_associated<TFvtxPisaHit>();
      _nhitsb = pisa_iter.count();
      //cout << "associated svx hits " << _nhitsb << endl;
      while( TFvtxPisaHitMap::const_pointer pisa_hit_ptr = pisa_iter.next() )
      {

        if(pisa_hit_ptr->get()->get_pisa_hit()->GetLayer() == 0){
          _x1mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetXGlobal();
          _y1mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetYGlobal();
          _z1mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetZGlobal();
        }
        if(pisa_hit_ptr->get()->get_pisa_hit()->GetLayer() == 1){
          _x2mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetXGlobal();
          _y2mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetYGlobal();
          _z2mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetZGlobal();
        }
        if(pisa_hit_ptr->get()->get_pisa_hit()->GetLayer() == 2){
          _x3mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetXGlobal();
          _y3mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetYGlobal();
          _z3mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetZGlobal();
        }
        if(pisa_hit_ptr->get()->get_pisa_hit()->GetLayer() == 3){
          _x4mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetXGlobal();
          _y4mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetYGlobal();
          _z4mcb = pisa_hit_ptr->get()->get_pisa_hit()->GetZGlobal();
        }
      }

      if (_x1mc>-990 && _x4mc>-990){
        _x0mcproj = _x1mc - (_x4mc-_x1mc)/(_z4mc-_z1mc)*_z1mc;
        _y0mcproj = _y1mc - (_y4mc-_y1mc)/(_z4mc-_z1mc)*_z1mc;
        _z0mcproj = 0.0;
      }


      if( _nhitsb == 0 ) {
        TFvtxSvxClusterMap::const_key_iterator svx_iter = ptr->get()->get_associated<TFvtxSvxCluster>();
        _nhitsb = svx_iter.count();
      }

      if( _trk_eval_tree ) _trk_eval_tree->Fill();

    }  // Loop on TFvtxTrks
  }  // If TFvtxTrk exists

  return;

}

// Utility to look up the charm & bottom history of true track "trackid".
// Requires _phpythia_node, _trackPriMap
//
void
FvtxMCEval::lookupCharmBottomAncestry(const int trackId, short int& primId,
				      int& parent_pdgid, short int& hasAncB, short int& hasAncD)
{
  if ( ! _phpythia_node ) return;

  std::map<int,int>::iterator it = _trackPriMap.find(trackId);
  if ( it != _trackPriMap.end() )
    {
      primId = (*it).second;
      try
	{
	  if ( TMCParticle* part = _phpythia_node->getPrimaryParticle(primId) )
	    {
	      if ( TMCParticle* parent = _phpythia_node->getParent(part) )
		{
		  parent_pdgid = parent->GetKF();
		}

	      // Now test for B or D in the particle's ancestry
	      if (
		  _phpythia_node->hasAncestor(part, 511) != 0 ||
		  _phpythia_node->hasAncestor(part,-511) != 0 ||
		  _phpythia_node->hasAncestor(part, 521) != 0 ||
		  _phpythia_node->hasAncestor(part,-521) != 0 ||
		  _phpythia_node->hasAncestor(part, 531) != 0 ||
		  _phpythia_node->hasAncestor(part,-531) != 0 ||
		  _phpythia_node->hasAncestor(part, 541) != 0 ||
		  _phpythia_node->hasAncestor(part,-541) != 0
		  ) hasAncB = 1;

	      if (
		  _phpythia_node->hasAncestor(part, 411) != 0 ||
		  _phpythia_node->hasAncestor(part, 421) != 0 ||
		  _phpythia_node->hasAncestor(part, 431) != 0 ||
		  _phpythia_node->hasAncestor(part,-411) != 0 ||
		  _phpythia_node->hasAncestor(part,-421) != 0 ||
		  _phpythia_node->hasAncestor(part,-431) != 0
		  ) hasAncD = 1;

	    }
	}
      catch (const std::exception& e)
	{
	  std::cout << "event " << _event << ", Exception fetching (true_track,primary) = "
		    << "(" << trackId << "," << _primId << "), what = " << e.what()
		    << std::endl;
	}
    }
}



//______________________________________________________
void FvtxMCEval::fill_mc_trk_eval_tree( void )
{
  if ( !_mut_mc_trk_map ) return;

  _evtx0mc = _evty0mc = _evtz0mc = -9999.0;

  if ( _vtxout_node )
  {
    //cout << "vertex (" << (_vtxout_node->get_Vertex()).getX() << ", " << (_vtxout_node->get_Vertex()).getY() << ", " << (_vtxout_node->get_Vertex()).getZ() << ")" << endl;
    //cout << "Simvertex " << (_vtxout_node->get_Vertex("SIM")).getX() << ", " << (_vtxout_node->get_Vertex("SIM")).getY() << ", " << (_vtxout_node->get_Vertex("SIM")).getZ() << endl;
    _evtx0mc = (_vtxout_node->get_Vertex("SIM")).getX();
    _evty0mc = (_vtxout_node->get_Vertex("SIM")).getY();
    _evtz0mc = (_vtxout_node->get_Vertex("SIM")).getZ();

    _evtx0error = (_vtxout_node->get_VertexError("SIM")).getX();
    _evty0error = (_vtxout_node->get_VertexError("SIM")).getY();
    _evtz0error = (_vtxout_node->get_VertexError("SIM")).getZ();
  }
  else
  {
    cout << "_vtxout_node not found " << endl;
  }

  bool error;
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  _evtx0smear = vtx.getX();
  _evty0smear = vtx.getY();
  _evtz0smear = vtx.getZ();

  // Get the total number of hits in the event:
  //
  _nhitsN = 0;
  _nhitsS = 0;
  if (_fvtx_hit_map){
    for (int istation=0; istation<FVTXOO::MAX_STATION; istation++){
      for (int icage=0; icage<FVTXOO::MAX_CAGE; icage++){
        TFvtxHitMap::iterator hit_iter_n = _fvtx_hit_map->get(FVTXOO::North, icage, istation);
        _nhitsN += hit_iter_n.count();
        TFvtxHitMap::iterator hit_iter_s = _fvtx_hit_map->get(FVTXOO::South, icage, istation);
        _nhitsS += hit_iter_s.count();
      }
    }
  }

  _ncoordsN = 0;
  _ncoordsS = 0;
  if (_fvtx_coord_map){
    for (int icage=0; icage<FVTXOO::MAX_STATION; icage++){
      for (int istation=0; istation<FVTXOO::MAX_STATION; istation++){
        TFvtxCoordMap::iterator coord_iter_n = _fvtx_coord_map->get(FVTXOO::North, icage, istation);
        _ncoordsN += coord_iter_n.count();
        TFvtxCoordMap::iterator coord_iter_s = _fvtx_coord_map->get(FVTXOO::South, icage, istation);
        _ncoordsS += coord_iter_s.count();
      }
    }
  }

  // Get number of MuID roads in this event:
  TMuiRoadMapO::iterator road_iter = _road_mapO->get(0);
  _nroadS = road_iter.count();
  road_iter = _road_mapO->get(1);
  _nroadN = road_iter.count();

  // loop over MC Tracks:
  TMutMCTrkMap::iterator iter( _mut_mc_trk_map->range() );
  while( TMutMCTrkMap::const_pointer mc_trk_ptr = iter.next() )
    {
      _ncoordsShared = 0;
      _hitstIds.assign(0);
      _nstationst = 0;
      _x1mc = -9999.0;
      _y1mc = -9999.0;
      _z1mc = -9999.0;
      _x2mc = -9999.0;
      _y2mc = -9999.0;
      _z2mc = -9999.0;
      _x3mc = -9999.0;
      _y3mc = -9999.0;
      _z3mc = -9999.0;
      _x4mc = -9999.0;
      _y4mc = -9999.0;
      _z4mc = -9999.0;
      _x0reco = -9999.0;
      _y0reco = -9999.0;
      _z0reco = -9999.0;
      _px0reco = -9999.0;
      _py0reco = -9999.0;
      _pz0reco = -9999.0;
      _chargereco = -9999.0;
      _chargemc = -9999.0;
      _chisqrecopdf = -9999.0;
      _nvtxf = 0;
      _nvtxmc = 0;
      _nhitsb = 0;
      _nhitsf = 0;
      _nhitst = 0;
      _nhitst_mut = 0;
      _nhitsmade_mut = 0;
      _ncoordst_mut = 0;
      _foundmut = 0;
      _ittrk = -9999;
      _idtrk = -9999;
      _parent_pid = -9999;
      _parent_pdgid = -9999;
      _ntracks = 0;
      _nhitsfound_mut = 0;
      _recosuc = 0;
      _recosuc_mut = 0;
      _mutmatch = 0;
      _muttrid = -1;
      int foundst[4] = {0};

      _x0slreco = -9999;
      _y0slreco = -9999;
      _mxslreco = -9999;
      _myslreco = -9999;
      _x0slerr = -9999;
      _y0slerr = -9999;
      _mxslerr = -9999;
      _myslerr = -9999;
      _chi2slreco = -9999;
      _ndfslreco = 0;

      _x0mutreco = -9999;
      _y0mutreco = -9999;
      _z0mutreco = -9999;
      _px0mutreco = -9999;
      _py0mutreco = -9999;
      _pz0mutreco = -9999;
      _chargemutreco = -9999;
      _chisqmutreco = -9999;
      _chisqmutrecopdf = -9999;

      _pxmutr = -9999.0;
      _pymutr = -9999.0;
      _pzmutr = -9999.0;
      _chi2mutr = -9999.0;

      _mu_charge = -9999;
      _mu_px  = -9999;
      _mu_py  = -9999;
      _mu_pz  = -9999;
      _mu_p   = -9999;
      _mu_pt  = -9999;
      _mu_chi2 = -9999;
      _ghost   = -9999;
      _mu_lastGap = 0;

      // Store the MC track params:
      _arm = mc_trk_ptr->get()->get_arm();
      _x0mc = mc_trk_ptr->get()->get_x_orig();
      _y0mc = mc_trk_ptr->get()->get_y_orig();
      _z0mc = mc_trk_ptr->get()->get_z_orig();
      _px0mc = mc_trk_ptr->get()->get_px_orig();
      _py0mc = mc_trk_ptr->get()->get_py_orig();
      _pz0mc = mc_trk_ptr->get()->get_pz_orig();
      _chargemc = mc_trk_ptr->get()->get_charge();
      _ittrk  = mc_trk_ptr->get()->get_track_id();
      _idtrk  = mc_trk_ptr->get()->get_pidG4();

      // Count the number of MuTr tracks, hits/coords associated with this MC track:
      TMutTrkMap::key_iterator mut_trk_iter = mc_trk_ptr->get()->get_associated<TMutTrk>();

      _foundmut = mut_trk_iter.count();

      _muttrid = mc_trk_ptr->get()->get_track_id();

      // Count the number of coordinates in the found track that come from the MC track of interest:
      while ( TMutTrkMap::const_pointer mut_trk_ptr = mut_trk_iter.next() ){
        // If multiple tracks found, try to only store the information for the one that was
        // successfully reconstructed:
        if (_recosuc_mut > 0) continue;
        _recosuc_mut = mut_trk_ptr->get()->get_reco_success();
        _nhitsfound_mut = 0;
        TMutCoordMap::key_iterator mut_coord_iter = mut_trk_ptr->get()->get_associated<TMutCoord>();
        while(TMutCoordMap::pointer mut_coord_ptr = mut_coord_iter.next()){
          TMutMCHitMap::key_iterator mc_hit_iter = mut_coord_ptr->get()->get_associated<TMutMCHit>();
          while(TMutMCHitMap::pointer mut_mc_hit_ptr = mc_hit_iter.next())
          {
            if (mut_mc_hit_ptr->get()->get_track_id() == _muttrid ) _nhitsfound_mut++;
          }
        }
      }

      // Count the actual number of hits, coordinates that the MC track made:
      TMutMCHitMap::key_iterator mut_mc_hit_iter = mc_trk_ptr->get()->get_associated<TMutMCHit>();
      _nhitst_mut = mut_mc_hit_iter.count();
      while ( TMutMCHitMap::const_pointer mut_mc_hit_ptr = mut_mc_hit_iter.next() ){
	  //if ( mc_trk_ptr->get()->get_track_id() != mut_mc_hit_ptr->get()->get_track_id() ) continue;
	  TMutHitMap::key_iterator mut_hit_iter = mut_mc_hit_ptr->get()->get_associated<TMutHit>();
	  if (mut_hit_iter.count() > 0) _nhitsmade_mut ++;

	  TMutCoordMap::key_iterator mut_mc_coord_iter = mut_mc_hit_ptr->get()->get_associated<TMutCoord>();
	  if (mut_mc_coord_iter.count() > 0) _ncoordst_mut++;
      }


      // Count FVTX MC hits associated with this MC track.  We may want to check that a coordinate
      // was actually formed by it once we start putting in inefficiencies, etc.  For now, just
      // count the MC hits
      TFvtxMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TFvtxMCHit>();
      while ( TFvtxMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next() )
	{
	  if ( mc_trk_ptr->get()->get_track_id() != mc_hit_ptr->get()->get_track_id() ) continue;

	  // Check to see that a coordinate was actually formed by this MC hit:
	  TFvtxCoordMap::key_iterator mc_coord_iter = mc_hit_ptr->get()->get_associated<TFvtxCoord>();
	  if (mc_coord_iter.count() > 0){
	    if ( _nhitst >= (int)_hitstIds.size() )
	      {
		std::cout << "fill_mc_trk_eval_tree: Number of Coords exceeds array bounds, skipping rest"
			  << std::endl;
		break;
	      }
	    foundst[mc_hit_ptr->get()->get_station()] = 1;
	    _hitstIds[_nhitst++] = mc_hit_ptr->get()->get_index();
	    }

	  if (mc_hit_ptr->get()->get_station()==FVTXOO::Station1){
	    _x1mc = mc_hit_ptr->get()->get_x();
	    _y1mc = mc_hit_ptr->get()->get_y();
	    _z1mc = mc_hit_ptr->get()->get_z();
	  }
	  else if (mc_hit_ptr->get()->get_station()==FVTXOO::Station2){
	    _x2mc = mc_hit_ptr->get()->get_x();
	    _y2mc = mc_hit_ptr->get()->get_y();
	    _z2mc = mc_hit_ptr->get()->get_z();
	  }
	  else if (mc_hit_ptr->get()->get_station()==FVTXOO::Station3){
	    _x3mc = mc_hit_ptr->get()->get_x();
	    _y3mc = mc_hit_ptr->get()->get_y();
	    _z3mc = mc_hit_ptr->get()->get_z();
	  }
	  else if (mc_hit_ptr->get()->get_station()==FVTXOO::Station4){
	    _x4mc = mc_hit_ptr->get()->get_x();
	    _y4mc = mc_hit_ptr->get()->get_y();
	    _z4mc = mc_hit_ptr->get()->get_z();
	  }

	  // Count the number of reco tracks using this hit
	  TFvtxTrkMap::key_iterator trk_iter = mc_hit_ptr->get()->get_associated<TFvtxTrk>();
	  if ( trk_iter.count() > 1 ) _ncoordsShared++; // despite the name, this is num of shared hits

	}
      for (int i=0; i<4; i++) _nstationst += foundst[i];

      // Look up the parent's PID.  The default value is the parent id as reported by GEANT.
      // This is translated to the PDG scheme and stored.  Then we ask the event generator what
      // the parent ID was.  If the particle is primary (meaning it came from the generator),
      // we record that parent ID instead of what GEANT told us.  This handles cases where decays
      // were handled by the event generator, which PISA will consider as primary particles.
      //

      // Changed to get_parent_id() -- Lisa 8-6-08 (check in 8-18-08)
      int parent_geantid = std::abs(mc_trk_ptr->get()->get_parent_id());
      //int parent_geantid = std::abs(mc_trk_ptr->get()->get_parent_track_id());
      _parent_pid = parent_geantid;
      _parent_pdgid = geant2pdgId(parent_geantid);
      _hasAncB = 0;
      _hasAncD = 0;
      _primId = -1;
      lookupCharmBottomAncestry(_ittrk,_primId,_parent_pdgid,_hasAncB,_hasAncD);

      // Get associated reconstructed tracks, loop over them and try to select best match
      _ntracks = 0;
      TFvtxTrkMap::key_iterator trk_iter = mc_trk_ptr->get()->get_associated<TFvtxTrk>();

      _chisqreco = -9999.0;
      _chisqrecopdf = -9999.0;

      int nhitsbest = 0;
      _nhitsfound = 0;
      _nhitsfoundt = 0;
      _nhitsfoundt1 = 0;
      _nhitsfoundt2 = 0;
      _nhitsfoundt3 = 0;
      _nhitsfoundt4 = 0;
      _hitsFoundIds.assign(0);
      boost::array<int,20> hitsFoundIds;
      boost::array<int,20> coordFoundIds;

      // Loop over the reco tracks, looking for the "best" match to store in
      // the ntuple.
      while( TFvtxTrkMap::const_pointer trk_ptr = trk_iter.next() ) {

        if (trk_ptr->get()->get_ghost() ) continue;

        TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();

	if (coord_iter.count() >= 3 && trk_ptr->get()->get_reco_success()) _ntracks++;

        // Count the number of coordinates that come from the track of interest:
        int nhitsfoundt = 0;
        int nhitsfoundt_st[4] = {0};
        while( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() ) {
          int coord_id = coord_ptr->get()->get_index();
          TFvtxMCHitMap::key_iterator mc_coord_hit_iter = coord_ptr->get()->get_associated<TFvtxMCHit>();
          while( TFvtxMCHitMap::const_pointer mc_coord_hit_ptr = mc_coord_hit_iter.next() ) {
            if (mc_coord_hit_ptr->get()->get_track_id() == _ittrk){
	      if ( nhitsfoundt >= (int)_coordFoundIds.size() )
		{
		  std::cout << "fill_mc_trk_eval_tree: Number of Found Coords exceeds array bounds, skipping rest"
			    << std::endl;
		  break;
		}
	      coordFoundIds[nhitsfoundt] = coord_id;
	      hitsFoundIds[nhitsfoundt] = mc_coord_hit_ptr->get()->get_index();
	      nhitsfoundt++;
	      nhitsfoundt_st[mc_coord_hit_ptr->get()->get_station()]++;
            }
          }
	}

	// Count the number of barrel hits this MC track generated
	//
        TFvtxPisaHitMap::const_key_iterator iter = mc_trk_ptr->get()->get_associated<TFvtxPisaHit>();
        while( TFvtxPisaHitMap::const_pointer pisa_hit_ptr = iter.next() )
        {
	  if (pisa_hit_ptr->get()->get_pisa_hit()->GetLayer() <= 2) _nhitsb++;
        }

	// Count the number of barrel layers used by the associated reco track(s)
	//  WARNING: this only has any meaning for those mc trks with ntracks=1
	//
	int nvtxf = 0;
	std::set<int> layers;
	TFvtxPisaHitMap::key_iterator vtxhit_iter = trk_ptr->get()->get_associated<TFvtxPisaHit>();
	while ( TFvtxPisaHitMap::const_pointer vtxhit_ptr = vtxhit_iter.next() )
	  {
	    nvtxf++;
	    layers.insert(vtxhit_ptr->get()->get_pisa_hit()->GetLayer());
	  }

        // Just store the "best" reconstructed track.  For now, select based on the number of found hits:

        add_muon_info( trk_ptr->get() );

        if (nhitsfoundt > nhitsbest){

	  _nvtxf = nvtxf;
	  //_nhitsb = layers.size();

          nhitsbest = nhitsfoundt;
          _hitsFoundIds = hitsFoundIds;
          _coordFoundIds = coordFoundIds;

          _nhitsfoundt = nhitsfoundt;
          _nhitsfoundt1 = nhitsfoundt_st[0];
          _nhitsfoundt2 = nhitsfoundt_st[1];
          _nhitsfoundt3 = nhitsfoundt_st[2];
          _nhitsfoundt4 = nhitsfoundt_st[3];

          _nhitsfound = coord_iter.count();
          _recosuc = trk_ptr->get()->get_reco_success();
          _chargereco = trk_ptr->get()->get_charge();
          _x0reco = trk_ptr->get()->get_trk_par_vtx()->get_x();
          _y0reco = trk_ptr->get()->get_trk_par_vtx()->get_y();
          _z0reco = trk_ptr->get()->get_trk_par_vtx()->get_z();
          _px0reco = trk_ptr->get()->get_trk_par_vtx()->get_px();
          _py0reco = trk_ptr->get()->get_trk_par_vtx()->get_py();
          _pz0reco = trk_ptr->get()->get_trk_par_vtx()->get_pz();
          _chisqreco = trk_ptr->get()->get_w_chi_square();
          _chisqrecopdf = trk_ptr->get()->get_w_chi_square_pdf();

          _pxmutr  = trk_ptr->get()->get_trk_par_mutr()->get_px();
          _pymutr  = trk_ptr->get()->get_trk_par_mutr()->get_py();
          _pzmutr  = trk_ptr->get()->get_trk_par_mutr()->get_pz();
          _chi2mutr = trk_ptr->get()->get_trk_par_mutr()->get_chi_square();

	  // See if there is a straight-line fit associated with the track.
	  //
	  TFvtxStraightTrkParMap::key_iterator par_iter = trk_ptr->get()->get_associated<TFvtxStraightTrkPar>();
	  //std::cout << "Found " << par_iter.count() << " straight fit pars for this reco track" << std::endl;
	  if ( TFvtxStraightTrkParMap::pointer par_ptr = par_iter.current() )
	    {
	      _x0slreco = par_ptr->get()->get_x();
	      _y0slreco = par_ptr->get()->get_y();
	      _mxslreco = par_ptr->get()->get_mx();
	      _myslreco = par_ptr->get()->get_my();
	      _x0slerr = sqrt(par_ptr->get()->get_covar(0,0));
	      _y0slerr = sqrt(par_ptr->get()->get_covar(1,1));
	      _mxslerr = sqrt(par_ptr->get()->get_covar(2,2));
	      _myslerr = sqrt(par_ptr->get()->get_covar(3,3));
	      _chi2slreco = par_ptr->get()->get_chi_square();
	      _ndfslreco = par_ptr->get()->get_ndf();
	    }

          // See if any muon track associated to this track.  If so, store params from track:

          TMutTrkMap::key_iterator mut_trk_iter = trk_ptr->get()->get_associated<TMutTrk>();
	  _mutmatch = mut_trk_iter.count();
	  //if (mut_trk_iter.count() > 0) _mutmatch = 1;
          //else _mutmatch = 0;

	  if ( _mutmatch )
	    {
	      if ( TMutTrkMap::const_pointer mut_trk_ptr = mut_trk_iter.current() )
		{
		  _x0mutreco = mut_trk_ptr->get()->get_trk_par_vtx()->get_x();
		  _y0mutreco = mut_trk_ptr->get()->get_trk_par_vtx()->get_y();
		  _z0mutreco = mut_trk_ptr->get()->get_trk_par_vtx()->get_z();
		  _px0mutreco = mut_trk_ptr->get()->get_trk_par_vtx()->get_px();
		  _py0mutreco = mut_trk_ptr->get()->get_trk_par_vtx()->get_py();
		  _pz0mutreco = mut_trk_ptr->get()->get_trk_par_vtx()->get_pz();
		  _chargemutreco = (short)mut_trk_ptr->get()->get_charge();
		  _chisqmutreco = mut_trk_ptr->get()->get_w_chi_square();
		  _chisqmutrecopdf = mut_trk_ptr->get()->get_w_chi_square_pdf();
		}
	    }

        }
      }  // Loop over reconstructed tracks associated with this MC track


      if ( _mc_trk_eval_tree ) _mc_trk_eval_tree->Fill();

    }  // Loop over MC tracks

  return;

}
//______________________________________________________
void FvtxMCEval::fill_mc_traj_eval_tree( void )
{
  static int ievent = 0;
  _itrack = 0;
  ievent++;

  //_event = ievent;

  // loop over TFvtxMCHits
  if( _fvtx_trk_map )
  {
    TFvtxTrkMap::iterator iter( _fvtx_trk_map->range() );
    while( TFvtxTrkMap::const_pointer ptr = iter.next() )
    {
      _xmc = _ymc = _zmc = -9999;
      const vector<TMutTrkPar>& trk_par_list( *ptr->get()->get_trk_par_list() );
      for( vector<TMutTrkPar>::const_iterator iter = trk_par_list.begin(); iter != trk_par_list.end(); iter++ ){
        _xreco = iter->get_x();
        _yreco = iter->get_y();
        _zreco = iter->get_z();
        if( _mc_traj_eval_tree ) _mc_traj_eval_tree->Fill();
      }

      _xreco = _yreco = _zreco = -9999;
      // get associated MC trk
      TMutMCTrkMap::key_iterator trk_iter = ptr->get()->get_associated<TMutMCTrk>();
      if( trk_iter.count() ){

        _xmc = trk_iter.current()->get()->get_x_orig();
        _ymc = trk_iter.current()->get()->get_y_orig();
        _zmc = trk_iter.current()->get()->get_z_orig();
        if( _mc_traj_eval_tree ) _mc_traj_eval_tree->Fill();
        // Get MC hits associated with track
        TFvtxMCHitMap::key_iterator mchit_iter = ptr->get()->get_associated<TFvtxMCHit>();
        while( TFvtxMCHitMap::const_pointer mchit_ptr = mchit_iter.next() ){
          _xmc = mchit_ptr->get()->get_x();
          _ymc = mchit_ptr->get()->get_y();
          _zmc = mchit_ptr->get()->get_z();
          if( _mc_traj_eval_tree ) _mc_traj_eval_tree->Fill();
        }

      }
      _itrack++;
    }  // Loop on TFvtxTrks
  }  // If TFvtxTrk exists

  return;
}

//______________________________________________________
void FvtxMCEval::book_mc_cluster_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _mc_cluster_tree = new TTree( "mc_cluster", "mc_cluster" );
  _mc_cluster_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "column", &_column, "column/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "size", &_size, "size/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "strip", &_strip[0], "strip[size]/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "q", &_q[0], "q[size]/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "ncoords", &_ncoords, "ncoords/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "w", &_w, "w/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "xbegin", &_xbegin, "xbegin/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "xend", &_xend, "xend/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "ybegin", &_ybegin, "ybegin/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "yend", &_yend, "yend/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "nhits", &_nhits, "nhits/I", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "xmcc", &_xmcc, "xmcc/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "ymcc", &_ymcc, "ymcc/D", BUFFER_SIZE );
  _mc_cluster_tree->Branch( "res", &_res, "res/D", BUFFER_SIZE );
  _mc_cluster_tree->SetAutoSave( AUTO_SAVE );
  cout << "_mc_cluster_tree booked" << endl;

  _eval_trees.push_back(_mc_cluster_tree);

}
//______________________________________________________
void FvtxMCEval::fill_mc_cluster_tree( void )
{
  static int ievent = 0;
  ievent++;

  // loop over TFvtxClusters
  if( _fvtx_clus_map )
  {
    TFvtxClusMap::iterator iter( _fvtx_clus_map->range() );
    while( TFvtxClusMap::const_pointer ptr = iter.next() )
    {

      // location
      _arm = ptr->get()->get_arm();
      _cage = ptr->get()->get_cage();
      _station = ptr->get()->get_station();
      _sector = ptr->get()->get_sector();
      _column = ptr->get()->get_column();
      //      _event = ievent;

      // get associated hits
      TFvtxHitMap::key_iterator hit_iter = ptr->get()->get_associated<TFvtxHit>();

      // get associated MC hits
      TFvtxMCHitMap::key_iterator mchit_iter = hit_iter->get()->get_associated<TFvtxMCHit>();
      _nhits = mchit_iter.count();
      if (mchit_iter.count()){
        _xmcc = mchit_iter->get()->get_x();
        _ymcc = mchit_iter->get()->get_y();

      }

      // retrieve number of strips in cluster
      _size = hit_iter.count();
      if( hit_iter.count() ) {
        int i = 0;
        while( TFvtxHitMap::const_pointer hit_ptr = hit_iter.next() ){
          _strip[i] = hit_ptr->get()->get_strip();
          _q[i] = hit_ptr->get()->get_q();
          i++;
        }
      }

      // get associated coordinates
      TFvtxCoordMap::key_iterator coord_iter = ptr->get()->get_associated<TFvtxCoord>();

      // retrieve number of coordinates in cluster
      _ncoords = coord_iter.count();

      // If a coordinate present, get position:
      _w = _xbegin = _ybegin = _xend = _yend = -9999;
      if ( coord_iter.count() ) {
        _w = coord_iter->get()->get_w();

        PHPoint begin = coord_iter->get()->get_coord_begin();
        _xbegin = begin.getX();
        _ybegin = begin.getY();
        PHPoint end = coord_iter->get()->get_coord_end();
        _xend = end.getX();
        _yend = end.getY();

        if (mchit_iter.count()){
          PHLine coord = coord_iter->get()->get_coord();
          float zplane = coord.getBasepoint().getZ();
          PHPoint xmc(_xmcc, _ymcc, zplane );
          _res = PHGeometry::distanceLinePoint(coord, xmc);
        }
      }

      if( _mc_cluster_tree ) _mc_cluster_tree->Fill();

    }
  }

  return;

}
//______________________________________________________
void FvtxMCEval::book_mc_fvtx_hit_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _mc_fvtx_hit_tree = new TTree( "mc_fvtx_hit", "mc_fvtx_hit" );
  _mc_fvtx_hit_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "column", &_column, "column/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "size", &_size, "size/I", BUFFER_SIZE );
  //_mc_fvtx_hit_tree->Branch( "strip", &_strip[0], "strip[size]/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "strip", &_fvtx_strip, "strip[size]/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "xbegin", &_xbegin, "xbegin/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "xend", &_xend, "xend/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "ybegin", &_ybegin, "ybegin/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "yend", &_yend, "yend/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "xmcc", &_xmcc, "xmcc/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "ymcc", &_ymcc, "ymcc/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "zmcc", &_zmcc, "zmcc/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "qhit", &_qhit, "qhit/D", BUFFER_SIZE );
  _mc_fvtx_hit_tree->Branch( "adc", &_adc, "adc/I", BUFFER_SIZE );
  _mc_fvtx_hit_tree->SetAutoSave( AUTO_SAVE );
  cout << "_mc_fvtx_hit_tree booked" << endl;

  _eval_trees.push_back(_mc_fvtx_hit_tree);

}
//______________________________________________________
void FvtxMCEval::fill_mc_fvtx_hit_tree( void )
{
  // loop over TFvtxMCHits
  if( _fvtx_hit_map )
  {
    TFvtxHitMap::iterator iter( _fvtx_hit_map->range() );
    while( TFvtxHitMap::const_pointer ptr = iter.next() )
    {
      _xbegin = 0.0;
      _ybegin = 0.0;
      _xend = 0.0;
      _yend = 0.0;

      // location
      _arm = ptr->get()->get_arm();
      _cage = ptr->get()->get_cage();
      _station = ptr->get()->get_station();
      _sector = ptr->get()->get_sector();
      _column = ptr->get()->get_column();
      _qhit = ptr->get()->get_q();
      _adc = ptr->get()->get_adc();

      // get associated MC hits
      TFvtxMCHitMap::key_iterator mchit_iter = ptr->get()->get_associated<TFvtxMCHit>();
      if (mchit_iter.count()){
        _xmcc = mchit_iter->get()->get_x();
        _ymcc = mchit_iter->get()->get_y();
        _zmcc = mchit_iter->get()->get_z();
      }

      _fvtx_strip = ptr->get()->get_strip();

      // Get position of strip:
      try {
        FvtxStrip* strip_ptr = FvtxGeom::get_arm( ptr->get()->get_arm())->
          get_cage(ptr->get()->get_cage())->
          get_station(ptr->get()->get_station())->
          get_sector(ptr->get()->get_sector())->
          get_column(ptr->get()->get_column())->
          get_strip(ptr->get()->get_strip());

        PHPoint coord_begin = strip_ptr->get_position_begin();
        PHPoint coord_end = strip_ptr->get_position_end();
        _xbegin = coord_begin.getX();
        _ybegin = coord_begin.getY();
        _xend = coord_end.getX();
        _yend = coord_end.getY();
      } catch (const std::exception& e) {
        std::ostringstream s;
        s << "FvtxMCEval::fill_mc_fvtx_hit_tree: Exception while processing strip"
          << " ("
          << _arm << "," << _cage << "," << _station << "," << _sector << "," << _column << "," << _fvtx_strip
          << "): " << e.what();
        FVTXOO::TRACE(s.str());
      }


      if( _mc_fvtx_hit_tree ) _mc_fvtx_hit_tree->Fill();

    }  // loop over TFvtxHits
  }  // If fvtx_hit_map exists

  return;
}
//______________________________________________________
void FvtxMCEval::book_event_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _event_tree = new TTree( "event", "event" );
  _event_tree->Branch( "nhits", &_nhits_ev, "nhits/I", BUFFER_SIZE );
  _event_tree->Branch( "narms", &_narms, "narms/I", BUFFER_SIZE );
  _event_tree->Branch( "nstations", &_nstations, "nstations/I", BUFFER_SIZE );
  _event_tree->Branch( "sector0", &_sector0, "sector0/I", BUFFER_SIZE );
  _event_tree->Branch( "sector1", &_sector1, "sector1/I", BUFFER_SIZE );
  _event_tree->Branch( "sector2", &_sector2, "sector2/I", BUFFER_SIZE );
  _event_tree->Branch( "sector3", &_sector3, "sector3/I", BUFFER_SIZE );
  _event_tree->Branch( "strip0", &_strip0, "strip0/I", BUFFER_SIZE );
  _event_tree->Branch( "strip1", &_strip1, "strip1/I", BUFFER_SIZE );
  _event_tree->Branch( "strip2", &_strip2, "strip2/I", BUFFER_SIZE );
  _event_tree->Branch( "strip3", &_strip3, "strip3/I", BUFFER_SIZE );
  _event_tree->Branch( "xbegin0", &_xbegin0, "xbegin0/I", BUFFER_SIZE );
  _event_tree->Branch( "xbegin1", &_xbegin1, "xbegin1/I", BUFFER_SIZE );
  _event_tree->Branch( "xbegin2", &_xbegin2, "xbegin2/I", BUFFER_SIZE );
  _event_tree->Branch( "xbegin3", &_xbegin3, "xbegin3/I", BUFFER_SIZE );
  _event_tree->Branch( "ybegin0", &_ybegin0, "ybegin0/I", BUFFER_SIZE );
  _event_tree->Branch( "ybegin1", &_ybegin1, "ybegin1/I", BUFFER_SIZE );
  _event_tree->Branch( "ybegin2", &_ybegin2, "ybegin2/I", BUFFER_SIZE );
  _event_tree->Branch( "ybegin3", &_ybegin3, "ybegin3/I", BUFFER_SIZE );
  _event_tree->Branch( "xend0", &_xend0, "xend0/I", BUFFER_SIZE );
  _event_tree->Branch( "xend1", &_xend1, "xend1/I", BUFFER_SIZE );
  _event_tree->Branch( "xend2", &_xend2, "xend2/I", BUFFER_SIZE );
  _event_tree->Branch( "xend3", &_xend3, "xend3/I", BUFFER_SIZE );
  _event_tree->Branch( "yend0", &_yend0, "yend0/I", BUFFER_SIZE );
  _event_tree->Branch( "yend1", &_yend1, "yend1/I", BUFFER_SIZE );
  _event_tree->Branch( "yend2", &_yend2, "yend2/I", BUFFER_SIZE );
  _event_tree->Branch( "yend3", &_yend3, "yend3/I", BUFFER_SIZE );
  _event_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _event_tree->SetAutoSave( AUTO_SAVE );
  cout << "_event_tree booked" << endl;

  _eval_trees.push_back(_event_tree);

}
//______________________________________________________
void FvtxMCEval::fill_event_tree( void )
{

  int nhits_arm[2] = {0};
  int nhits[2][4];  // number of hits per arm/station
  int sector[2][4];  // number of hits per arm/station
  int strip[2][4];  // number of hits per arm/station
  int xbegin[2][4];  // number of hits per arm/station
  int ybegin[2][4];  // number of hits per arm/station
  int xend[2][4];  // number of hits per arm/station
  int yend[2][4];  // number of hits per arm/station


  for (int iarm=0; iarm < 2; iarm++)
    for (int ista=0; ista < 4; ista++){
      nhits[iarm][ista] = 0;
      strip[iarm][ista] = -999;
      sector[iarm][ista] = -999;
    }
  
   
  // loop over TFvtxMCHits
  if( _fvtx_hit_map )
  {
    TFvtxHitMap::iterator iter( _fvtx_hit_map->range() );
    _nhits_ev =  _fvtx_hit_map->size();
    while( TFvtxHitMap::const_pointer ptr = iter.next() )
    {
      _xbegin = 0.0;
      _ybegin = 0.0;
      _xend = 0.0;
      _yend = 0.0;

      // location
      _arm = ptr->get()->get_arm();
      _cage = ptr->get()->get_cage();
      _station = ptr->get()->get_station();
      _sector = ptr->get()->get_sector();
      _column = ptr->get()->get_column();
      _qhit = ptr->get()->get_q();
      _adc = ptr->get()->get_adc();
      _fvtx_strip = ptr->get()->get_strip();

      nhits[_arm][_station]++;
      nhits_arm[_arm]++;
      sector[_arm][_station] = _sector;
      strip[_arm][_station] = _fvtx_strip;


      // Get position of strip:
      try {
        FvtxStrip* strip_ptr = FvtxGeom::get_arm( ptr->get()->get_arm())->
          get_cage(ptr->get()->get_cage())->
          get_station(ptr->get()->get_station())->
          get_sector(ptr->get()->get_sector())->
          get_column(ptr->get()->get_column())->
          get_strip(ptr->get()->get_strip());

        PHPoint coord_begin = strip_ptr->get_position_begin();
        PHPoint coord_end = strip_ptr->get_position_end();
        _xbegin = coord_begin.getX();
        _ybegin = coord_begin.getY();
        _xend = coord_end.getX();
        _yend = coord_end.getY();
      } catch (const std::exception& e) {
        std::ostringstream s;
        s << "FvtxMCEval::fill_mc_fvtx_hit_tree: Exception while processing strip"
          << " ("
          << _arm << "," << _cage << "," << _station << "," << _sector << "," << _column << "," << _fvtx_strip
          << "): " << e.what();
        FVTXOO::TRACE(s.str());
      }

      xbegin[_arm][_station] = _xbegin;
      ybegin[_arm][_station] = _ybegin;
      xend[_arm][_station] = _xend;
      yend[_arm][_station] = _yend;

    }  // loop over TFvtxHits
  }  // If fvtx_hit_map exists

  _narms = 0;
  _nstations = 0;
  _sector0 = -999;
  _sector1 = -999;
  _sector2 = -999;
  _sector3 = -999;
  _strip0 = -999;
  _strip1 = -999;
  _strip2 = -999;
  _strip3 = -999;
  for (int iarm=0; iarm<2; iarm++){
    if (nhits_arm[iarm] > 0) _narms++;
    if (nhits_arm[iarm] > 0) _nstations = 0;
    for (int ista=0; ista<4; ista++){
      if (nhits[iarm][ista] > 0){
        _nstations++;
        if (ista == 0){
          _sector0 = sector[iarm][ista];
          _strip0 = strip[iarm][ista];
          _xbegin0 = xbegin[iarm][ista];
          _ybegin0 = ybegin[iarm][ista];
          _xend0 = xend[iarm][ista];
          _yend0 = yend[iarm][ista];
        }
        else if (ista == 1){
          _sector1 = sector[iarm][ista];
          _strip1 = strip[iarm][ista];
          _xbegin1 = xbegin[iarm][ista];
          _ybegin1 = ybegin[iarm][ista];
          _xend1 = xend[iarm][ista];
          _yend1 = yend[iarm][ista];
        }
        else if (ista == 2){
          _sector2 = sector[iarm][ista];
          _strip2 = strip[iarm][ista];
          _xbegin2 = xbegin[iarm][ista];
          _ybegin2 = ybegin[iarm][ista];
          _xend2 = xend[iarm][ista];
          _yend2 = yend[iarm][ista];
        }
        else if (ista == 3){
          _sector3 = sector[iarm][ista];
          _strip3 = strip[iarm][ista];
          _xbegin3 = xbegin[iarm][ista];
          _ybegin3 = ybegin[iarm][ista];
          _xend3 = xend[iarm][ista];
          _yend3 = yend[iarm][ista];
        }
      }
    }
  }

  if( _event_tree ) _event_tree->Fill();

  return;
}

void
FvtxMCEval::book_fkin_eval_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _fkin_eval_tree = new TTree( "fkin_eval", "fkin_eval" );
  _fkin_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "true_track", &_fkin.true_track, "true_track/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "subevent", &_fkin.subevent, "subevent/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "ntrack", &_fkin.ntrack, "ntrack/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "ptot", &_fkin.ptot, "ptot/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "pthet", &_fkin.pthet, "pthet/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "pphi", &_fkin.pphi, "pphi/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "r_vertex", &_fkin.r_vertex, "r_vertex/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "z_vertex", &_fkin.z_vertex, "z_vertex/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "th_vertx", &_fkin.th_vertx, "th_vertx/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "ph_vertx", &_fkin.ph_vertx, "ph_vertx/F", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "itparent", &_fkin.itparent, "itparent/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "idparent", &_fkin.idparent, "idparent/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "idpart", &_fkin.idpart, "idpart/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "nfile", &_fkin.nfile, "nfile/I", BUFFER_SIZE );
  _fkin_eval_tree->Branch( "nstationsHit", &_nstationsHit, "nfile/S", BUFFER_SIZE );

  std::cout << "_fkin_eval_tree booked" << endl;

  _eval_trees.push_back(_fkin_eval_tree);

  return;
}

void
FvtxMCEval::fill_fkin_eval_tree()
{
  if ( ! _fkin_ptr ) return;

  // count the stations hit by the tracks
  //
  std::map<int,int> stationsHit;
  if ( _mut_mc_trk_map )
    {
      TMutMCTrkMap::iterator iter( _mut_mc_trk_map->range() );
      while( TMutMCTrkMap::const_pointer mc_trk_ptr = iter.next() )
	{
	  int staHit[4] = { 0 };
	  int id =  mc_trk_ptr->get()->get_track_id();

	  TFvtxMCHitMap::key_iterator mc_hit_iter = mc_trk_ptr->get()->get_associated<TFvtxMCHit>();
	  while( TFvtxMCHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next() )
	    {
	      if ( mc_hit_ptr->get()->get_track_id() == id )
		{
		  staHit[mc_hit_ptr->get()->get_station()] = 1;
		}
	    }

	  int n = std::count_if(staHit,staHit+4,std::bind2nd(std::greater<int>(),0));
	  stationsHit.insert(std::make_pair(id,n));
	}
    }
  else
    {
      stationsHit[0] = -1;
      stationsHit[1] = -1;
      stationsHit[2] = -1;
      stationsHit[3] = -1;
    }

  for (unsigned int i=0; i<_fkin_ptr->RowCount(); i++)
    {
      _fkin = (*_fkin_ptr)[i];
      _nstationsHit = (short) stationsHit[_fkin.true_track];

      if ( _fkin_eval_tree ) _fkin_eval_tree->Fill();
    }

  return;
}

void
FvtxMCEval::book_header_eval_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _header_eval_tree = new TTree( "header_eval", "header_eval" );
  _header_eval_tree->Branch( "run", &_header.run, "run/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "event", &_header.event, "event/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "mult", &_header.multiplicity, "mult/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "b", &_header.b, "b/F", BUFFER_SIZE );
  _header_eval_tree->Branch( "a1", &_header.a1, "a1/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "z1", &_header.z1, "z1/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "a2", &_header.a2, "a2/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "z2", &_header.z2, "z2/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "sqrt_s", &_header.sqrt_s, "sqrt_s/F", BUFFER_SIZE );
  _header_eval_tree->Branch( "bmin", &_header.bmin, "bmin/F", BUFFER_SIZE );
  _header_eval_tree->Branch( "bmax", &_header.bmax, "bmax/F", BUFFER_SIZE );
  _header_eval_tree->Branch( "t0femto", &_header.t0femto, "t0femto/F", BUFFER_SIZE );
  _header_eval_tree->Branch( "vertex[3]", &_header.vertex[0], "vertex[0]/F", BUFFER_SIZE );
  _header_eval_tree->Branch( "itime", &_header.itime, "itime/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "idate", &_header.idate, "idate/I", BUFFER_SIZE );
  _header_eval_tree->Branch( "nrndm[2]", &_header.nrndm[0], "nrndm[0]/I", BUFFER_SIZE );
  _header_eval_tree->Branch( "isqStart", &_header.isqStart, "isqStart/S", BUFFER_SIZE );
  _header_eval_tree->Branch( "iSeconds", &_header.iSeconds, "iSeconds/I", BUFFER_SIZE );
  _header_eval_tree->Branch( "maxTrueTrack", &_header.maxTrueTrack, "maxTrueTrack/I", BUFFER_SIZE );

  std::cout << "_header_eval_tree booked" << endl;

  _eval_trees.push_back(_header_eval_tree);

  return;
}

template<typename WRAPPER, typename ROW> void
fill_PHTable_eval_tree(const WRAPPER* table_ptr, ROW& row, TTree* t)
{
  if ( ! table_ptr ) return;
  if ( ! t ) return;

  for (unsigned int i=0; i<table_ptr->RowCount(); i++)
    {
      row = (*table_ptr)[i];
      t->Fill();
    }
}

void
FvtxMCEval::fill_header_eval_tree()
{
  fill_PHTable_eval_tree(_header_ptr,_header,_header_eval_tree);
}

void
FvtxMCEval::book_pythia_eval_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _pythia_eval_tree = new TTree( "pythia_eval", "pythia_eval" );
  _pythia_eval_tree->Branch( "pyth_proc_id", &_pythia.pyth_proc_id, "pyth_proc_id/S", BUFFER_SIZE );
  _pythia_eval_tree->Branch( "pyth_bjork[2]", &_pythia.pyth_bjork[0], "pyth_bjork[2]/F", BUFFER_SIZE );
  _pythia_eval_tree->Branch( "pyth_parstu[3]", &_pythia.pyth_parstu[0], "pyth_parstu[3]/F", BUFFER_SIZE );
  _pythia_eval_tree->Branch( "pyth_qsqr", &_pythia.pyth_qsqr, "pyth_qsqr/F", BUFFER_SIZE );
  _pythia_eval_tree->Branch( "pyth_ptrans", &_pythia.pyth_ptrans, "pyth_ptrans/F", BUFFER_SIZE );
  _pythia_eval_tree->Branch( "intr_part_id[4]", &_pythia.intr_part_id[0], "intr_part_id[4]/S", BUFFER_SIZE );
  _pythia_eval_tree->Branch( "intr_part_p[4][4]", &_pythia.intr_part_p[0][0], "intr_part_p[4][4]/F", BUFFER_SIZE );

  std::cout << "_pythia_eval_tree booked" << endl;

  _eval_trees.push_back(_pythia_eval_tree);

  return;
}

void
FvtxMCEval::fill_pythia_eval_tree()
{
  fill_PHTable_eval_tree(_pythia_ptr,_pythia,_pythia_eval_tree);
}

void
FvtxMCEval::book_primary_eval_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _primary_eval_tree = new TTree( "primary_eval", "primary_eval" );
  _primary_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _primary_eval_tree->Branch( "key", &_primary.key, "key/I", BUFFER_SIZE );
  _primary_eval_tree->Branch( "event_track", &_primary.event_track, "event_track/I", BUFFER_SIZE );
  _primary_eval_tree->Branch( "subevent_track", &_primary.subevent_track, "subevent_track/I", BUFFER_SIZE );
  _primary_eval_tree->Branch( "true_track", &_primary.true_track, "true_track/I", BUFFER_SIZE );
  _primary_eval_tree->Branch( "subevent", &_primary.subevent, "subevent/S", BUFFER_SIZE );
  _primary_eval_tree->Branch( "idpart", &_primary.idpart, "idpart/S", BUFFER_SIZE );
  _primary_eval_tree->Branch( "nfile", &_primary.nfile, "nfile/S", BUFFER_SIZE );
  _primary_eval_tree->Branch( "px_momentum", &_primary.px_momentum, "px_momentum/F", BUFFER_SIZE );
  _primary_eval_tree->Branch( "py_momentum", &_primary.py_momentum, "py_momentum/F", BUFFER_SIZE );
  _primary_eval_tree->Branch( "pz_momentum", &_primary.pz_momentum, "pz_momentum/F", BUFFER_SIZE );

  std::cout << "_primary_eval_tree booked" << endl;

  _eval_trees.push_back(_primary_eval_tree);

  return;
}

void
FvtxMCEval::fill_primary_eval_tree()
{
  if (!_primary_ptr) return;
  fill_PHTable_eval_tree(_primary_ptr,_primary,_primary_eval_tree);

  _trackPriMap.clear();
  for (unsigned int i=0; i<_primary_ptr->RowCount(); i++)
    {
      PRIMARY_ST row = (*_primary_ptr)[i];
      int k = row.true_track; // Key to the table is the geant track id
      int v = row.key; // Value stored at this key is the index into primary list
      _trackPriMap[k] = v;
    }

}


void
FvtxMCEval::book_coord_eval_tree( void )
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _coord_eval_tree = new TTree( "coord_eval", "coord_eval" );
  _coord_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "column", &_column, "column/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "index", &_index, "index/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "nstrip", &_size, "nstrip/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "strip", &_strip[0], "strip[nstrip]/I", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "q", &_q[0], "q[size]/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "nclus", &_nclus, "ncoords/I", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "w", &_w, "w/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "xbegin", &_xbegin, "xbegin/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "xend", &_xend, "xend/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "ybegin", &_ybegin, "ybegin/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "yend", &_yend, "yend/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "phibegin", &_phi_begin, "phibegin/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "phiend", &_phi_end, "phiend/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "r", &_r, "r/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "z", &_z, "z/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "nhits", &_nhits, "nhits/I", BUFFER_SIZE );
  _coord_eval_tree->Branch( "mc_track_id", &_track_id, "mc_track_id/I", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "xmcc", &_xmcc, "xmcc/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "ymcc", &_ymcc, "ymcc/D", BUFFER_SIZE );
  _coord_eval_tree->SetAutoSave( AUTO_SAVE );

  std::cout << "_coord_eval_tree booked" << endl;

  _eval_trees.push_back(_coord_eval_tree);

}

void
FvtxMCEval::fill_coord_eval_tree()
{
  if ( !_coord_eval_tree ) return;

  if ( !_fvtx_coord_map ) return;

  TFvtxCoordMap::iterator coord_iter = _fvtx_coord_map->range();
  while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      _arm = coord_ptr->get()->get_arm();
      _cage = coord_ptr->get()->get_cage();
      _station = coord_ptr->get()->get_station();
      _sector = coord_ptr->get()->get_sector();
      _column = coord_ptr->get()->get_column();
      _index = coord_ptr->get()->get_index();

      _track_id = -1;

      PHCylPoint end = coord_ptr->get()->get_coord_end();
      PHCylPoint begin = coord_ptr->get()->get_coord_begin();
      PHCylPoint mid = coord_ptr->get()->get_coord_midpoint();

      _phi_begin = FVTXOO::angle_normalize(begin.getPhi().getPhi());
      _phi_end = FVTXOO::angle_normalize(end.getPhi().getPhi());
      _r = mid.getR();
      _z = mid.getZ();

      _size = 0;
      _strip.assign(0);
      _nhits = 0;

      // Get associated cluster
      TFvtxClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TFvtxClus>();
      TFvtxClusMap::const_pointer clus_ptr = clus_iter.current();

      // get associated hits
      TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
      _size = hit_iter.count();

      if ( hit_iter.count() )
	{
	  std::set<int> mchitIds; // count the number of (unique) mc hits are associated with this coord
	  int i = 0;
	  while( TFvtxHitMap::const_pointer hit_ptr = hit_iter.next() )
	    {
	      TFvtxMCHitMap::key_iterator mchit_iter = hit_ptr->get()->get_associated<TFvtxMCHit>();
	      while ( TFvtxMCHitMap::const_pointer p = mchit_iter.next() ) {
		mchitIds.insert(p->get()->get_index());
		_track_id = p->get()->get_track_id(); // TODO: somehow handle possiblilyt for >1 track
	      }
	      _strip[i++] = hit_ptr->get()->get_strip();
	  }
	  _nhits += mchitIds.size();
      }

      _coord_eval_tree->Fill();
    }

  return;
}

void
FvtxMCEval::book_match_eval_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _match_eval_tree = new TTree( "match_eval", "match_eval" );
  FvtxMCMatch* p = &_mc_match; // ROOT pointer insanity
  _match_eval_tree->Branch( "match", _mc_match.GetName(), &p, BUFFER_SIZE );
  _match_eval_tree->SetAutoSave( AUTO_SAVE );

  std::cout << "_match_eval_tree booked" << endl;

  _eval_trees.push_back(_match_eval_tree);

  return;
}

template<typename T> struct Abs : public std::unary_function<T,T>
{
  T operator()(const T& v) const { return std::abs(v); }
};

void
FvtxMCEval::fill_match_eval_tree()
{
  if ( !_match_eval_tree ) return;

  if ( !_mut_mc_trk_map ) return;

  TMutMCTrkMap::iterator iter( _mut_mc_trk_map->range() );

  while( TMutMCTrkMap::const_pointer ptr = iter.next() )
    {
      _mc_match.clear();

      // Store the MC hit Ids.  At the same time count the coords associated
      // with the mc hits.
      //
      TFvtxMCHitMap::key_iterator mchit_iter = ptr->get()->get_associated<TFvtxMCHit>();
      //_mc_match.nMCHits = 0;
      //_mc_match.mcHitIds.clear();
      //_mc_match.nCoords = 0;
      //_mc_match.coordIds.clear();
      std::vector<TFvtxMCHit*> hits;
      std::vector<TFvtxCoord*> coords;
      while ( TFvtxMCHitMap::const_pointer mchit_ptr = mchit_iter.next() )
	{
	  // Only count the hit if its track id matches the current MC track
	  // we are looking at.
	  if ( mchit_ptr->get()->get_track_id() == ptr->get()->get_track_id() )
	    {
	      _mc_match.mcHitIds.push_back(mchit_ptr->get()->get_index());
	      hits.push_back(mchit_ptr->get());

	      TFvtxCoordMap::key_iterator coord_iter = mchit_ptr->get()->get_associated<TFvtxCoord>();
	      while ( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
		{
		  coords.push_back(coord_ptr->get());
		}
	    }
	}
      _mc_match.nMCHits = _mc_match.mcHitIds.size();
      _mc_match.nCoords = coords.size();
      _mc_match.coordIds.resize(coords.size());
      std::transform(coords.begin(),coords.end(),_mc_match.coordIds.begin(),
		     boost::bind(&TFvtxCoord::get_index,_1));

      if ( _mc_match.nMCHits < 3 ) continue;

      // Count the number of stations hit
      //
      //_mc_match.nstaHit = 0;
      for (int i=0; i<4; i++)
	{
	  int nSta = std::count_if(hits.begin(),hits.end(),
				   boost::bind(&TFvtxMCHit::get_station,_1) == i);
	  if ( nSta > 0 ) _mc_match.nstaHit++;
	}

      if ( _mc_match.nstaHit < 3 ) continue;

      _mc_match.event = _event;
      _mc_match.mc_track_id = ptr->get()->get_track_id();
      _mc_match.arm = ptr->get()->get_arm();
      _mc_match.ptot_orig = ptr->get()->get_ptot_orig();
      _mc_match.idtrack = ptr->get()->get_pidG4();
      _mc_match.idparent = ptr->get()->get_parent_id();
      _mc_match.itparent = ptr->get()->get_parent_track_id();

      _mc_match.vtx_orig[0] = ptr->get()->get_x_orig();
      _mc_match.vtx_orig[1] = ptr->get()->get_y_orig();
      _mc_match.vtx_orig[2] = ptr->get()->get_z_orig();

      if ( coords.size() < 2 ) continue;

      // TODO: For the phi start, end and slope & offset, all these things should be
      // precalculated and looked up.  For now, calculate them on the fly.  Start by sorting
      // the coords by abs value of their Z positions (in ascending |z|).
      //
      std::sort(
		coords.begin(), coords.end(),
		boost::bind(std::ptr_fun<double,double>(&std::fabs),boost::bind(&TFvtxCoord::get_mean_z,_1)) <
		boost::bind(std::ptr_fun<double,double>(&std::fabs),boost::bind(&TFvtxCoord::get_mean_z,_2))
		);

//       if ( ptr->get()->get_arm() == FVTXOO::North )
// 	std::sort(coords.begin(),coords.end(),
// 		  boost::bind(&TFvtxCoord::get_mean_z,_1) < boost::bind(&TFvtxCoord::get_mean_z,_2));
//       else
// 	std::sort(coords.begin(),coords.end(),
// 		  boost::bind(std::negate<double>(),boost::bind(&TFvtxCoord::get_mean_z,_1)) <
// 		  boost::bind(std::negate<double>(),boost::bind(&TFvtxCoord::get_mean_z,_2)));

      PHCylPoint pnt0(coords.front()->get_coord_begin());
      PHCylPoint pnt1(coords.front()->get_coord_end());

      _mc_match.phi_start = pnt0.getPhi();
      _mc_match.phi_end = pnt1.getPhi();

      // Calculate the slope of the mc track from the begin, end mid-points
      //
      PHCylPoint mid0(coords.front()->get_coord_midpoint());
      PHCylPoint mid1(coords.back()->get_coord_midpoint());

      double r0 = mid0.getR();
      double z0 = mid0.getZ();
      double r1 = mid1.getR();
      double z1 = mid1.getZ();
      _mc_match.mc_r_slope = (r1-r0)/(z1-z0);
      _mc_match.mc_r_offset = r0 - _mc_match.mc_r_slope * z0;

      // Store the reco track information
      //
      _mc_match.reco_r_slopeFit.clear();
      _mc_match.reco_r_offsetFit.clear();

      TFvtxTrkMap::key_iterator trk_iter = ptr->get()->get_associated<TFvtxTrk>();
      _mc_match.nreco = trk_iter.count();
      while ( TFvtxTrkMap::const_pointer trk_ptr = trk_iter.next() )
	{
	  _mc_match.reco_track_ids.push_back(trk_ptr->get()->get_index());

	  // TODO: create an object that can be associated with trk/recotrk
	  // and contains all the fit info.

	  // Assemble a list of the coords for this reco track
	  //
	  std::vector<TFvtxCoord*> coords;
	  std::vector<double> rv;
	  std::vector<double> zv;
	  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
	  while ( TFvtxCoordMap::pointer coord_ptr = coord_iter.next() )
	    {
	      coords.push_back(coord_ptr->get());
	      PHCylPoint mid(coord_ptr->get()->get_coord_midpoint());
	      rv.push_back(mid.getR());
	      zv.push_back(mid.getZ());
	    }

	  // Sort the list
	  //
	  std::sort(
		    coords.begin(),coords.end(),
	  	    boost::bind(Abs<double>(),boost::bind(&TFvtxCoord::get_mean_z,_1)) <
	  	    boost::bind(Abs<double>(),boost::bind(&TFvtxCoord::get_mean_z,_2))
	  	    );
	  PHCylPoint pntBegin(coords.front()->get_coord_begin());
	  PHCylPoint pntEnd(coords.front()->get_coord_end());

	  _mc_match.reco_phi_start.push_back(pntBegin.getPhi());
	  _mc_match.reco_phi_end.push_back(pntEnd.getPhi());

	  double reco_offset = 0.0;
	  double reco_slope = 0.0;
	  boost::tie(reco_slope,reco_offset) = linearFit(zv,rv);

	  _mc_match.reco_r_slopeFit.push_back(reco_slope);
	  _mc_match.reco_r_offsetFit.push_back(reco_offset);

	  if ( trk_ptr->get()->get_ghost() )
	    {
	      _mc_match.nghost++;
	      _mc_match.reco_ghost.push_back(1);
	    }
	  else
	    {
	      _mc_match.reco_ghost.push_back(0);
	    }
	}

      FvtxMCMatch* p = &_mc_match;
      _match_eval_tree->SetBranchAddress("match",&p);
      _match_eval_tree->Fill();
    }

  return;
}

void
FvtxMCEval::book_straight_trk_eval_tree( void )
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _straight_trk_eval_tree = new TTree("straight_trk_eval","straight_trk_eval");
  _straight_trk_eval_tree->Branch("event",&_event,"event/I",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("track",&_track,"track/I",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("ntrack_hits",&_ntrack_hits,"ntrack_hits/I",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("rchi2",&_rchi2,"rchi2/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("ndf",&_ndf,"ndf/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("x_int",&_x_int,"x_int/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("y_int",&_y_int,"y_int/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("x_slope",&_x_slope,"x_slope/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("y_slope",&_y_slope,"y_slope/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("phi_rot",&_phi_rot,"phi_rot/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("foundVtxHit",&_foundVtxHit,"foundVtxHit/O",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("halfWedgeId",&_halfWedgeId[0],"halfWedgeId[ntrack_hits]/I",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("nstrips",&_nstrips[0],"nstrips[ntrack_hits]/I",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("x_hit",&_x_hit[0],"x_hit[ntrack_hits]/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("y_hit",&_y_hit[0],"y_hit[ntrack_hits]/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("z_hit",&_z_hit[0],"z_hit[ntrack_hits]/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("r_hit",&_r_hit[0],"r_hit[ntrack_hits]/F",BUFFER_SIZE);
  _straight_trk_eval_tree->Branch("phi_hit",&_phi_hit[0],"phi_hit[ntrack_hits]/F",BUFFER_SIZE);
//   _straight_trk_eval_tree->Branch("x_cluster_width",_x_cluster_width,"x_cluster_width[size]/F",BUFFER_SIZE);
//   _straight_trk_eval_tree->Branch("y_cluster_width",_y_cluster_width,"y_cluster_width[size]/F",BUFFER_SIZE);
//   _straight_trk_eval_tree->Branch("x_cluster_begin",_x_cluster_begin,"x_cluster_begin[size]/F",BUFFER_SIZE);
//   _straight_trk_eval_tree->Branch("y_cluster_begin",_y_cluster_begin,"y_cluster_begin[size]/F",BUFFER_SIZE);
//   _straight_trk_eval_tree->Branch("x_cluster_end",_x_cluster_end,"x_cluster_end[size]/F",BUFFER_SIZE);
//   _straight_trk_eval_tree->Branch("y_cluster_end",_y_cluster_end,"y_cluster_end[size]/F",BUFFER_SIZE);
  _straight_trk_eval_tree->SetAutoSave(AUTO_SAVE);
  std::cout << "_straigh_trk_eval_tree booked" << std::endl;

  _eval_trees.push_back(_straight_trk_eval_tree);
  return;
}

void
FvtxMCEval::fill_straight_trk_eval_tree( void )
{
  if ( !_straight_trk_eval_tree ) return;

  if ( !_fvtx_trk_map ) return;

  _track = 0;
  TFvtxTrkMap::iterator iter( _fvtx_trk_map->range() );
  while( TFvtxTrkMap::const_pointer trk_ptr = iter.next() )
    {

      // initialize values
      _track = -9999;
      _ntrack_hits = -9999;
      _rchi2 = -9999;
      _x_int = -9999.0;
      _y_int = -9999.0;
      _x_slope = -9999.0;
      _y_slope = -9999.0;
      _phi_rot = -9999.0;
      _foundVtxHit = false;
      _halfWedgeId.assign(0);
      _nstrips.assign(0);
      _x_hit.assign(0);
      _y_hit.assign(0);
      _z_hit.assign(0);
      _r_hit.assign(0);
      _phi_hit.assign(0);

      //      _track = trk_ptr->get()->get_track_id();
      //      _size = trk_ptr->get()->get_n_cooord();

      TFvtxStraightTrkParMap::key_iterator trk_par_iter = trk_ptr->get()->get_associated<TFvtxStraightTrkPar>();
      if ( trk_par_iter.count()==0 ) continue;

//       _track = trk_par_iter.current()->get()->get_index();
      _rchi2 = trk_par_iter.current()->get()->get_chi_square();
      _ndf = trk_par_iter.current()->get()->get_ndf();
      _x_int = trk_par_iter.current()->get()->get_x();
      _y_int = trk_par_iter.current()->get()->get_y();
      _x_slope = trk_par_iter.current()->get()->get_mx();
      _y_slope = trk_par_iter.current()->get()->get_my();


      int ihit = 0;
      TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
      while(TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next())
	{
          if ( ihit >= (int)_x_hit.size() )
            {
              std::cout << "fill_straight_trk_eval_tree: Number of true hits ("
                        << coord_iter.count() << ") exceeds array bounds, skipping rest"
                        << std::endl;
              break; // protect against overflow
            }
	  int arm = coord_ptr->get()->get_arm();
          int cage = coord_ptr->get()->get_cage();
          int station = coord_ptr->get()->get_station();
	  int sector = coord_ptr->get()->get_sector();
	  int column = coord_ptr->get()->get_column();

	  _halfWedgeId[ihit] = FVTXGEOM::get_halfwedge_id(arm,cage,station,sector,column);

	  PHPoint cartPnt = coord_ptr->get()->get_coord_midpoint();
	  PHCylPoint cylPnt = cartPnt;

	  _x_hit[ihit] = cartPnt.getX();
	  _y_hit[ihit] = cartPnt.getY();
	  _z_hit[ihit] = cartPnt.getZ();
	  _phi_hit[ihit] = cylPnt.getPhi();

	  ihit++;
	}// coord_trk_map


      // get VTX hits
      TFvtxPisaHitMap::key_iterator mc_hit_iter = trk_ptr->get()->get_associated<TFvtxPisaHit>();
      while(TFvtxPisaHitMap::const_pointer mc_hit_ptr = mc_hit_iter.next())
	{
	  _foundVtxHit = true;
	  const SvxSnglPisaHit* hit = mc_hit_ptr->get()->get_pisa_hit();

          if ( ihit >= (int)_x_hit.size() )
            {
              std::cout << "fill_straight_trk_eval_tree: Number of true hits ("
                        << mc_hit_iter.count() << ") exceeds array bounds, skipping rest"
                        << std::endl;
              break; // protect against overflow
            }

	  _x_hit[ihit] = hit->GetXGlobal();
	  _y_hit[ihit] = hit->GetYGlobal();
	  _z_hit[ihit] = hit->GetZGlobal();
	  _phi_hit[ihit] = atan2(_y_hit[ihit],_x_hit[ihit]);

	  ihit++;
	}// pisa_hit_trk_map

      _ntrack_hits = ihit;

      _straight_trk_eval_tree->Fill();
    }// fvtx_trk_map

  return;
}

int pdgIds[] = {
      0,  // id of 0 doesn't exist in geant
     22,  // 1 gamma
    -11,  // 2 e+
     11,  // 3 e-
     12,  // 4 nu (nu_e)
    -13,  // 5 mu+
     13,  // 6 mu-
    111,  // 7 pi0
    211,  // 8 pi+
   -211,  // 9 pi-
    130,  // 10 K0L
    321,  // 11 K+
   -321,  // 12 K-
   2112,  // 13 n
   2212,  // 14 p
  -2212,  // 15 p~
    310,  // 16 K0s
    221,  // 17 eta
   3122,  // 18 Lambda
   3222,  // 19 Sigma+
   3212,  // 20 Sigma0
   3112,  // 21 Sigma-
   3322,  // 22 Xi0
   3312,  // 23 Xi-
   3334,  // 24 Omega-
  -2112,  // 25 n~
  -3122,  // 16 Lambda~
  -3112,  // 27 Sigma+~
  -3212,  // 28 Sigma0~
  -3222,  // 29 sigma-~
  -3322,  // 30 Xi0~
  -3312,  // 31 Xi-~
  -3334,  // 32 Omega-~
    -15,  // 33 tau+
     15,  // 34 tau-
    411,  // 35 D+
   -411,  // 36 D-
    421,  // 37 D0
   -421,  // 38 D0~
    431,  // 39 Ds+
   -431,  // 40 Ds-
   4122,  // 41 Lambda_c+
     24,  // 42 W+
    -24,  // 43 W-
     23,  // 44 Z
     45,   // 45 deuteron
     1003001002,    // 46 tritium
     1004002001,    // 47 alpha
     0,   // 48 geantino
  1003002001,    // 49 He3
     0,   // 50 geantino
     0,   // 51 geantino
     0,   // 52 geantino
     0,   // 53 geantino
     0,   // 54 geantino
     55   // 55 antideuteron
};

// translate a pdg id to a geant id
// uses a plain array to do the lookup
int
geant2pdgId(const int id)
{
  int n = sizeof(pdgIds)/sizeof(int);
  if ( abs(id) >= n ) return 0;         // abs() geant id b/c negative ids indicate secondary particles
  return pdgIds[abs(id)];               // Lisa 8-6-08 (check in 8-18-08)
}

void
FvtxMCEval::add_muon_info( TFvtxTrk *fvtx_trk_ptr )
{
  TMutTrkMap::key_iterator mut_trk_iter = fvtx_trk_ptr->get_associated<TMutTrk>();
  if ( mut_trk_iter.count() == 0 )
  {
    //cout << "FvtxMCEval::add_muon_info() : No mut trk associated to this fvtx trk, return " << endl;
    return;
  }
  else if ( mut_trk_iter.count() > 1 )
  {
    //cout << "FvtxMCEval::add_muon_info() : more than one mut trk associated, check mMutKalFitWithSiliReal module " << endl;
  }
  //cout << "FvtxMCEval::add_muon_info() : found one mut trk" << endl;

  // Should be only one associated mut trk
  TMutTrkMap::const_pointer trk_ptr = mut_trk_iter.next();

/*
  // Copied from MWGOOReco.C
  int itrk = 0;
  PHMuoTracksOut *particle = new PHMuoTracksv11();
  particle->AddPHParticle( itrk );
  particle->set_uid( itrk, trk_ptr->get()->get_key().get_obj_key() );

    // Track parameters at primary vertex
    const TMutTrkPar* trk_par = trk_ptr->get()->get_trk_par_vtx();
    if( trk_par ) particle->set_charge(itrk, static_cast<int>(trk_ptr->get()->get_charge()) );

    particle->set_chisquare( itrk, trk_ptr->get()->get_w_chi_square_pdf() );
    particle->set_ndf( itrk, trk_ptr->get()->get_ndf() );

    particle->set_px(0, itrk, trk_par->get_px());
    particle->set_py(0, itrk, trk_par->get_py());
    particle->set_pz(0, itrk, trk_par->get_pz());
    particle->set_xpos(0, itrk, trk_par->get_x());
    particle->set_ypos(0, itrk, trk_par->get_y());
    particle->set_zpos(0, itrk, trk_par->get_z());

    // Covariance matrix at primary vertex
    //
    for (int iarr1=0; iarr1<5; iarr1++)
    {
      for (int iarr2=0; iarr2<5; iarr2++)
      { particle->set_cov(iarr1, iarr2, itrk, trk_par->get_covar(iarr1, iarr2)); }
    }

    // Track parameters at gap 0 of each station
    for (int sta=0; sta<3; ++sta)
    {
      const TMutTrkPar* sta_trk_par = trk_ptr->get()->get_trk_par_station(sta);
      if( sta_trk_par )
      {
        particle->set_px(sta+1, itrk, sta_trk_par->get_px());
        particle->set_py(sta+1, itrk, sta_trk_par->get_py());
        particle->set_pz(sta+1, itrk, sta_trk_par->get_pz());
        particle->set_xpos(sta+1, itrk, sta_trk_par->get_x());
        particle->set_ypos(sta+1, itrk, sta_trk_par->get_y());
        particle->set_zpos(sta+1, itrk, sta_trk_par->get_z());
      }
    }

    //Add the new kalman projection at Gap0
    if( !trk_ptr->get()->get_trk_par_list()->empty() )
    {
      const TMutTrkPar& track_par( trk_ptr->get()->get_trk_par_list()->back() );
      particle->set_px(4,itrk, track_par.get_px());
      particle->set_py(4,itrk, track_par.get_py());
      particle->set_pz(4,itrk, track_par.get_pz());
      particle->set_xpos(4,itrk, track_par.get_x());
      particle->set_ypos(4,itrk, track_par.get_y());
      particle->set_zpos(4,itrk, track_par.get_z());
    }

    // Hit bit pattern
    UShort_t pattern( trk_ptr->get()->get_hit_pattern() );
    particle->set_muTRhits(itrk, pattern);

    // get number of hits on track from hit_pattern
    int n_hit = 0;
    static const int numberOfCathodes( 16 );
    for( int i=0; i < numberOfCathodes; i++ ) { if( pattern & ( 1 << i ) ) n_hit++; }
    particle->set_nhits(itrk, n_hit );

    // All MUTOO ghost tracks have already been removed.
    // all remaining tracks have ghostflag set to 0
    //particle->set_ghostflag(itrk,0);
    particle->set_TMutTrk_status(itrk,trk_ptr->get()->get_status());

    // loop over associated roads
    TMuiRoadMapO::const_key_iterator mui_iter = trk_ptr->get()->get_associated<TMuiRoadO>();

    short deepest_depth = 0;
    while(TMuiRoadMapO::const_pointer mui_ptr = mui_iter.next())
    {

      const short iroad = (mui_ptr->get()->get_depth())-2;
      particle->set_muIDOOhits(iroad, itrk, mui_ptr->get()->get_gapbit());

      // road parameters
      const TMutFitPar* road_par = mui_ptr->get()->get_const_fitpar();
      if( !road_par ) continue;

      particle->set_muIDOOchi(iroad, itrk, road_par->get_chi_square());

      PHPoint gap0_point = mui_ptr->get()->get_gap0_point();
      particle->set_muIDOO_gap0(0,iroad,itrk,(gap0_point.getX()));
      particle->set_muIDOO_gap0(1,iroad,itrk,(gap0_point.getY()));
      particle->set_muIDOO_gap0(2,iroad,itrk,(gap0_point.getZ()));
      particle->set_muIDOO_gap0(3,iroad,itrk,road_par->get_dxdz());
      particle->set_muIDOO_gap0(4,iroad,itrk,road_par->get_dydz());

      if (mui_ptr->get()->get_depth() > deepest_depth)
      {
        deepest_depth = mui_ptr->get()->get_depth();

        for(int igap=0; igap<5; igap++){
          particle->set_muid_hit_x(igap, itrk, -8888.);
          particle->set_muid_hit_y(igap, itrk, -8888.);
        }

        // get the distance between the current road and its associated clusters (index=0)
        // retrieve associated clusters
        TMuiClusterMapO::key_iterator iClust = mui_ptr->get()->get_associated<TMuiClusterO>();

        // loop over clusters
        int index=0;
        while(TMuiClusterMapO::pointer pClust = iClust.next())
        {

	  if(pClust->get()->get_orientation() == kVERT)
	    particle->set_muid_hit_x(pClust->get()->get_plane(), itrk, pClust->get()->get_centroidpos().getX());
	  else
	    particle->set_muid_hit_y(pClust->get()->get_plane(), itrk, pClust->get()->get_centroidpos().getY());

          index++;
        }

      } // depth check
    } // loop over associated roads

    // fill gap coordinate charge difference and errors
    TMutGapCoordMap::const_key_iterator gap_coord_iter( trk_ptr->get()->get_associated< TMutGapCoord >() );
    while( TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next() )
    {

      // get associated coordinates
      TMutCoordMap::const_key_iterator coord_iter( gap_coord_ptr->get()->get_associated< TMutCoord >() );
      if( coord_iter.count() != 2 ) {
        cout << "MWGOOReco::do_muons_OO - gap coordinate does not match 2 coordinates. skipped" << endl;
        continue;
      }

      // save charges and error
      float q[2] = {0,0};
      float q_error[2] = {0,0};
      while( TMutCoordMap::const_pointer coord_ptr = coord_iter.next() )
      {
        q[ coord_ptr->get()->get_cathode() ] = static_cast<float>(coord_ptr->get()->get_q_tot());
        q_error[ coord_ptr->get()->get_cathode() ] = static_cast<float>(coord_ptr->get()->get_q_error());
      }

      // add 2D dependant correction to charge in cathode 1
      float correction( static_cast<float>(gap_coord_ptr->get()->get_charge_corr()));
      q[1]/=(1+correction);
      q_error[1]/=(1+correction);

      // calculate delta_q and error
      float delta_q = 2*( q[1]-q[0] )/( q[1] + q[0] );
      float delta_q_error = 4 *
        sqrt(
	     MUTOO::SQUARE( q[1]*q_error[0] ) +
	     MUTOO::SQUARE( q[0]*q_error[1] )) /
        MUTOO::SQUARE( q[0] + q[1] );

      // calculate gap coordinate location and append to track
      unsigned int index = gap_coord_ptr->get()->get_station()*3+gap_coord_ptr->get()->get_gap();
      particle->set_delta_q( index, itrk, delta_q );
      particle->set_delta_q_error( index, itrk, delta_q_error );

    }

    // particle bent plane momentum at station 1
    const TMutBPPar* bp_par( trk_ptr->get()->get_bp_par() );
    if( bp_par )
    {
      particle->set_st1_bp_P(0,itrk, bp_par->get_px_st1() );
      particle->set_st1_bp_P(1,itrk, bp_par->get_py_st1() );
      particle->set_st1_bp_P(2,itrk, bp_par->get_pz_st1() );
    }

    // Increment the track if particle OK...
    itrk++;

  // resize array
  particle->set_npart(itrk);
*/

  //
  _mu_charge = trk_ptr->get()->get_charge();
  _mu_px = trk_ptr->get()->get_trk_par_vtx()->get_px();
  _mu_py = trk_ptr->get()->get_trk_par_vtx()->get_py();
  _mu_pz = trk_ptr->get()->get_trk_par_vtx()->get_pz();
  _mu_p  = sqrt(_mu_px*_mu_px+_mu_py*_mu_py+_mu_pz*_mu_pz);
  _mu_pt = sqrt(_mu_px*_mu_px+_mu_py*_mu_py);
  _mu_chi2 = trk_ptr->get()->get_w_chi_square_pdf();

  _mu_ghost = 0;
  if ( trk_ptr->get()->get_ghost() ) _mu_ghost = 1;

/*
  PHMuoTracksOut *muo = particle;
  int ipart = 0;

  MWGCuts mwg_cuts;

  // get best road
  int iroad = mwg_cuts.get_best_road_oo( ipart, muo );

  //=== First track variables
  _mu_muIDquad0 = (muo->get_muIDOO_gap0( 0, iroad, ipart )>0) + 2*(muo->get_muIDOO_gap0( 1, iroad, ipart )<0);
  _mu_muIDchis0 = muo->get_muIDOOchi(iroad,ipart);
  _mu_muTRhits0 = muo->get_muTRhits(ipart);
  _mu_muIDhits0 = muo->get_muIDOOhits(iroad,ipart);

  _mu_dS30 = Tools::DS3(muo,ipart, iroad);
  _mu_DG0 = Tools::DG0(muo,ipart, iroad);
  _mu_DDG0 = Tools::DDG0(muo,ipart, iroad);
  _mu_dS3ctp0 = Tools::DS3ctp(muo,ipart, iroad );
  _mu_DS0 = Tools::DS0(muo,ipart, iroad );

  _mu_lastGap = 0;
  for(int igap=4; igap>0; igap--)
  {
    if (muo->is_muIDOOhit( iroad, ipart, igap, 0) || muo->is_muIDOOhit( iroad, ipart, igap, 1 ))
    {
      _mu_lastGap = igap;
      break;
    }
  }
  _mu_mutr_nhits = Tools::sumbit( _mu_muTRhits0 );
  _mu_muid_nhits = Tools::sumbit( _mu_muIDhits0 );

  if ( _mu_p == _mu_pz ) _mu_eta = -9999;
  else _mu_eta = 0.5*log( (_mu_p+_mu_pz)/(_mu_p-_mu_pz) );

  delete particle;
*/
}
