// $Id: FvtxEval.cxx,v 1.15 2017/07/13 19:24:26 phnxbld Exp $

/*!
  \file FvtxEval.cxx
  \ingroup supermodules
  \brief reads TMutMCtrkMap and create TFvtxMCHit accordingly
  \author Hugo Pereira
  \version $Revision: 1.15 $
  \date $Date: 2017/07/13 19:24:26 $
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
#include <PHTrackIntegratorKF.h>
#include <TMutKalmanUtil.h>
#include <TMutTrkMap.h>
#include <TMutHitMap.h>
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
#include <PHIODataNode.h>
#include <getClass.h>
#include <VtxOut.h>
#include<TMutExtVtx.h>
//#include <PHMuoTracksv11.h>
#include <TMutGapCoordMap.h>
//#include <TMutCoordMap.h>
//#include <MWGCuts.h>
//#include <Tools.h>

#include <SvxSegmentList.h>

#include <PHCylPoint.h>
#include <PHGeometry.h>

#include <FvtxGeom.h>
#include "FvtxEval.h"

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
FvtxEval::FvtxEval( const char* name, const char* file ) :
  SubsysReco( name ),
  _fit_model(0),
  _event(0),
  _trk_eval_tree(0),
  _cluster_tree(0),
  _svx_cluster_tree(0),
  _fvtx_hit_tree(0),
  _event_tree(0),
  _event_hit_tree(0),
  _event_coord_tree(0),
  _coord_eval_tree(0),
  _straight_trk_eval_tree(0),
  _timer( new PHTimer(name) ),
  _file_name( file ),
  _signalNodeName("TOP"),
  _signal_top_node(0),
  _fvtx_trk_map(0),
  _fvtx_hit_map(0),
  _fvtx_clus_map(0),
  _svx_clus_map(0),
  _fvtx_coord_map(0),
  _do_match_mod(false),
  _do_mutr_matching(true)
{}

//______________________________________________________
FvtxEval::~FvtxEval()
{}

//_____________________________________
int FvtxEval::Init(PHCompositeNode *top_node)
{
  FVTXOO::PRINT( cout, "FvtxEval::Init" );

  // Interface Object Containers (IOCS)
  TMutNode<TFvtxEvalMap>::new_node( top_node, "TFvtxEvalMap");

  // Module parameter tables
  TMutNode<mFvtxEvalPar>::new_node( top_node,"mFvtxEvalPar");
  TMutNode<mFvtxEvalPar>::find_node( top_node,"mFvtxEvalPar");

  //! open TFile
  PHTFileServer::get().open( _file_name, "RECREATE" );
  cout << "FvtxEval::Init: writing to file \"" << _file_name << "\"" << endl;

  _eval_trees.clear();

  book_trk_eval_tree();
  FVTXOO::PRINT( cout, "**" );

  book_cluster_tree();
  FVTXOO::PRINT( cout, "**" );

  book_svx_cluster_tree();
  FVTXOO::PRINT( cout, "**" );

  book_fvtx_hit_tree();
  FVTXOO::PRINT( cout, "**" );

  book_event_tree();
  FVTXOO::PRINT( cout, "**" );

  book_event_hit_tree();
  FVTXOO::PRINT( cout, "**" );

  book_event_coord_tree();
  FVTXOO::PRINT( cout, "**" );

  book_coord_eval_tree();
  FVTXOO::PRINT( cout, "**" );

  book_straight_trk_eval_tree();
  FVTXOO::PRINT( cout, "**" );

  return 0;
}

//_____________________________________
int FvtxEval::InitRun(PHCompositeNode *top_node)
{
  _event = 0;

  // Instantiate FVTXOO analysis modules
  _mFvtxEvalMod = new mFvtxEval();
  FVTXOO::TRACE("Done with setup_modules ");

  // set topnode names from recoconst
  recoConsts *rc = recoConsts::instance();
  if ( rc->FlagExist("EMBED_MC_TOPNODE") )
  {
    cout << "FvtxEval::InitRun - reading _signalNodeName from recoConst EMBED_MC_TOPNODE" << endl;
    SetSignalNodeName( rc->get_CharFlag("EMBED_MC_TOPNODE") );
  }

  cout << "FvtxEval::InitRun - _signalNodeName : " << _signalNodeName << endl;

  return 0;
}


//______________________________________________________
int FvtxEval::process_event(PHCompositeNode *top_node)
{
  _event++;

  _timer->restart();
  try {
    
    set_interface_pointers( top_node );

    // Run the fvtxoo evaluation module
    //_mFvtxEvalMod->event( _signal_top_node, top_node );

    fill_trk_eval_tree();
    fill_cluster_tree();
    fill_svx_cluster_tree();
    fill_fvtx_hit_tree();
    fill_event_tree();
    fill_event_hit_tree();
    fill_event_coord_tree();
    fill_coord_eval_tree();
    fill_straight_trk_eval_tree();
    
  } catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  _timer->stop();
  return 0;
}

//______________________________________________________
int FvtxEval::End(PHCompositeNode* top_node)
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
  if ( _trk_eval_tree ) _trk_eval_tree->AutoSave();
  if ( _cluster_tree ) _cluster_tree->AutoSave();
  if ( _svx_cluster_tree ) _svx_cluster_tree->AutoSave();
  if ( _fvtx_hit_tree ) _fvtx_hit_tree->AutoSave();
  if ( _event_tree ) _event_tree->AutoSave();
  if ( _event_hit_tree ) _event_hit_tree->AutoSave();
  if ( _event_coord_tree ) _event_coord_tree->AutoSave();
  if ( _coord_eval_tree ) _coord_eval_tree->AutoSave();
  if ( _straight_trk_eval_tree ) _straight_trk_eval_tree->AutoSave();
  return 0;
}

//______________________________________________________
void FvtxEval::set_interface_pointers( PHCompositeNode* top_node )
{
  _top_node = top_node;
  _signal_top_node = Fun4AllServer::instance()->topNode( _signalNodeName );

  // maps

  try {  _fvtx_trk_map = TMutNode<TFvtxTrkMap>::find_node( top_node, "TFvtxTrkMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  if(_do_mutr_matching)
    {
      try { _mut_hit_map = TMutNode<TMutHitMap>::find_node( top_node, "TMutHitMap" ); }
      catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }
    }
  else
    {
      _mut_hit_map = 0;
    }
      
  if(_do_mutr_matching)
    {
      try{ _mut_gap_coord_map = TMutNode<TMutGapCoordMap>::find_node(top_node,"TMutGapCoordMap"); }
      catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }
    }
  else
    {
      _do_mutr_matching = 0;
    }


  try { _fvtx_hit_map = TMutNode<TFvtxHitMap>::find_node( top_node, "TFvtxHitMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_clus_map = TMutNode<TFvtxClusMap>::find_node( top_node, "TFvtxClusMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_coord_map = TMutNode<TFvtxCoordMap>::find_node( top_node, "TFvtxCoordMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _svx_clus_map = TMutNode<TFvtxSvxClusterMap>::find_node( top_node, "TFvtxSvxClusterMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  try { _fvtx_eval_map = TMutNode<TFvtxEvalMap>::find_node( top_node, "TFvtxEvalMap" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

  // vtxout node
  try { _vtxout_node = MuonUtil::find_io_node<VtxOut>( "VtxOut" ); }
  catch (const std::exception& e) { FVTXOO::TRACE(e.what()); }

}

//______________________________________________________
// Evaluation tree to compare MC track information to reconstructed track information
//
void FvtxEval::book_trk_eval_tree( void )
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _trk_eval_tree = new TTree( "trk_eval", "trk_eval" );
  _trk_eval_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "column1", &_column1, "column1/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "column2", &_column2, "column2/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "column3", &_column3, "column3/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "column4", &_column4, "column4/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "sector1", &_sector1, "sector1/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "sector2", &_sector2, "sector2/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "sector3", &_sector3, "sector3/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "sector4", &_sector4, "sector4/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "size1", &_size1, "size1/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "size2", &_size2, "size2/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "size3", &_size3, "size3/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "size4", &_size4, "size4/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "nseg", &_nseg, "nseg/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "vtxx", &_vtxx, "vtxx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "vtxy", &_vtxy, "vtxy/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "vtxz", &_vtxz, "vtxz/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "vtxxp", &_vtxxp, "vtxxp/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "vtxyp", &_vtxyp, "vtxyp/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "vtxzp", &_vtxzp, "vtxzp/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x0reco", &_x0reco, "x0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y0reco", &_y0reco, "y0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z0reco", &_z0reco, "z0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "px0reco", &_px0reco, "px0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "py0reco", &_py0reco, "py0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "pz0reco", &_pz0reco, "pz0reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chargereco", &_chargereco, "chargereco/F", BUFFER_SIZE );
  _trk_eval_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chisqreco", &_chisqreco, "chisqreco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chisqrecopdf_w", &_chisqrecopdf_w, "chisqrecopdf_w/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "chisqrecopdf_r", &_chisqrecopdf_r, "chisqrecopdf_r/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "ittrk",  &_ittrk,  "ittrk/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "idtrk",  &_idtrk,  "idtrk/I", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r1meas_svx", &_r1meas_svx, "r1meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1meas_svx", &_z1meas_svx, "z1meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi1meas_svx", &_phi1meas_svx, "phi1meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x1reco_svx", &_x1reco_svx, "x1reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y1reco_svx", &_y1reco_svx, "y1reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1reco_svx", &_z1reco_svx, "z1reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x1meas_svx", &_x1meas_svx, "x1meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y1meas_svx", &_y1meas_svx, "y1meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1meas_svx", &_z1meas_svx, "z1meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r2meas_svx", &_r2meas_svx, "r2meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2meas_svx", &_z2meas_svx, "z2meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi2meas_svx", &_phi2meas_svx, "phi2meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x2reco_svx", &_x2reco_svx, "x2reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y2reco_svx", &_y2reco_svx, "y2reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2reco_svx", &_z2reco_svx, "z2reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x2meas_svx", &_x2meas_svx, "x2meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y2meas_svx", &_y2meas_svx, "y2meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2meas_svx", &_z2meas_svx, "z2meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r3meas_svx", &_r3meas_svx, "r3meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3meas_svx", &_z3meas_svx, "z3meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi3meas_svx", &_phi3meas_svx, "phi3meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x3reco_svx", &_x3reco_svx, "x3reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y3reco_svx", &_y3reco_svx, "y3reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3reco_svx", &_z3reco_svx, "z3reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x3meas_svx", &_x3meas_svx, "x3meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y3meas_svx", &_y3meas_svx, "y3meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3meas_svx", &_z3meas_svx, "z3meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r4meas_svx", &_r4meas_svx, "r4meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4meas_svx", &_z4meas_svx, "z4meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi4meas_svx", &_phi4meas_svx, "phi4meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x4reco_svx", &_x4reco_svx, "x4reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y4reco_svx", &_y4reco_svx, "y4reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4reco_svx", &_z4reco_svx, "z4reco_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x4meas_svx", &_x4meas_svx, "x4meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y4meas_svx", &_y4meas_svx, "y4meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4meas_svx", &_z4meas_svx, "z4meas_svx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r1meas", &_r1meas, "r1meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1meas", &_z1meas, "z1meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi1meas", &_phi1meas, "phi1meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "q1", &_q1, "q1/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "w1", &_w1, "w1/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x1reco", &_x1reco, "x1reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y1reco", &_y1reco, "y1reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z1reco", &_z1reco, "z1reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r2meas", &_r2meas, "r2meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2meas", &_z2meas, "z2meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi2meas", &_phi2meas, "phi2meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "q2", &_q2, "q2/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "w2", &_w2, "w2/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x2reco", &_x2reco, "x2reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y2reco", &_y2reco, "y2reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z2reco", &_z2reco, "z2reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r3meas", &_r3meas, "r3meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3meas", &_z3meas, "z3meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi3meas", &_phi3meas, "phi3meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "q3", &_q3, "q3/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "w3", &_w3, "w3/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x3reco", &_x3reco, "x3reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y3reco", &_y3reco, "y3reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z3reco", &_z3reco, "z3reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "r4meas", &_r4meas, "r4meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4meas", &_z4meas, "z4meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "phi4meas", &_phi4meas, "phi4meas/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "q4", &_q4, "q4/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "w4", &_w4, "w4/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "x4reco", &_x4reco, "x4reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "y4reco", &_y4reco, "y4reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "z4reco", &_z4reco, "z4reco/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "nhitsb", &_nhitsb, "nhitsb/I", BUFFER_SIZE );
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
  _trk_eval_tree->Branch( "nsvxclusters", &_nsvxclusters, "nsvxclusters/I", BUFFER_SIZE );
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
  _trk_eval_tree->Branch( "extrapx", &_extrapx, "extrapx/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "extrapy", &_extrapy, "extrapy/D", BUFFER_SIZE );
  _trk_eval_tree->Branch( "extrapz", &_extrapz, "extrapz/D", BUFFER_SIZE );


  cout << "_trk_eval_tree booked" << endl;

  _eval_trees.push_back(_trk_eval_tree);

}

//______________________________________________________
void FvtxEval::fill_trk_eval_tree( void )
{
  static int ievent = 0;
  ievent++;

  //Get SVX n_segments info (indicates whether a vertex should be found or not):

  SvxSegmentList *d_segment;

  PHTypedNodeIterator<SvxSegmentList> segmentiter(_top_node);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  if ( !SvxSegmentListNode ) {
    //    cout << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;
    _nseg = -999;
  } else {
    d_segment = (SvxSegmentList*)SvxSegmentListNode->getData();
    _nseg = d_segment->get_nSegments();
  }

  if ( _vtxout_node )
  {
    //cout << "vertex (" << (_vtxout_node->get_Vertex()).getX() << ", " << (_vtxout_node->get_Vertex()).getY() << ", " << (_vtxout_node->get_Vertex()).getZ() << ")" << endl;
    //cout << "Simvertex " << (_vtxout_node->get_Vertex("SIM")).getX() << ", " << (_vtxout_node->get_Vertex("SIM")).getY() << ", " << (_vtxout_node->get_Vertex("SIM")).getZ() << endl;
    _vtxx = _vtxout_node->get_Vertex("SVX").getX();
    _vtxy = _vtxout_node->get_Vertex("SVX").getY();
    _vtxz = _vtxout_node->get_Vertex("SVX").getZ();
    _vtxxp = _vtxout_node->get_Vertex("SVX_PRECISE").getX();
    _vtxyp = _vtxout_node->get_Vertex("SVX_PRECISE").getY();
    _vtxzp = _vtxout_node->get_Vertex("SVX_PRECISE").getZ();
  }
  else
  {
    //    cout << "_vtxout_node not found " << endl;
    _vtxx = -999;
    _vtxy = -999;
    _vtxz = -999;
    _vtxxp = -999;
    _vtxyp = -999;
    _vtxzp = -999;
  }

  // loop over TFvtxMCHits
  if( _fvtx_trk_map )
  {
    TFvtxTrkMap::iterator iter( _fvtx_trk_map->range() );
    while( TFvtxTrkMap::const_pointer ptr = iter.next() )
    {

      _arm = -9999;
      _x0reco = _y0reco = _z0reco = -9999.0;
      _px0reco = _py0reco = _pz0reco = -9999.0;
      _chargereco = 0.0;
      //      _event = 0;
      _chisqreco = -9999.0;
      _chisqrecopdf_w = -9999.0;
      _chisqrecopdf_r = -9999.0;
      _ittrk = 99999; _idtrk = 99999;
      _sector1 = _sector2 = _sector3 = _sector4 = -999;
      _column1 = _column2 = _column3 = _column4 = -999;
      _r1meas = _r2meas= _r3meas = _r4meas = -9999.0;
      _z1meas = _z2meas= _z3meas = _z4meas = -9999.0;
      _phi1meas = _phi2meas= _phi3meas = _phi4meas = -9999.0;
      _x1reco = _y1reco = _z1reco = -9999.0;
      _x2reco = _y2reco = _z2reco = -9999.0;
      _x3reco = _y3reco = _z3reco = -9999.0;
      _x4reco = _y4reco = _z4reco = -9999.0;
      _x1reco_svx = _y1reco_svx = _z1reco_svx = -9999.0;
      _x2reco_svx = _y2reco_svx = _z2reco_svx = -9999.0;
      _x3reco_svx = _y3reco_svx = _z3reco_svx = -9999.0;
      _x4reco_svx = _y4reco_svx = _z4reco_svx = -9999.0;
      _x1meas_svx = _y1meas_svx = _z1meas_svx = -9999.0;
      _x2meas_svx = _y2meas_svx = _z2meas_svx = -9999.0;
      _x3meas_svx = _y3meas_svx = _z3meas_svx = -9999.0;
      _x4meas_svx = _y4meas_svx = _z4meas_svx = -9999.0;
      _r1meas_svx = _r2meas_svx = _r3meas_svx = _r4meas_svx = -9999.0;
      _phi1meas_svx = _phi2meas_svx = _phi3meas_svx = _phi4meas_svx = -9999.0;
      _nhitsb = _nhitsf = 0;
      _x0recoerr = _y0recoerr = -9999.0;
      _ncoords = 0;
      _ncoordsShared = 0;
      _parent_pdgid = 0;
      _pxmutr = _pymutr = _pzmutr = -9999.0;
      _chi2mutr = -9999.0;

//       if (ptr->get()->get_w_chi_square() > 10.0
//             || !ptr->get()->get_reco_success()) continue;
      TFvtxCoordMap::key_iterator trk_coord_iter = ptr->get()->get_associated<TFvtxCoord>();
      TFvtxSvxClusterMap::key_iterator trk_svx_iter = ptr->get()->get_associated<TFvtxSvxCluster>();

      //if (trk_coord_iter.count() < 4) continue;

      _coordIds.assign(0);
      _coordPhiStart.assign(0);
      _coordPhiEnd.assign(0);
      _coordR.assign(0);
      _coordZ.assign(0);
      _ncoords = trk_coord_iter.count();
      _nsvxclusters = trk_svx_iter.count();

      int cnt = 0;
      std::vector<double> r, z;
      while( TFvtxCoordMap::const_pointer trk_coord_ptr = trk_coord_iter.next() ){
        double wcoord = trk_coord_ptr->get()->get_w();
        PHCylPoint mid = trk_coord_ptr->get()->get_coord_midpoint();
        PHCylPoint end = trk_coord_ptr->get()->get_coord_end();
        PHCylPoint begin = trk_coord_ptr->get()->get_coord_begin();
        _phi_begin = FVTXOO::angle_normalize(begin.getPhi().getPhi());
        _phi_end = FVTXOO::angle_normalize(end.getPhi().getPhi());
        // Get associated cluster
        TFvtxClusMap::key_iterator clus_iter = trk_coord_ptr->get()->get_associated<TFvtxClus>();
        TFvtxClusMap::const_pointer clus_ptr = clus_iter.current();
        _sector = trk_coord_ptr->get()->get_sector();
        _column = trk_coord_ptr->get()->get_column();
  
        // get associated hits
        double qtotal = 0;
        TFvtxHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TFvtxHit>();
        if( hit_iter.count() ) {
          int i = 0;
          while( TFvtxHitMap::const_pointer hit_ptr = hit_iter.next() ){
            qtotal += hit_ptr->get()->get_q();
            i++;
          }
        }

        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station1){
          _coord1 = trk_coord_ptr->get()->get_key().get_obj_key();
          _r1meas = mid.getR();
          _z1meas = mid.getZ();
          _phi1meas = (_phi_begin+_phi_end)/2.0;
          _size1 = hit_iter.count();
          _q1 = qtotal;
          _w1 = wcoord;
          _sector1 = _sector;
          _column1 = _column;
        }
        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station2){
          _coord2 = trk_coord_ptr->get()->get_key().get_obj_key();
          _r2meas = mid.getR();
          _z2meas = mid.getZ();
          _phi2meas = (_phi_begin+_phi_end)/2.0;
          _size2 = hit_iter.count();
          _q2 = qtotal;
          _w2 = wcoord;
          _sector2 = _sector;
          _column2 = _column;
        }
        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station3){
          _coord3 = trk_coord_ptr->get()->get_key().get_obj_key();
          _r3meas = mid.getR();
          _z3meas = mid.getZ();
          _phi3meas = (_phi_begin+_phi_end)/2.0;
          _size3 = hit_iter.count();
          _q3 = qtotal;
          _w3 = wcoord;
          _sector3 = _sector;
          _column3 = _column;
        }
        if (trk_coord_ptr->get()->get_station() == FVTXOO::Station4){
          _coord4 = trk_coord_ptr->get()->get_key().get_obj_key();
          _r4meas = mid.getR();
          _z4meas = mid.getZ();
          _phi4meas = (_phi_begin+_phi_end)/2.0;
          _size4 = hit_iter.count();
          _q4 = qtotal;
          _w4 = wcoord;
          _sector4 = _sector;
          _column4 = _column;
        }

	TFvtxTrkMap::key_iterator i = trk_coord_ptr->get()->get_associated<TFvtxTrk>();
	if ( i.count() > 1 ) _ncoordsShared++;

	_coordIds[cnt] = trk_coord_ptr->get()->get_index();
	PHCylPoint pnt0 = trk_coord_ptr->get()->get_coord_begin();
	PHCylPoint pnt1 = trk_coord_ptr->get()->get_coord_end();
	_coordPhiStart[cnt] = FVTXOO::angle_normalize(pnt0.getPhi().getPhi());
	_coordPhiEnd[cnt] = FVTXOO::angle_normalize(pnt1.getPhi().getPhi());
	_coordR[cnt] = mid.getR();
	_coordZ[cnt] = mid.getZ();
	r.push_back(_coordR[cnt]);
	z.push_back(_coordZ[cnt]);

        //Protect against exceeding array bounds:
        if (cnt < 100) cnt++;
      }

      while( TFvtxSvxClusterMap::const_pointer trk_svx_ptr = trk_svx_iter.next() )
	{
	  const SvxCluster *clus = trk_svx_ptr->get()->get_cluster();
	  // location & ID
	  //_hitID = clus->get_hitID();
	  //_svxSection = clus->get_svxSection();
	  _layer = clus->get_layer();
	  //_ladder = clus->get_ladder();
	  //_sensor = clus->get_sensor();
	  
	  // Coordinates 
	  _x_global = clus->get_xyz_global(0);
	  _y_global = clus->get_xyz_global(1);
	  _z_global = clus->get_xyz_global(2);
	  
	  if (_layer == 0) // Inner layer of pixels
	    {
	      _r1meas_svx = sqrt(_x_global*_x_global + _y_global*_y_global);
	      _z1meas_svx = _z_global;
	      _phi1meas_svx = atan2(_y_global,_x_global);
	      _x1meas_svx = _x_global;
	      _y1meas_svx = _y_global;
	      _z1meas_svx = _z_global;

	    }
	  if (_layer == 1) // Outer layer of pixels
	    {
	      _r2meas_svx = sqrt(_x_global*_x_global + _y_global*_y_global);
	      _z2meas_svx = _z_global;
	      _phi2meas_svx = atan2(_y_global,_x_global);
	      _x2meas_svx = _x_global;
	      _y2meas_svx = _y_global;
	      _z2meas_svx = _z_global;
	    }
	  if (_layer == 2) // Inner layer of strip pixels
	    {
	      _r3meas_svx = sqrt(_x_global*_x_global + _y_global*_y_global);
	      _z3meas_svx = _z_global;
	      _phi3meas_svx = atan2(_y_global,_x_global);
	      _x3meas_svx = _x_global;
	      _y3meas_svx = _y_global;
	      _z3meas_svx = _z_global;
	    }
	  if (_layer == 3) // Outer layer of strip pixels
	    {
	      _r4meas_svx = sqrt(_x_global*_x_global + _y_global*_y_global);
	      _z4meas_svx = _z_global;
	      _phi4meas_svx = atan2(_y_global,_x_global);
	      _x4meas_svx = _x_global;
	      _y4meas_svx = _y_global;
	      _z4meas_svx = _z_global;
	    }
	}

      boost::tie(_r_slopeFit,_r_offsetFit) = linearFit(z,r);


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
      for( vector<TMutTrkPar>::const_iterator iter = trk_par_list.begin(); iter != trk_par_list.end(); iter++ ){
	double _rtmp = sqrt(iter->get_x()*iter->get_x()+iter->get_y()*iter->get_y());
	if (fabs(iter->get_z()) < 18.0 && _rtmp < 4.0){
	  _x1reco_svx = iter->get_x();
	  _y1reco_svx = iter->get_y();
	  _z1reco_svx = iter->get_z();
	}
	if (fabs(iter->get_z()) < 18.0 && _rtmp > 4.0 && _rtmp < 6.0){
	  _x2reco_svx = iter->get_x();
	  _y2reco_svx = iter->get_y();
	  _z2reco_svx = iter->get_z();
	}
	if (fabs(iter->get_z()) < 18.0 && _rtmp > 10.0 && _rtmp < 14.0 ){
	  _x3reco_svx = iter->get_x();
	  _y3reco_svx = iter->get_y();
	  _z3reco_svx = iter->get_z();
	}
	if (fabs(iter->get_z()) < 18.0 && _rtmp > 15.0 && _rtmp < 19.0 ){
	  _x4reco_svx = iter->get_x();
	  _y4reco_svx = iter->get_y();
	  _z4reco_svx = iter->get_z();
	}
        if (fabs(iter->get_z()) > 18.0 && fabs(iter->get_z()) < 21.5){
          _x1reco = iter->get_x();
          _y1reco = iter->get_y();
          _z1reco = iter->get_z();
        }
        if (fabs(iter->get_z()) > 23.0 && fabs(iter->get_z()) < 28){
          _x2reco = iter->get_x();
          _y2reco = iter->get_y();
          _z2reco = iter->get_z();
        }
        if (fabs(iter->get_z()) > 28.0 && fabs(iter->get_z()) < 35.0){
          _x3reco = iter->get_x();
          _y3reco = iter->get_y();
          _z3reco = iter->get_z();
        }
        if (fabs(iter->get_z()) > 35.0 && fabs(iter->get_z()) < 41.0){
          _x4reco = iter->get_x();
          _y4reco = iter->get_y();
          _z4reco = iter->get_z();
        }
      }

      //Get mutr track extrapolation to FVTX station 3 (or 2 or 1 or 0 depending on what is present):
      _extrapx = -999;
      _extrapy = -999;
      if (abs(_z4reco)<50 && _pzmutr != 0){
        PHTrackIntegratorKF integrator;
        integrator.initialize( *ptr->get()->get_trk_par_mutr() );
        integrator.extrapolate ( _z4reco );

        TMutTrkPar extrap_trk_par;
        if (integrator.get_error()){
          cout << "in FvtxEval extraploation to FVTX failed" << endl;
        } else {
          integrator.finish( extrap_trk_par );
          _extrapx = extrap_trk_par.get_x();
          _extrapy = extrap_trk_par.get_y();
        }
      }
      else if ( abs(_z3reco)<50 && _pzmutr != 0)
	{
	  PHTrackIntegratorKF integrator;
	  integrator.initialize( *ptr->get()->get_trk_par_mutr() );
	  integrator.extrapolate ( _z3reco );
	  
	  TMutTrkPar extrap_trk_par;
	  if (integrator.get_error())
	    {
	      cout << "in FvtxEval extraploation to FVTX failed" << endl;	      
	    } 
	  else 
	    {
	      integrator.finish( extrap_trk_par );
	      _extrapx = extrap_trk_par.get_x();
	      _extrapy = extrap_trk_par.get_y();
	    }
	}
      else if ( abs(_z2reco)<50 && _pzmutr != 0)
	{
	  PHTrackIntegratorKF integrator;
	  integrator.initialize( *ptr->get()->get_trk_par_mutr() );
	  integrator.extrapolate ( _z2reco );
	  
	  TMutTrkPar extrap_trk_par;
	  if (integrator.get_error())
	    {
	      cout << "in FvtxEval extraploation to FVTX failed" << endl;	      
	    } 
	  else 
	    {
	      integrator.finish( extrap_trk_par );
	      _extrapx = extrap_trk_par.get_x();
	      _extrapy = extrap_trk_par.get_y();
	    }
	}
      else if ( abs(_z1reco)<50 && _pzmutr != 0)
	{
	  PHTrackIntegratorKF integrator;
	  integrator.initialize( *ptr->get()->get_trk_par_mutr() );
	  integrator.extrapolate ( _z1reco );
	  
	  TMutTrkPar extrap_trk_par;
	  if (integrator.get_error())
	    {
	      cout << "in FvtxEval extraploation to FVTX failed" << endl;	      
	    } 
	  else 
	    {
	      integrator.finish( extrap_trk_par );
	      _extrapx = extrap_trk_par.get_x();
	      _extrapy = extrap_trk_par.get_y();
	    }
	}
      else if ( abs(_z4reco_svx)<50 && _pzmutr != 0)
	{
	  PHTrackIntegratorKF integrator;
	  integrator.initialize( *ptr->get()->get_trk_par_mutr() );
	  integrator.extrapolate ( _z4reco_svx );
	  
	  TMutTrkPar extrap_trk_par;
	  if (integrator.get_error())
	    {
	      cout << "in FvtxEval extraploation to FVTX failed" << endl;	      
	    } 
	  else 
	    {
	      integrator.finish( extrap_trk_par );
	      _extrapx = extrap_trk_par.get_x();
	      _extrapy = extrap_trk_par.get_y();
	    }
	}
      else if ( abs(_z3reco_svx)<50 && _pzmutr != 0)
	{
	  PHTrackIntegratorKF integrator;
	  integrator.initialize( *ptr->get()->get_trk_par_mutr() );
	  integrator.extrapolate ( _z3reco_svx );
	  
	  TMutTrkPar extrap_trk_par;
	  if (integrator.get_error())
	    {
	      cout << "in FvtxEval extraploation to FVTX failed" << endl;	      
	    } 
	  else 
	    {
	      integrator.finish( extrap_trk_par );
	      _extrapx = extrap_trk_par.get_x();
	      _extrapy = extrap_trk_par.get_y();
	    }
	}
      else if ( abs(_z2reco_svx)<50 && _pzmutr != 0)
	{
	  PHTrackIntegratorKF integrator;
	  integrator.initialize( *ptr->get()->get_trk_par_mutr() );
	  integrator.extrapolate ( _z2reco_svx );
	  
	  TMutTrkPar extrap_trk_par;
	  if (integrator.get_error())
	    {
	      cout << "in FvtxEval extraploation to FVTX failed" << endl;	      
	    } 
	  else 
	    {
	      integrator.finish( extrap_trk_par );
	      _extrapx = extrap_trk_par.get_x();
	      _extrapy = extrap_trk_par.get_y();
	    }
	}

      _chisqreco = ptr->get()->get_w_chi_square();
      _chisqrecopdf_w = ptr->get()->get_w_chi_square_pdf();
      _chisqrecopdf_r = ptr->get()->get_r_chi_square_pdf();
      //      _event = ievent;


      if( _nhitsb == 0 ) {
        TFvtxSvxClusterMap::const_key_iterator iter = ptr->get()->get_associated<TFvtxSvxCluster>();
        _nhitsb = iter.count();
      }

      if( _trk_eval_tree ) _trk_eval_tree->Fill();

    }  // Loop on TFvtxTrks
  }  // If TFvtxTrk exists

  return;

}

//______________________________________________________
void FvtxEval::book_cluster_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _cluster_tree = new TTree( "cluster", "cluster" );
  _cluster_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _cluster_tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _cluster_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _cluster_tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _cluster_tree->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
  _cluster_tree->Branch( "column", &_column, "column/I", BUFFER_SIZE );
  _cluster_tree->Branch( "size", &_size, "size/I", BUFFER_SIZE );
  _cluster_tree->Branch( "strip", &_strip[0], "strip[size]/I", BUFFER_SIZE );
  _cluster_tree->Branch( "q", &_q[0], "q[size]/D", BUFFER_SIZE );
  _cluster_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _cluster_tree->Branch( "ncoords", &_ncoords, "ncoords/I", BUFFER_SIZE );
  _cluster_tree->Branch( "w", &_w, "w/D", BUFFER_SIZE );
  _cluster_tree->Branch( "xbegin", &_xbegin, "xbegin/D", BUFFER_SIZE );
  _cluster_tree->Branch( "xend", &_xend, "xend/D", BUFFER_SIZE );
  _cluster_tree->Branch( "ybegin", &_ybegin, "ybegin/D", BUFFER_SIZE );
  _cluster_tree->Branch( "yend", &_yend, "yend/D", BUFFER_SIZE );
  _cluster_tree->Branch( "zbegin", &_zbegin, "zbegin/D", BUFFER_SIZE );
  _cluster_tree->Branch( "zend", &_zend, "zend/D", BUFFER_SIZE );
  _cluster_tree->Branch( "nhits", &_nhits, "nhits/I", BUFFER_SIZE );
  _cluster_tree->Branch( "res", &_res, "res/D", BUFFER_SIZE );
  _cluster_tree->SetAutoSave( AUTO_SAVE );
  cout << "_cluster_tree booked" << endl;

  _eval_trees.push_back(_cluster_tree);

}
//______________________________________________________
void FvtxEval::fill_cluster_tree( void )
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
      _w = _xbegin = _ybegin = _zbegin = _xend = _yend = _zend = -9999;
      if ( coord_iter.count() ) {
        _w = coord_iter->get()->get_w();

        PHPoint begin = coord_iter->get()->get_coord_begin();
        _xbegin = begin.getX();
        _ybegin = begin.getY();
        _zbegin = begin.getZ();
        PHPoint end = coord_iter->get()->get_coord_end();
        _xend = end.getX();
        _yend = end.getY();
        _zend = end.getZ();

      }

      if( _cluster_tree ) _cluster_tree->Fill();

    }
  }

  return;

}

//______________________________________________________
void FvtxEval::book_svx_cluster_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _svx_cluster_tree = new TTree( "svx_cluster", "svx_cluster" );
  _svx_cluster_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "hitID", &_hitID, "hitID/I", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "svxSection", &_svxSection, "svxSection/I", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "layer", &_layer, "layer/I", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "ladder", &_ladder, "ladder/I", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "sensor", &_sensor, "sensor/I", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "x_global", &_x_global, "x_global/D", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "y_global", &_y_global, "y_global/D", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "z_global", &_z_global, "z_global/D", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "x_local", &_x_local, "x_local/D", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "y_local", &_y_local, "y_local/D", BUFFER_SIZE );
  _svx_cluster_tree->Branch( "z_local", &_z_local, "z_local/D", BUFFER_SIZE );
  _svx_cluster_tree->SetAutoSave( AUTO_SAVE );
  cout << "_svx_cluster_tree booked" << endl;

  _eval_trees.push_back(_svx_cluster_tree);

}

//______________________________________________________
void FvtxEval::fill_svx_cluster_tree( void )
{
  static int ievent = 0;
  ievent++;

  // loop over TFvtxClusters
  if( _svx_clus_map )
  {
    TFvtxSvxClusterMap::iterator iter( _svx_clus_map->range() );
    while( TFvtxSvxClusterMap::const_pointer ptr = iter.next() )
    {
      const SvxCluster *clus = ptr->get()->get_cluster();
      // location & ID
      _hitID = clus->get_hitID();
      _svxSection = clus->get_svxSection();
      _layer = clus->get_layer();
      _ladder = clus->get_ladder();
      _sensor = clus->get_sensor();

      // Coordinates 
      _x_global = clus->get_xyz_global(0);
      _y_global = clus->get_xyz_global(1);
      _z_global = clus->get_xyz_global(2);

      _x_local = clus->get_xyz_local(0);
      _y_local = clus->get_xyz_local(1);
      _z_local = clus->get_xyz_local(2);

      if( _svx_cluster_tree ) _svx_cluster_tree->Fill();

    }
  }

  return;

}

//______________________________________________________
void FvtxEval::book_fvtx_hit_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _fvtx_hit_tree = new TTree( "fvtx_hit", "fvtx_hit" );
  _fvtx_hit_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "plane", &_plane, "plane/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "column", &_column, "column/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "size", &_size, "size/I", BUFFER_SIZE );
  //_fvtx_hit_tree->Branch( "strip", &_strip[0], "strip[size]/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "strip", &_fvtx_strip, "strip[size]/s", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "xbegin", &_xbegin, "xbegin/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "xend", &_xend, "xend/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "ybegin", &_ybegin, "ybegin/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "yend", &_yend, "yend/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "zbegin", &_zbegin, "zbegin/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "zend", &_zend, "zend/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "qhit", &_qhit, "qhit/D", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "adc", &_adc, "adc/s", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "packet_id", &_packet_id, "packet_id/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "fem_id", &_fem_id, "fem_id/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "chip_id", &_chip_id, "chip_id/I", BUFFER_SIZE );
  _fvtx_hit_tree->Branch( "chan_id", &_chan_id, "chan_id/I", BUFFER_SIZE );
  _fvtx_hit_tree->SetAutoSave( AUTO_SAVE );
  cout << "_fvtx_hit_tree booked" << endl;

  _eval_trees.push_back(_fvtx_hit_tree);

}
//______________________________________________________
void FvtxEval::fill_fvtx_hit_tree( void )
{
  // loop over TFvtxMCHits
  if( _fvtx_hit_map )
  {
    TFvtxHitMap::iterator iter( _fvtx_hit_map->range() );
    while( TFvtxHitMap::const_pointer ptr = iter.next() )
    {
      _xbegin = 0.0;
      _ybegin = 0.0;
      _zbegin = 0.0;
      _xend = 0.0;
      _yend = 0.0;
      _zend = 0.0;

      // location
      _arm = ptr->get()->get_arm();
      _cage = ptr->get()->get_cage();
      _station = ptr->get()->get_station();
      _sector = ptr->get()->get_sector();
      _column = ptr->get()->get_column();
      _qhit = ptr->get()->get_q();
      _adc = ptr->get()->get_adc();

      _fvtx_strip = ptr->get()->get_strip();

      // Get position of strip:
      try {
        FvtxStrip* strip_ptr = FvtxGeom::get_arm( ptr->get()->get_arm())->
          get_cage(ptr->get()->get_cage())->
          get_station(ptr->get()->get_station())->
          get_sector(ptr->get()->get_sector())->
          get_column(ptr->get()->get_column())->
          get_strip(ptr->get()->get_strip());

        _packet_id = strip_ptr->get_packet_id();
        _fem_id = strip_ptr->get_fem_id();
        _chip_id = strip_ptr->get_chip_id();
        _chan_id = strip_ptr->get_dcm_channel();

        PHPoint coord_begin = strip_ptr->get_position_begin();
        PHPoint coord_end = strip_ptr->get_position_end();
        _xbegin = coord_begin.getX();
        _ybegin = coord_begin.getY();
        _zbegin = coord_begin.getZ();
        _xend = coord_end.getX();
        _yend = coord_end.getY();
        _zend = coord_end.getZ();
      } catch (const std::exception& e) {
        std::ostringstream s;
        s << "FvtxEval::fill_fvtx_hit_tree: Exception while processing strip"
          << " ("
          << _arm << "," << _cage << "," << _station << "," << _sector << "," << _column << "," << _fvtx_strip
          << "): " << e.what();
        FVTXOO::TRACE(s.str());
      }


      if( _fvtx_hit_tree ) _fvtx_hit_tree->Fill();

    }  // loop over TFvtxHits
  }  // If fvtx_hit_map exists

  return;
}
//______________________________________________________
void FvtxEval::book_event_coord_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _event_coord_tree = new TTree( "event_coord", "event_coord" );
  _event_coord_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _event_coord_tree->Branch( "iarmf", &_iarmf, "iarmf/I", BUFFER_SIZE );
  _event_coord_tree->Branch( "iarmm", &_iarmm, "iarmm/I", BUFFER_SIZE );
  _event_coord_tree->Branch( "ncoordsf", &_ncoordsf, "ncoordsf/I", BUFFER_SIZE );
  _event_coord_tree->Branch( "ncoordsm", &_ncoordsm, "ncoordsm/I", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposf1", &_rposf1, "rposf1/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposf1", &_zposf1, "zposf1/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposf1", &_phiposf1, "phiposf1/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposf2", &_rposf2, "rposf2/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposf2", &_zposf2, "zposf2/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposf2", &_phiposf2, "phiposf2/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposf3", &_rposf3, "rposf3/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposf3", &_zposf3, "zposf3/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposf3", &_phiposf3, "phiposf3/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposf4", &_rposf4, "rposf4/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposf4", &_zposf4, "zposf4/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposf4", &_phiposf4, "phiposf4/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm1", &_rposm1, "rposm1/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm1", &_zposm1, "zposm1/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm1", &_phiposm1, "phiposm1/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm2", &_rposm2, "rposm2/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm2", &_zposm2, "zposm2/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm2", &_phiposm2, "phiposm2/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm3", &_rposm3, "rposm3/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm3", &_zposm3, "zposm3/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm3", &_phiposm3, "phiposm3/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm4", &_rposm4, "rposm4/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm4", &_zposm4, "zposm4/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm4", &_phiposm4, "phiposm4/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm5", &_rposm5, "rposm5/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm5", &_zposm5, "zposm5/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm5", &_phiposm5, "phiposm5/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm6", &_rposm6, "rposm6/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm6", &_zposm6, "zposm6/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm6", &_phiposm6, "phiposm6/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm7", &_rposm7, "rposm7/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm7", &_zposm7, "zposm7/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm7", &_phiposm7, "phiposm7/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "rposm8", &_rposm8, "rposm8/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "zposm8", &_zposm8, "zposm8/D", BUFFER_SIZE );
  _event_coord_tree->Branch( "phiposm8", &_phiposm8, "phiposm8/D", BUFFER_SIZE );
  _event_coord_tree->SetAutoSave( AUTO_SAVE );
  cout << "_event_coord_tree booked" << endl;

  _eval_trees.push_back(_event_coord_tree);

}
//______________________________________________________
void FvtxEval::fill_event_coord_tree( void )
{
  if ( !_coord_eval_tree ) return;

  if ( !_fvtx_coord_map ) return;

  double rposf[4] = {-999.};
  double zposf[4] = {-999.};
  double phiposf[4] = {-999.};

  double rposm[8] = {-999.};
  double zposm[8] = {-999.};
  double phiposm[8] = {-999.};

  //Fill ntuple with fvtx coords:
  TFvtxCoordMap::iterator coord_iter = _fvtx_coord_map->range();
  _ncoordsf = _fvtx_coord_map->size();
  while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      _iarmf = coord_ptr->get()->get_arm();
      _cage = coord_ptr->get()->get_cage();
      _station = coord_ptr->get()->get_station();
      _sector = coord_ptr->get()->get_sector();
      _column = coord_ptr->get()->get_column();
      _index = coord_ptr->get()->get_index();


      PHCylPoint end = coord_ptr->get()->get_coord_end();
      PHCylPoint begin = coord_ptr->get()->get_coord_begin();
      PHCylPoint mid = coord_ptr->get()->get_coord_midpoint();

      _phi_begin = FVTXOO::angle_normalize(begin.getPhi().getPhi());
      _phi_end = FVTXOO::angle_normalize(end.getPhi().getPhi());
      _phipos = (_phi_begin + _phi_end)/2.0;
      _rpos = mid.getR();
      _zpos = mid.getZ();
      rposf[_station] = _rpos;
      zposf[_station] = _zpos;
      phiposf[_station] = _phipos;
    }

    if (_mut_gap_coord_map){
      TMutGapCoordMap::const_iterator gap_coord_iter = _mut_gap_coord_map->range();
      _ncoordsm = _mut_gap_coord_map->size();
      while(TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next()){
        float xpos = (gap_coord_ptr->get()->get_coord()).getX();
        float ypos = (gap_coord_ptr->get()->get_coord()).getY();
        _iarmm = gap_coord_ptr->get()->get_arm();
        _station = gap_coord_ptr->get()->get_station();
        _gap = gap_coord_ptr->get()->get_gap();
        _zpos = (double)(gap_coord_ptr->get()->get_coord()).getZ();
        _rpos = (double)sqrt(xpos*xpos + ypos*ypos);
        _phipos = (double)atan2(ypos, xpos);
        rposm[_station*3 + _gap] = _rpos;
        zposm[_station*3 + _gap] = _zpos;
        phiposm[_station*3 + _gap] = _phipos;
      }
    }

    _rposf1 = rposf[0];
    _zposf1 = zposf[0];
    _phiposf1 = phiposf[0];
    _rposf2 = rposf[1];
    _zposf2 = zposf[1];
    _phiposf2 = phiposf[1];
    _rposf3 = rposf[2];
    _zposf3 = zposf[2];
    _phiposf3 = phiposf[2];
    _rposf4 = rposf[3];
    _zposf4 = zposf[3];
    _phiposf4 = phiposf[3];

    _rposm1 = rposm[0];
    _zposm1 = zposm[0];
    _phiposm1 = phiposm[0];
    _rposm2 = rposm[1];
    _zposm2 = zposm[1];
    _phiposm2 = phiposm[1];
    _rposm3 = rposm[2];
    _zposm3 = zposm[2];
    _phiposm3 = phiposm[2];
    _rposm4 = rposm[3];
    _zposm4 = zposm[3];
    _phiposm4 = phiposm[3];
    _rposm5 = rposm[4];
    _zposm5 = zposm[4];
    _phiposm5 = phiposm[4];
    _rposm6 = rposm[5];
    _zposm6 = zposm[5];
    _phiposm6 = phiposm[5];
    _rposm7 = rposm[6];
    _zposm7 = zposm[6];
    _phiposm7 = phiposm[6];
    _rposm8 = rposm[7];
    _zposm8 = zposm[7];
    _phiposm8 = phiposm[7];
    _event_coord_tree->Fill();

  return;
}
//______________________________________________________
void FvtxEval::book_event_hit_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _event_hit_tree = new TTree( "event_hit", "event_hit" );
  _event_hit_tree->Branch( "event", &_event, "event/I", BUFFER_SIZE );
  _event_hit_tree->Branch( "rpos", &_rpos, "rpos/D", BUFFER_SIZE );
  _event_hit_tree->Branch( "zpos", &_zpos, "zpos/D", BUFFER_SIZE );
  _event_hit_tree->Branch( "phipos", &_phipos, "phipos/D", BUFFER_SIZE );
  _event_hit_tree->SetAutoSave( AUTO_SAVE );
  cout << "_event_hit_tree booked" << endl;

  _eval_trees.push_back(_event_hit_tree);

}
//______________________________________________________
void FvtxEval::fill_event_hit_tree( void )
{
  if ( !_coord_eval_tree ) return;

  if ( !_fvtx_coord_map ) return;

  //Fill ntuple with fvtx coords:
  TFvtxCoordMap::iterator coord_iter = _fvtx_coord_map->range();
  while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      _arm = coord_ptr->get()->get_arm();
      _cage = coord_ptr->get()->get_cage();
      _station = coord_ptr->get()->get_station();
      _sector = coord_ptr->get()->get_sector();
      _column = coord_ptr->get()->get_column();
      _index = coord_ptr->get()->get_index();


      PHCylPoint end = coord_ptr->get()->get_coord_end();
      PHCylPoint begin = coord_ptr->get()->get_coord_begin();
      PHCylPoint mid = coord_ptr->get()->get_coord_midpoint();

      _phi_begin = FVTXOO::angle_normalize(begin.getPhi().getPhi());
      _phi_end = FVTXOO::angle_normalize(end.getPhi().getPhi());
      _phipos = (_phi_begin + _phi_end)/2.0;
      _rpos = mid.getR();
      _zpos = mid.getZ();

      _event_hit_tree->Fill();
    }

    if (_mut_gap_coord_map){
      TMutGapCoordMap::const_iterator gap_coord_iter = _mut_gap_coord_map->range();
      while(TMutGapCoordMap::const_pointer gap_coord_ptr = gap_coord_iter.next()){
        float xpos = (gap_coord_ptr->get()->get_coord()).getX();
        float ypos = (gap_coord_ptr->get()->get_coord()).getY();
        _zpos = (double)(gap_coord_ptr->get()->get_coord()).getZ();
        _rpos = (double)sqrt(xpos*xpos + ypos*ypos);
        _phipos = (double)atan2(ypos, xpos);
        _event_hit_tree->Fill();
      }
    }

  return;
}
//______________________________________________________
void FvtxEval::book_event_tree( void )
{

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  _event_tree = new TTree( "event", "event" );
  _event_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _event_tree->Branch( "mut_arm", &_mut_arm, "mut_arm/I", BUFFER_SIZE );
  _event_tree->Branch( "nhits", &_nhits_ev, "nhits/I", BUFFER_SIZE );
  _event_tree->Branch( "nhits_mut", &_nhits_mut, "nhits_mut/I", BUFFER_SIZE );
  _event_tree->Branch( "narms", &_narms, "narms/I", BUFFER_SIZE );
  _event_tree->Branch( "nstations", &_nstations, "nstations/I", BUFFER_SIZE );
  _event_tree->Branch( "nstations_mut", &_nstations_mut, "nstations_mut/I", BUFFER_SIZE );
  _event_tree->Branch( "mut_octant", &_mut_octant, "mut_octant/I", BUFFER_SIZE );
  _event_tree->Branch( "sector0", &_sector0, "sector0/I", BUFFER_SIZE );
  _event_tree->Branch( "sector1", &_sector1, "sector1/I", BUFFER_SIZE );
  _event_tree->Branch( "sector2", &_sector2, "sector2/I", BUFFER_SIZE );
  _event_tree->Branch( "sector3", &_sector3, "sector3/I", BUFFER_SIZE );
  _event_tree->Branch( "strip0", &_strip0, "strip0/s", BUFFER_SIZE );
  _event_tree->Branch( "strip1", &_strip1, "strip1/s", BUFFER_SIZE );
  _event_tree->Branch( "strip2", &_strip2, "strip2/s", BUFFER_SIZE );
  _event_tree->Branch( "strip3", &_strip3, "strip3/s", BUFFER_SIZE );
  _event_tree->Branch( "xbegin0", &_xbegin0, "xbegin0/D", BUFFER_SIZE );
  _event_tree->Branch( "xbegin1", &_xbegin1, "xbegin1/D", BUFFER_SIZE );
  _event_tree->Branch( "xbegin2", &_xbegin2, "xbegin2/D", BUFFER_SIZE );
  _event_tree->Branch( "xbegin3", &_xbegin3, "xbegin3/D", BUFFER_SIZE );
  _event_tree->Branch( "ybegin0", &_ybegin0, "ybegin0/D", BUFFER_SIZE );
  _event_tree->Branch( "ybegin1", &_ybegin1, "ybegin1/D", BUFFER_SIZE );
  _event_tree->Branch( "ybegin2", &_ybegin2, "ybegin2/D", BUFFER_SIZE );
  _event_tree->Branch( "ybegin3", &_ybegin3, "ybegin3/D", BUFFER_SIZE );
  _event_tree->Branch( "zbegin0", &_zbegin0, "zbegin0/D", BUFFER_SIZE );
  _event_tree->Branch( "zbegin1", &_zbegin1, "zbegin1/D", BUFFER_SIZE );
  _event_tree->Branch( "zbegin2", &_zbegin2, "zbegin2/D", BUFFER_SIZE );
  _event_tree->Branch( "zbegin3", &_zbegin3, "zbegin3/D", BUFFER_SIZE );
  _event_tree->Branch( "xend0", &_xend0, "xend0/D", BUFFER_SIZE );
  _event_tree->Branch( "xend1", &_xend1, "xend1/D", BUFFER_SIZE );
  _event_tree->Branch( "xend2", &_xend2, "xend2/D", BUFFER_SIZE );
  _event_tree->Branch( "xend3", &_xend3, "xend3/D", BUFFER_SIZE );
  _event_tree->Branch( "yend0", &_yend0, "yend0/D", BUFFER_SIZE );
  _event_tree->Branch( "yend1", &_yend1, "yend1/D", BUFFER_SIZE );
  _event_tree->Branch( "yend2", &_yend2, "yend2/D", BUFFER_SIZE );
  _event_tree->Branch( "yend3", &_yend3, "yend3/D", BUFFER_SIZE );
  _event_tree->Branch( "zend0", &_zend0, "zend0/D", BUFFER_SIZE );
  _event_tree->Branch( "zend1", &_zend1, "zend1/D", BUFFER_SIZE );
  _event_tree->Branch( "zend2", &_zend2, "zend2/D", BUFFER_SIZE );
  _event_tree->Branch( "zend3", &_zend3, "zend3/D", BUFFER_SIZE );
  _event_tree->Branch( "event_num", &_event, "event_num/D", BUFFER_SIZE );
  _event_tree->Branch( "r0", &_r0, "r0/D", BUFFER_SIZE );
  _event_tree->Branch( "r1", &_r1, "r1/D", BUFFER_SIZE );
  _event_tree->Branch( "r2", &_r2, "r2/D", BUFFER_SIZE );
  _event_tree->Branch( "r3", &_r3, "r3/D", BUFFER_SIZE );
  _event_tree->Branch( "z0", &_z0, "z0/D", BUFFER_SIZE );
  _event_tree->Branch( "z1", &_z1, "z1/D", BUFFER_SIZE );
  _event_tree->Branch( "z2", &_z2, "z2/D", BUFFER_SIZE );
  _event_tree->Branch( "z3", &_z3, "z3/D", BUFFER_SIZE );
  _event_tree->Branch( "phi0", &_phi0, "phi0/D", BUFFER_SIZE );
  _event_tree->Branch( "phi1", &_phi1, "phi1/D", BUFFER_SIZE );
  _event_tree->Branch( "phi2", &_phi2, "phi2/D", BUFFER_SIZE );
  _event_tree->Branch( "phi3", &_phi3, "phi3/D", BUFFER_SIZE );
  _event_tree->Branch( "bbcz", &_bbcz, "bbcz/D", BUFFER_SIZE );
  _event_tree->Branch( "vtxz", &_vtxz, "vtxz/D", BUFFER_SIZE );
  _event_tree->Branch( "nseg", &_nseg, "nseg/I", BUFFER_SIZE );
  _event_tree->SetAutoSave( AUTO_SAVE );
  cout << "_event_tree booked" << endl;

  _eval_trees.push_back(_event_tree);

}
//______________________________________________________
void FvtxEval::fill_event_tree( void )
{

  int nhits_arm[2] = {0};
  int nhits_mut_arm[2] = {0};
  int nhits[2][4];  // number of hits per arm/station
  int nhits_mut[2][3];  // number of hits per arm/station
  int sector[2][4];  // number of hits per arm/station
  int strip[2][4];  // number of hits per arm/station
  int mut_octant[2][3];  // number of hits per arm/station
  double rpos[2][4];  // number of hits per arm/station
  double phipos[2][4];  // number of hits per arm/station
  double zpos[2][4];  // number of hits per arm/station
  double xbegin[2][4];  // number of hits per arm/station
  double ybegin[2][4];  // number of hits per arm/station
  double zbegin[2][4];  // number of hits per arm/station
  double xend[2][4];  // number of hits per arm/station
  double yend[2][4];  // number of hits per arm/station
  double zend[2][4];  // number of hits per arm/station

  //Get SVX n_segments info (indicates whether a vertex should be found or not):

  SvxSegmentList *d_segment;

  PHTypedNodeIterator<SvxSegmentList> segmentiter(_top_node);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  if ( !SvxSegmentListNode ) {
    //    cout << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl;
    _nseg = -999;
  } else {
    d_segment = (SvxSegmentList*)SvxSegmentListNode->getData();
    _nseg = d_segment->get_nSegments();
  }

  if ( _vtxout_node )
  {
    _vtxz = _vtxout_node->get_Vertex("SVX_PRECISE").getZ();
    _bbcz = _vtxout_node->get_Vertex("").getZ();
  }
  else
  {
    cout << "_vtxout_node not found " << endl;
    _vtxz = -999;
    _bbcz = -999;
  }

  for (int iarm=0; iarm < 2; iarm++)
    for (int ista=0; ista < 4; ista++){
      nhits[iarm][ista] = 0;
      strip[iarm][ista] = -999;
      sector[iarm][ista] = -999;
      rpos[iarm][ista] = -999;
      phipos[iarm][ista] = -999;
      zpos[iarm][ista] = -999;
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
      _zbegin = 0.0;
      _xend = 0.0;
      _yend = 0.0;
      _zend = 0.0;

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
        _zbegin = coord_begin.getZ();
        _xend = coord_end.getX();
        _yend = coord_end.getY();
        _zend = coord_end.getZ();
      } catch (const std::exception& e) {
        std::ostringstream s;
        s << "FvtxEval::fill_fvtx_hit_tree: Exception while processing strip"
          << " ("
          << _arm << "," << _cage << "," << _station << "," << _sector << "," << _column << "," << _fvtx_strip
          << "): " << e.what();
        FVTXOO::TRACE(s.str());
      }

      xbegin[_arm][_station] = _xbegin;
      ybegin[_arm][_station] = _ybegin;
      zbegin[_arm][_station] = _zbegin;
      xend[_arm][_station] = _xend;
      yend[_arm][_station] = _yend;
      zend[_arm][_station] = _zend;

    }  // loop over TFvtxHits
  }  // If fvtx_hit_map exists

  if ( _fvtx_coord_map ){
    TFvtxCoordMap::iterator coord_iter = _fvtx_coord_map->range();
    while ( TFvtxCoordMap::const_pointer coord_ptr = coord_iter.next() )
    {
      _arm = coord_ptr->get()->get_arm();
      _cage = coord_ptr->get()->get_cage();
      _station = coord_ptr->get()->get_station();
      _sector = coord_ptr->get()->get_sector();
      _column = coord_ptr->get()->get_column();
      _index = coord_ptr->get()->get_index();

      PHCylPoint end = coord_ptr->get()->get_coord_end();
      PHCylPoint begin = coord_ptr->get()->get_coord_begin();
      PHCylPoint mid = coord_ptr->get()->get_coord_midpoint();

      _phi_begin = FVTXOO::angle_normalize(begin.getPhi().getPhi());
      _phi_end = FVTXOO::angle_normalize(end.getPhi().getPhi());
      rpos[_arm][_station] = mid.getR();
      phipos[_arm][_station] = (_phi_begin+_phi_end)/2.0;
      zpos[_arm][_station] = mid.getZ();
    }
  }

  // loop over TMutMCHits
  for (int iarm=0; iarm<2; iarm++)
    nhits_mut_arm[iarm] = 0;

  if( _mut_hit_map )
  {
    TMutHitMap::iterator iter( _mut_hit_map->range() );
    _nhits_mut =  _mut_hit_map->size();
    while( TMutHitMap::const_pointer ptr = iter.next() )
    {

      // location
      _mut_arm = ptr->get()->get_arm();
      _mut_station = ptr->get()->get_station();
      _mut_half_octant = ptr->get()->get_half_octant();
      _mut_gap = ptr->get()->get_gap();
      _mut_cathode = ptr->get()->get_cathode();

      nhits_mut[_mut_arm][_mut_station]++;
      nhits_mut_arm[_mut_arm]++;
      mut_octant[_mut_arm][_mut_station] = ptr->get()->get_octant();
    }  // loop over TMutHits
  }  // If mut_hit_map exists

  _narms = 0;
  _nstations = 0;
  _nstations_mut = 0;
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
    if (nhits_mut_arm[iarm] > 0) _nstations_mut = 0;
    for (int ista=0; ista<4; ista++){
      if(ista < 3)
	{
	  if (nhits_mut[iarm][ista] > 0)
	    {
	      _nstations_mut++;
	      _mut_octant = mut_octant[iarm][ista];
	    }
	}
      if (nhits[iarm][ista] > 0){
        _nstations++;
        if (ista == 0){
          _sector0 = sector[iarm][ista];
          _strip0 = strip[iarm][ista];
          _xbegin0 = xbegin[iarm][ista];
          _ybegin0 = ybegin[iarm][ista];
          _zbegin0 = zbegin[iarm][ista];
          _xend0 = xend[iarm][ista];
          _yend0 = yend[iarm][ista];
          _zend0 = zend[iarm][ista];
          _r0 = rpos[iarm][ista];
          _phi0 = phipos[iarm][ista];
          _z0 = zpos[iarm][ista];
        }
        else if (ista == 1){
          _sector1 = sector[iarm][ista];
          _strip1 = strip[iarm][ista];
          _xbegin1 = xbegin[iarm][ista];
          _ybegin1 = ybegin[iarm][ista];
          _zbegin1 = zbegin[iarm][ista];
          _xend1 = xend[iarm][ista];
          _yend1 = yend[iarm][ista];
          _zend1 = zend[iarm][ista];
          _r1 = rpos[iarm][ista];
          _phi1 = phipos[iarm][ista];
          _z1 = zpos[iarm][ista];
        }
        else if (ista == 2){
          _sector2 = sector[iarm][ista];
          _strip2 = strip[iarm][ista];
          _xbegin2 = xbegin[iarm][ista];
          _ybegin2 = ybegin[iarm][ista];
          _zbegin2 = zbegin[iarm][ista];
          _xend2 = xend[iarm][ista];
          _yend2 = yend[iarm][ista];
          _zend2 = zend[iarm][ista];
          _r2 = rpos[iarm][ista];
          _phi2 = phipos[iarm][ista];
          _z2 = zpos[iarm][ista];
        }
        else if (ista == 3){
          _sector3 = sector[iarm][ista];
          _strip3 = strip[iarm][ista];
          _xbegin3 = xbegin[iarm][ista];
          _ybegin3 = ybegin[iarm][ista];
          _zbegin3 = zbegin[iarm][ista];
          _xend3 = xend[iarm][ista];
          _yend3 = yend[iarm][ista];
          _zend3 = zend[iarm][ista];
          _r3 = rpos[iarm][ista];
          _phi3 = phipos[iarm][ista];
          _z3 = zpos[iarm][ista];
        }
      }
    }
  }

  if( _event_tree ) _event_tree->Fill();

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
FvtxEval::book_coord_eval_tree( void )
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
  //_coord_eval_tree->Branch( "zbegin", &_zbegin, "zbegin/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "zend", &_zend, "zend/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "phibegin", &_phi_begin, "phibegin/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "phiend", &_phi_end, "phiend/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "r", &_r, "r/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "z", &_z, "z/D", BUFFER_SIZE );
  _coord_eval_tree->Branch( "nhits", &_nhits, "nhits/I", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "xmcc", &_xmcc, "xmcc/D", BUFFER_SIZE );
  //_coord_eval_tree->Branch( "ymcc", &_ymcc, "ymcc/D", BUFFER_SIZE );
  _coord_eval_tree->SetAutoSave( AUTO_SAVE );

  std::cout << "_coord_eval_tree booked" << endl;

  _eval_trees.push_back(_coord_eval_tree);

}


void
FvtxEval::fill_coord_eval_tree()
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
	  int i = 0;
	  while( TFvtxHitMap::const_pointer hit_ptr = hit_iter.next() )
	    {
	      _strip[i++] = hit_ptr->get()->get_strip();
	  }
      }

      _coord_eval_tree->Fill();
    }

  return;
}

void
FvtxEval::book_straight_trk_eval_tree( void )
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
FvtxEval::fill_straight_trk_eval_tree( void )
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

          // Protect against going beyond array bounds:
         if (ihit < 100) ihit++;
	}// coord_trk_map

      _ntrack_hits = ihit;

      _straight_trk_eval_tree->Fill();
    }// fvtx_trk_map

  return;
}

