// $Id: mMutKalFit.cxx,v 1.88 2017/07/11 16:13:14 phnxbld Exp $

/*!
   \file mMutKalFit.cxx
   \brief kalman filter for mutoo tracks.
   \author Hugo Pereira
   \version $Revision: 1.88 $
   \date $Date: 2017/07/11 16:13:14 $
*/

// IOC
#include<TMuiRoadMapO.h>
#include<MuiCommon.hh>

// MUTOO
#include<MUTOO.h>
#include <MUTGEOM.h>
#include<mMutKalFit.h>
#include<mMutKalFitPar.h>
#include<PHException.h>
#include<PHGeometry.h>
#include<PHTFileServer.h>
#include<PHTimer.h>
#include<PHTrackIntegratorKF.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutDatabaseInit.h>
#include<TMutExtVtx.h>
#include<TMutGeo.h>
#include<TMutKalmanUtil.h>
#include<TMutMeasureModel.h>
#include<TMutNode.h>
#include<TMutRecoPar.hh>
#include<TMutTrackUtil.h>
#include<TMutTrkPar.hh>

// GLOBAL
#include<PHLine.h>
#include<PHPoint.h>
#include<PHVector.h>

// MUIGEOM
#include<MuiGeomClasses.hh>

// MUTGEOM
#include <MutWire.h>

// STL/BOOST
#include<iostream>
#include<string>
#include<list>

using namespace std;

//______________________________________________________________________
mMutKalFit::mMutKalFit() :
  _timer(PHTimeServer::get()->insert_new("mMutKalFit") ),
  _top_node( 0 ),
  _mod_par( 0 ),
  _trk_map( 0 ),
  _filename( "" ),
  _residual_tree( 0 ),
  _arm( 0 ),
  _station( 0 ),
  _gap( 0 ),
  _cathode( 0 ),
  _chi_square( 0 ),
  _vertex_extrapolation_tree( 0 )
{
  _res.assign( 0 );
  _res_cov.assign( 0 );
  _p_up.assign( 0 );
  _p_down.assign( 0 );
  _error_up.assign( 0 );
  _error_down.assign( 0 );
  MUTOO::TRACE("initializing module mMutKalFit",MUTOO::ALOT);
}

//______________________________________________________________________
// Destructor
mMutKalFit::~mMutKalFit()
{
  finish_evaluation();
  _nodes.clear();
}

//______________________________________________________________________
bool mMutKalFit::init_evaluation()
{

  // retrieve evaluation mode
  const unsigned int& mode( _mod_par->get_evaluation_mode() );

  // if no evaluation, do nothing
  if( mode == mMutKalFitPar::NONE ) return true;

  // create filename
  _filename = _mod_par->get_evaluation_file();
  PHTFileServer::get().open( _filename, "RECREATE" );

  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  // residuals
  if( mode & mMutKalFitPar::RESIDUAL )
  {

    _residual_tree = new TTree( "residuals", "single hit residuals" );
    _residual_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
    _residual_tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
    _residual_tree->Branch( "gap", &_gap, "gap/I", BUFFER_SIZE );
    _residual_tree->Branch( "cathode", &_cathode, "cathode/I", BUFFER_SIZE );
    _residual_tree->Branch( "res", &_res[0], "res[3]/D", BUFFER_SIZE );
    _residual_tree->Branch( "res_cov", &_res_cov[0], "res_cov[3]/D", BUFFER_SIZE );
    _residual_tree->Branch( "chi_square", &_chi_square, "chi_square/D", BUFFER_SIZE );
    _residual_tree->SetAutoSave( AUTO_SAVE );

  }

  // vertex extrapolation
  if( mode & mMutKalFitPar::VERTEX_EXTRAPOLATION )
  {
    _vertex_extrapolation_tree = new TTree( "vtx_extrap", "vertex extrapolation_tree" );
    _vertex_extrapolation_tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
    _vertex_extrapolation_tree->Branch( "p_up", &_p_up[0], "p_up[3]/D", BUFFER_SIZE );
    _vertex_extrapolation_tree->Branch( "p_tot_up", &_p_tot_up, "p_tot_up/D", BUFFER_SIZE );
    _vertex_extrapolation_tree->Branch( "error_up", &_error_up[0], "error_up[5]/D", BUFFER_SIZE );
    _vertex_extrapolation_tree->Branch( "p_tot_down", &_p_tot_down, "p_tot_down/D", BUFFER_SIZE );
    _vertex_extrapolation_tree->Branch( "p_down", &_p_down[0], "p_down[3]/D", BUFFER_SIZE );
    _vertex_extrapolation_tree->Branch( "error_down", &_error_down[0], "error_down[5]/D", BUFFER_SIZE );
  }

  return true;
}

//______________________________________________________________________
void mMutKalFit::finish_evaluation( void )
{

  // check filename
  if( _filename.empty() ) return;

  // close TFile
  MUTOO::TRACE("mMutKalFit::finish_evaluation");
  PHTFileServer::get().write( _filename );

  return;
}

//______________________________________________________________________
PHBoolean mMutKalFit::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // initialize evaluation trees
    static bool init_done __attribute__ ((unused)) = init_evaluation();

    // load vertex z
    load_ext_vertex( top_node );

    // loop over tracks
    track_loop();

    if(_mod_par->get_verbosity() >= MUTOO::SOME) _trk_map->print();
    if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();

  } catch(exception& e) {

    MUTOO::TRACE(e.what());
    _timer.get()->stop();
    return False;

  }

  _timer.get()->stop();
  return True;

}

//______________________________________________________________________
void mMutKalFit::set_interface_ptrs(PHCompositeNode* top_node)
{
  _top_node = top_node;
  _mod_par = TMutNode<mMutKalFitPar>::find_node(top_node,"mMutKalFitPar");
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");
}

//______________________________________________________________________
void mMutKalFit::load_ext_vertex( PHCompositeNode* top_node )
{

  // reference z is used by default
  _vertex_z = _mod_par->get_z_reference();

  // try load external vertex
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( error )
  {

    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    cerr << "mMutKalFit::load_ext_vertex - wrong external vertex.\n";

  } else {
    _vertex_z = vtx.getZ();
  }

  return;

}

//______________________________________________________________________
void mMutKalFit::track_loop( void )
{
  // Loop over all track in the track map
  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while(TMutTrkMap::pointer trk_ptr = trk_iter.next())
  try {

    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    cout << "mMutKalFit::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;

    // perform basic checks on the track
    if( !accept_track( trk_ptr ) ) {
      trk_ptr->get()->set_kalman_fail();
      continue;
    }

    TMutKalmanFilter kalman_filter;
    kalman_filter.set_verbosity( _mod_par->get_verbosity() );
    kalman_filter.set_running_trk_par();

    // initialize starting parameters and nodes
    init( trk_ptr, kalman_filter );

    // perform full fit
    unsigned int iterations;
    if( !( iterations = fit_nodes( kalman_filter ) ) )
    {
      if( _mod_par->get_verbosity() >= MUTOO::SOME )
      cout << "mMutKalFit::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " fit failed." << endl;
      trk_ptr->get()->set_kalman_fail();
      continue;
    }

    // redo full fit taking anode corrections into account
    if( _mod_par->get_use_anodes() ) {

      calculate_anode_corrections();

      unsigned int iterations_2;
      if( !( iterations_2 = fit_nodes( kalman_filter ) ) )
      {
        if( _mod_par->get_verbosity() >= MUTOO::SOME )
        cout << "mMutKalFit::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " refit failed." << endl;
        trk_ptr->get()->set_kalman_fail();
        continue;
      }

      iterations += iterations_2;

    }

    // dump number of iterations
    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    { cout << "mMutKalFit::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " iterations=" << iterations << endl; }

    // fill the track with node informations
    fill_track( trk_ptr );

    // extrapolate track parameters toward vertex
    extrapolate_to_vertex( trk_ptr );

    // extrapolate track parameters toward muid first gap
    if( _mod_par->get_extrapolate_to_muid() )
    extrapolate_to_muid( trk_ptr );

    // fill evaluation tree if requested
    if( _mod_par->get_evaluation_mode() != mMutKalFitPar::NONE ) fill_evaluation_tree();

    // set track status
    if( !trk_ptr->get()->get_kalman_fail() ) trk_ptr->get()->set_kalman_fit();
  } catch( exception &e ) { cout << "mMutKalFit::track_loop - " << e.what() << endl; }

}

//______________________________________________________________________
bool mMutKalFit::accept_track( TMutTrkMap::const_pointer trk_ptr ) const
{

  // check ghost bit
  if(trk_ptr->get()->get_ghost()) return false;

  // check estimate bit (need for starting point)
  if( trk_ptr->get()->get_no_estimate() )
  {
    trk_ptr->get()->set_no_fit();
    trk_ptr->get()->set_ghost();
    if( _mod_par->get_verbosity()>= MUTOO::SOME )
    {
      cout << "mMutKalFit::accept_track - trk="
        << trk_ptr->get()->get_key().get_obj_key()
        << " NO_ESTIMATE - setting NO_FIT.\n";
    }
    return false;
  }

  // check low momentum bit
  if( trk_ptr->get()->get_low_mom() )
  {
    trk_ptr->get()->set_no_fit();
    trk_ptr->get()->set_ghost();
    if( _mod_par->get_verbosity()>= MUTOO::SOME )
    {
      cout << "mMutKalFit::accept_track - trk="
        << trk_ptr->get()->get_key().get_obj_key()
        << " LOW_MOM - setting NO_FIT.\n";
    }
    return false;
  }

  // check number of hits in cathode
  TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  if ( coord_iter.count() < _mod_par->get_min_n_coord() )
  {
    trk_ptr->get()->set_reco_min_hits();
    trk_ptr->get()->set_no_fit();
    trk_ptr->get()->set_ghost();

    if( _mod_par->get_verbosity()>= MUTOO::SOME )
    {
      cout << "mMutKalFit::accept_track - trk="
        << trk_ptr->get()->get_key().get_obj_key()
        << " not enough hits (" << coord_iter.count() << ").\n";
    }
    return false;
  }

  return true;

}

//______________________________________________________________________
void mMutKalFit::init(
  TMutTrkMap::pointer trk_ptr,
  TMutKalmanFilter& kalman_filter
)

{

  // make sure list of nodes is empty
  _nodes.clear();

  // stores reference to starting parameters localy and initialize
  TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );
   start_kf._direction = ( trk_ptr->get()->get_arm() == MUTOO::South ) ? -1:1;

  //  adds coordinate associated to the track to the kalman filter object
  // retrieve coordinates associated to the track
  list<TMutCoordMap::value_type> coord_list;
  typedef list< TMutCoordMap::value_type >::iterator coord_iterator;

  TMutCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()) coord_list.push_back( *coord_ptr );

  // sort the list of coordinates from vertex to muid
  coord_list.sort( coord_less_ftor() );

  // add measurements
  for( coord_iterator coord = coord_list.begin(); coord != coord_list.end();  coord++ )
  {

    // look if it is a desactivated detectors
    if(_mod_par->get_mutr_desactivated(
      coord->get()->get_arm(),
      coord->get()->get_station(),
      coord->get()->get_gap(),
      coord->get()->get_cathode()))
    {

      if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      cout
          << "mMutKalFit::init - coord ["
          << coord->get()->get_arm() << ","
          << coord->get()->get_station() << ","
          << coord->get()->get_gap() << ","
          << coord->get()->get_cathode() << "]"
          << " ignored" << endl;
      continue;

    }

    // create new node
    KalmanFilterNode node( &(*coord) );

    /*
      weight covariance matrix. All errors are devided by the weight
      the smaller the weight, the smaller the corresponding station contributes to the chisquare
    */
    node.get_measurement_cov() *= MUTOO::SQUARE( 1.0/_mod_par->get_mutr_detector_weight(
      coord->get()->get_arm(),
      coord->get()->get_station(),
      coord->get()->get_gap(),
      coord->get()->get_cathode() ) );

    // add node
    _nodes.push_back( node );

  }

  // get track associated road, if any
  TMuiRoadMapO::key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );

  /*
    adds muid clusters associated to the track to the kalman filter object
    if requested and if only one associated road is found
  */
  if( _mod_par->get_use_muid() && road_iter.count() > 0 )
  {

    // set association to best muid road clusters
    associate_road( trk_ptr );

    list<TMuiClusterMapO::value_type> mui_clus_list;
    typedef list< TMuiClusterMapO::value_type >::iterator mui_clus_iterator;

    TMuiClusterMapO::key_iterator clus_iter = road_iter->get()->get_associated<TMuiClusterO>();
    while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
    {

      // look if it is a desactivated detectors
      if(_mod_par->get_muid_desactivated(
        clus_ptr->get()->get_arm(),
        clus_ptr->get()->get_plane(),
        clus_ptr->get()->get_panel() )) continue;

      mui_clus_list.push_back( *clus_ptr );

    }

    mui_clus_list.sort( mui_clus_less_ftor() );

    // add measurement
    for( mui_clus_iterator clus = mui_clus_list.begin(); clus != mui_clus_list.end(); clus++ )
    _nodes.push_back( KalmanFilterNode( &(*clus) ) );

    /*
      if fit starts from muid, gets the muid road track parameters (x,y,dxdz,dydz)
      use the momentum from the track
    */
    start_kf._z = mui_clus_list.back().get()->get_mean_z();
    const TMutTrkPar& trk_par( *trk_ptr->get()->get_trk_par_station( MUTOO::Station3 ) );

    // make the extrapolation from trk_par to gap 0
    PHTrackIntegratorKF integrator;
    integrator.initialize( trk_par );
    integrator.extrapolate( start_kf._z );
    TMutTrkPar trk_par_extrap;
    if( integrator.get_error() ) {
      cout << "mMutKalFit::init - error extrapolating trk_par to gap0" <<  endl;
      trk_par_extrap = trk_par;
    } else integrator.finish( trk_par_extrap );

    start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par_extrap );

  } else {

    // set z from last coordinate
    start_kf._z = coord_list.back().get()->get_mean_z();

    // extrapolate track parameters to _z
    const TMutTrkPar trk_par( *trk_ptr->get()->get_trk_par() );
    TMutTrkPar trk_par_extrap(
      trk_par.get_x() + trk_par.get_dxdz()*(start_kf._z - trk_par.get_z()),
      trk_par.get_y() + trk_par.get_dydz()*(start_kf._z - trk_par.get_z()),
      start_kf._z,
      trk_par.get_px(),
      trk_par.get_py(),
      trk_par.get_pz(),
      trk_par.get_charge() );

    start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par_extrap );

  }

  // in both cases, use the default covariance matrix for error
  start_kf._covar_kf(0, 0) = MUTOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) );  // error on c/p
  start_kf._covar_kf(1, 1) = MUTOO::SQUARE( _mod_par->get_angular_resolution() );  // error on dx/dz
  start_kf._covar_kf(2, 2) = MUTOO::SQUARE( _mod_par->get_angular_resolution() );  // error on dy/dz
  start_kf._covar_kf(3, 3) = MUTOO::SQUARE( _mod_par->get_position_resolution() ); // error on x
  start_kf._covar_kf(4, 4) = MUTOO::SQUARE( _mod_par->get_position_resolution() ); // error on y

  // sort nodes from vertex to muid
  _nodes.sort();

  // dump all nodes
  if( _mod_par->get_verbosity() >= MUTOO::MAX )
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) {
    MUTOO::PRINT( cout, "mMutKalFit::init_nodes - node");
    cout << "measurement" << node->get_measurement();
    cout << "covariance" << node->get_measurement_cov();
    cout << "h" << node->get_h();
  }

  if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
    cout << "mMutKalFit::init - trk="
      << trk_ptr->get()->get_key().get_obj_key()
      << " start parameters"
      << endl;
    start_kf.print();
  }

}

//_________________________________________________________________
void mMutKalFit::calculate_anode_corrections( void )
{

  // calculate anode corrections if required; redo the fit
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node ++ ) {

    // check node has associated coordinate
    if( !node->has_coord() ) continue;

    //! calculate anode correction from associated coord
    PHPoint trk_point(
      node->get_smoothed()._state_kf(3,0),
      node->get_smoothed()._state_kf(4,0),
      node->get_smoothed()._z );

    MutWire *wire = TMutGeo::find_nearest_anode( node->get_arm(), trk_point );
    if( wire ) {

      PHLine line(wire->getGlobalPositionBegin(), wire->getGlobalPositionEnd());
      PHPoint projected( PHGeometry::closestApproachLinePoint( line, trk_point ) );

      const PHGslMatrix &h( node->get_h() );
      node->get_measurement()(0,0) -= ( projected.getX() - trk_point.getX() )*h( 0, 3 ) + ( projected.getY() - trk_point.getY() )*h( 0, 4 );

    }
  }

}


//___________________________________________________________________
unsigned int mMutKalFit::fit_nodes( TMutKalmanFilter &kalman_filter )
{

  // kalman_filter.set_verbosity( MUTOO::ALOT );

  // loop over max number of iterations
  unsigned int iteration( 0 );
  double old_chi_square = 1e6;
  for( ; iteration < _mod_par->get_max_iterations(); iteration++ ) {

    double chi_square( 0 );

    // reset node status
    reset_nodes();

    // perform prediction and filter using reverse iterator
    for( reverse_node_iterator node = _nodes.rbegin(); node != _nodes.rend(); node ++ )
    if( kalman_filter.predict( *node ) && kalman_filter.filter( *node ) ) chi_square += node->get_chi_square();
    else {
      if( _mod_par->get_verbosity()>= MUTOO::SOME )
      cout << "mMutKalFit::fit_nodes - prediction/filter failed.\n";
      return 0;
    }

    // initialize smoothing starting from first node (closest to vertex)
    bool first_node( true );
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node ++ )
    if( first_node ) {
      first_node = false;
      kalman_filter.initialize_smoother( *node );
    } else if( !kalman_filter.smooth( *node ) ) {
      cout << "mMutKalFit::fit_nodes - prediction/filter failed.\n";
      return 0;
    }

    /*
      update starting parameters
      update is done before stop coundition check since used at refit stage event
      after the last iteration.
      Only position/momentum and charge should be updated.
    */

    // retrieves last node from the list
    KalmanFilterNode& last_node( _nodes.back() );

    // stores reference to starting parameters localy
    TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );
    start_kf._state_kf = last_node.get_smoothed()._state_kf;
    start_kf._z = last_node.get_smoothed()._z;
    start_kf._direction = last_node.get_smoothed()._direction;

    if( _mod_par->get_update_cov_matrix() ) {

      /*
        this is mathematicaly wrong since starting parameter
        covariance matrix should be kept large.
        Unfortunately, this improves resolution a lot
      */
      start_kf._covar_kf = last_node.get_smoothed()._covar_kf;

    } else {

      /*
        update the first term of the cov matrix using new p_tot
        since the meaningfull error given as argument  is dp_tot/p_tot
        whereas the error used in the matrix is c/p_tot
      */
      start_kf._covar_kf(0, 0) = MUTOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) );  // error on c/p

    }

    if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
      MUTOO::TRACE( "mMutKalFit::fit_nodes - start parameters [updated]");
      start_kf.print();
    }

    // dump chisquare
    if( _mod_par->get_verbosity()>=MUTOO::SOME )
    cout << "mMutKalFit::fit_nodes - iteration " << iteration << ", chi2=" << chi_square << endl;

    if( iteration && fabs( chi_square - old_chi_square ) < _mod_par->get_chi_cut()*old_chi_square ) break;
    old_chi_square = chi_square;

  }

  if( _mod_par->get_verbosity() >= MUTOO::SOME )
  cout << "mMutKalFit::fit_nodes - " << iteration << " iterations.\n";

  return iteration+1;
}

//___________________________________________
bool mMutKalFit::fill_track( TMutTrkMap::pointer trk_ptr )
{

  if( _mod_par->get_verbosity() >= MUTOO::SOME )
  cout << "mMutKalFit::fill_track - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;

  // initialize track lists
  trk_ptr->get()->clear_trk_par_list();
  trk_ptr->get()->clear_w_residual_list();
  trk_ptr->get()->clear_r_residual_list();

  // initialize w chi_square
  double chi_square_w( 0 );

  // loop over nodes
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) {

    // add smoothed trk_par and associated reco_par into track
    PHVector mom_mutoo = TMutKalmanUtil::mom_kalman_to_mutoo(
      trk_ptr->get()->get_arm(),
      PHVector(
        node->get_smoothed()._state_kf( 0,0 ),
        node->get_smoothed()._state_kf( 1,0 ),
        node->get_smoothed()._state_kf( 2,0 )
      )
    );

    TMutTrkPar trk_par(
      node->get_smoothed()._state_kf( 3, 0 ),
      node->get_smoothed()._state_kf( 4, 0 ),
      node->get_z(),
      mom_mutoo.getX(),
      mom_mutoo.getY(),
      mom_mutoo.getZ(),
      ( node->get_smoothed()._state_kf( 0,0 ) < 0 ) ? -1:1, node->get_chi_square() );

    PHGslMatrix cov_mutoo( TMutKalmanUtil::cov_kalman_to_mutoo(
      trk_par.get_charge(),
      node->get_smoothed()._covar_kf,
      mom_mutoo ) );

    for( unsigned int i=0; i<TMutTrkPar::COVAR_ROW; i++ )
    for( unsigned int j=0; j<TMutTrkPar::COVAR_ROW; j++ )
    trk_par.set_covar( i, j, cov_mutoo(i,j) );

    trk_ptr->get()->push_trk_par( trk_par );

    if( node->has_coord() )
    {

      // store chi_square contribution into coord
      node->get_coord().get()->push_chi_sqr_inc(
        trk_ptr->get()->get_key().get_obj_key(),
        node->get_chi_square() );

      // push residuals
      push_r_residual( trk_ptr, trk_par, node->get_coord() );
      push_w_residual( trk_ptr, trk_par, node->get_coord() );

    }

    // add node chi_square to total
    chi_square_w += node->get_chi_square();

  }

  // fill track chi_squares
  trk_ptr->get()->set_w_chi_square(chi_square_w);

  // the r chisquare is set to 0 since it makes no sense with kalman filter fit.
  trk_ptr->get()->set_r_chi_square(0);

  // fill track number of freedom (number of nodes - number of parameters)
  trk_ptr->get()->set_ndf( _nodes.size() - 5 );

  /*
    fill track hit pattern. Calling the baseclass method
    TMutTrk::get_hit_pattern calculates the hit pattern from coord to track associations
    starting from TMutTrk_v4, this hit pattern is stored internaly and not recalculated
  */
  trk_ptr->get()->set_hit_pattern( trk_ptr->get()->TMutTrk::get_hit_pattern() );

  // set track parameters in first gap
  trk_ptr->get()->set_trk_par(  trk_ptr->get()->get_trk_par_list()->front() );

  return true;
}

//_____________________________________________________________________________
bool mMutKalFit::extrapolate_to_vertex( TMutTrkMap::pointer trk_ptr )
{

  if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
    MUTOO::PRINT( cout, "mMutKalFit::extrapolate_to_vertex - before extrapolation");
    TMutKalmanUtil::print_trk_par_kf( trk_ptr->get()->get_trk_par_list()->front() );
  }

  PHTrackIntegratorKF integrator;
  const TMutTrkPar& down_trk_par( trk_ptr->get()->get_trk_par_list()->front() );
  integrator.initialize( down_trk_par );

  // store downstream momentum and error
  if( _mod_par->get_evaluation_mode() & mMutKalFitPar::VERTEX_EXTRAPOLATION )
  {

    _p_down[0] = down_trk_par.get_px();
    _p_down[1] = down_trk_par.get_py();
    _p_down[2] = down_trk_par.get_pz();
    _p_tot_down = sqrt(
      MUTOO::SQUARE( _p_down[0] ) +
      MUTOO::SQUARE( _p_down[1] ) +
      MUTOO::SQUARE( _p_down[2] ) );

    const PHGslMatrix& error_square_kf = integrator.get_covar_kf();
    for( unsigned int i=0; i<5; i++ )
    { _error_down[i] = sqrt( error_square_kf(i,i) ); }

  }

  // extrapolate to z_ref
  integrator.extrapolate( _vertex_z );
  if( integrator.get_error() )
  {
    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    {
      cout
        << "mMutKalFit::extrapolate_to_vertex - extrapolation towards vertex failed ("
        << trk_ptr->get()->get_trk_par_list()->front().get_z() << "->" << _vertex_z << ")\n";
    }

    trk_ptr->get()->set_kalman_fail();
    if( _mod_par->get_evaluation_mode() & mMutKalFitPar::VERTEX_EXTRAPOLATION )
    {
      _p_up.assign(0);
      _p_tot_up = 0;
      _error_up.assign(0);
    }

    return false;

  } else {

    TMutTrkPar extrap_trk_par;
    integrator.finish( extrap_trk_par );
    trk_ptr->get()->set_trk_par_vtx( extrap_trk_par );

    // store downstream momentum and error
    if( _mod_par->get_evaluation_mode() & mMutKalFitPar::VERTEX_EXTRAPOLATION )
    {

      _p_up[0] = extrap_trk_par.get_px();
      _p_up[1] = extrap_trk_par.get_py();
      _p_up[2] = extrap_trk_par.get_pz();

      _p_tot_up = sqrt(
        MUTOO::SQUARE( _p_up[0] ) +
        MUTOO::SQUARE( _p_up[1] ) +
        MUTOO::SQUARE( _p_up[2] ) );

      const PHGslMatrix& error_square_kf = integrator.get_covar_kf();
      for( unsigned int i=0; i<5; i++ )
      { _error_up[i] = sqrt( error_square_kf(i,i) ); }

    }

    // debug output
    if( _mod_par->get_verbosity() >= MUTOO::MAX )
    {
      MUTOO::PRINT( cout, "mMutKalFit::extrapolate_to_vertex - trk_par_vtx");
      extrap_trk_par.print();
    }

  }

  return true;

}

//_____________________________________________________________________________
bool mMutKalFit::extrapolate_to_muid( TMutTrkMap::pointer trk_ptr )
{

  // extrapolate track parameters from last gap to z of first MuID gap
  double z_ref( 0 );
  TMuiRoadMapO::const_key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
  if( !road_iter.count() ) {

    // if no road is found get z from plane0 panel0 center.

    // retrieve panel 0 from plane 0 and track arm.
    TMuiPanelGeo* panel( TMuiGeometry::Geom()->getPanel( trk_ptr->get()->get_arm(), 0, 0 ) );

    // retrieve center position assign z to z_ref
    float x_panel( 0 ), y_panel( 0 ), z_panel( 0 );
    panel->CenterPos( x_panel, y_panel, z_panel );
    z_ref = z_panel;

    // some dump
    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    cout << "mMutKalFit::extrapolate_to_muid - track has no associated road. Using z=" << z_panel << " (panel 0, plane 0)" << endl;

  } else {

    // get the z from first road gap0 point
    z_ref = road_iter->get()->get_gap0_point().getZ();

  }

  PHTrackIntegratorKF integrator;
  integrator.initialize( trk_ptr->get()->get_trk_par_list()->back() );
  integrator.extrapolate( z_ref );
  if( integrator.get_error() ) {

    if( _mod_par->get_verbosity() >= MUTOO::SOME ) {
      cout
        << "in mMutKalFit::extrapolate_to_muid - extrapolation towards MuID failed ("
        << trk_ptr->get()->get_trk_par_list()->back().get_z() << "->" << z_ref << ")\n";
    }
    return 0;

  } else {

    TMutTrkPar extrap_trk_par;
    integrator.finish( extrap_trk_par );
    trk_ptr->get()->push_trk_par( extrap_trk_par );
    if( _mod_par->get_verbosity() >= MUTOO::MAX ) {
      MUTOO::PRINT( cout, "mMutKalFit::extrapolate_to_muid - trk_par_muid");
      extrap_trk_par.print();
    }

  }

  return true;

}

//___________________________________________
void mMutKalFit::fill_evaluation_tree( void )
{

  // residual tree
  if( _residual_tree )
  {
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
    if( node->has_coord() ) {

      // stores coord location
      _arm = node->get_coord().get()->get_arm();
      _station = node->get_coord().get()->get_station();
      _gap = node->get_coord().get()->get_gap();
      _cathode = node->get_coord().get()->get_cathode();

      // initialise residuals and errors
      _res.assign(0);
      _res_cov.assign(0);
      _chi_square = 0;

      // fill filtered residuals
      if( node->prediction_done() ) {
        _res[0] = node->get_predicted_residual()(0,0);
        _res_cov[0] = node->get_predicted_residual_cov()(0,0);
      }

      // fill filtered residuals
      if( node->filter_done() ) {
        _res[1] = node->get_filtered_residual()(0,0);
        _res_cov[1] = node->get_filtered_residual_cov()(0,0);
        _chi_square = node->get_chi_square();
      }

      // fill filtered residuals
      if( node->smooth_done() ) {
        _res[2] = node->get_smoothed_residual()(0,0);
        _res_cov[2] = node->get_smoothed_residual_cov()(0,0);
      }

      // fill tree
      _residual_tree->Fill();
    }
  }

  // vertex extrapolation tree
  if( _vertex_extrapolation_tree ) _vertex_extrapolation_tree->Fill();

}

//__________________________________________________________
void mMutKalFit::push_r_residual( TMutTrkMap::pointer trk_ptr, const TMutTrkPar &trk_par, TMutCoordMap::value_type coord )
{

  // stores smoothed track point
  PHPoint trk_point( trk_par.get_x(), trk_par.get_y(), trk_par.get_z() );

  // loop over associated TMutGapCoord and fill r residuals.  The check on
  // cathode is to prevent writing 2 r residuals for each gap coord.  If
  // no gap coord then we don't write a r residual (SK 6/17/03)
  if(coord.get()->get_cathode() == 0) {
    TMutGapCoordMap::key_iterator gap_coord_iter = coord.get()->get_associated<TMutGapCoord>();
    while(TMutGapCoordMap::pointer gap_ptr = gap_coord_iter.next()){
      double delta_r = TMutMeasureModel::anode_measure(trk_point, gap_ptr->get());
      trk_ptr->get()->push_r_residual(delta_r);
    }
  }

  return;
}

//__________________________________________________________
double mMutKalFit::push_w_residual( TMutTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par, TMutCoordMap::value_type coord )
{

  // stores smoothed track point
  PHPoint trk_point( trk_par.get_x(), trk_par.get_y(), trk_par.get_z() );

  // stores smoothed track point errors
  PHPoint trk_errors( trk_par.get_covar( 0, 0 ), trk_par.get_covar( 1, 1 ), 0 );

  // stores smoothed track tangents
  PHVector trk_tangent(
    trk_par.get_px()/trk_par.get_pz(),
    trk_par.get_py()/trk_par.get_pz(),
    1 );

  // stores track fit parameters
  TMutFitPar fit_par(
    trk_par.get_x(), trk_par.get_y(),trk_par.get_z(), // position
    trk_par.get_px()/trk_par.get_pz(), // dx/dz
    trk_par.get_py()/trk_par.get_pz(), // dy/dz
    trk_par.get_chi_square() );

  // calculate and store w_residual
  MUTOO::cathode_locator location = coord.get()->get_location();
  double r_trk = TMutGeo::get_point_cathode_rdist(trk_point, location, coord.get()->get_peak_strip());
  double cos_th_wz = PHGeometry::dot(TMutGeo::get_w_axis(location, coord.get()->get_peak_strip()),trk_tangent);
  double cos_th_r = PHGeometry::dot(TMutGeo::get_strip_direction(location, coord.get()->get_peak_strip()),trk_tangent);
  double w_fit_error = TMutMeasureModel::get_w_error(trk_point, trk_errors, coord.get());
  double delta_w = TMutMeasureModel::cathode_measure( trk_point, coord.get(), _mod_par->get_use_anodes() );
  double w_meas = coord.get()->get_w();            // measurement from mutoo
  double w_trk = coord.get()->get_w() + delta_w;  // 'corrected' track position offset

  // calculate cluster width
  TMutClusMap::const_key_iterator clus_iter = coord.get()->get_associated<TMutClus>();
  unsigned short clus_wid = 0;
  if(!clus_iter.at_end()) clus_wid = clus_iter->get()->get_n_strip();

  trk_ptr->get()->push_w_residual(TMutTrkRes(location,
    fit_par,
    coord.get()->get_cos_theta_wire(),
    w_trk,
    w_meas,
    r_trk,
    cos_th_wz,
    cos_th_r,
    coord.get()->get_q_peak(),
    coord.get()->get_q_tot(),
    w_fit_error,
    clus_wid,
    coord.get()->get_peak_strip(),
    coord.get()->get_status())
  );

  return MUTOO::SQUARE((w_trk-w_meas)/w_fit_error);

}

//__________________________________________________________
double mMutKalFit::get_r_error()
{
  static double err = 0.5/sqrt(12.0);
  return err;
}

//__________________________________________________________
void mMutKalFit::associate_road( TMutTrkMap::pointer trk_ptr ) const
{
  // get associated roads
  TMuiRoadMapO::key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) cout << "mMutKalFit::associate_road - n_roads=" << road_iter.count() << endl;
  if( !road_iter.count() ) return;

  // get track parameters at station 3
  const TMutTrkPar* trk_par( trk_ptr->get()->get_trk_par_station( MUTOO::Station3 ) );

  // loop over associated roads
  double dg0_min( -1 );
  TMuiRoadMapO::pointer road_ptr_min( 0 );
  while( TMuiRoadMapO::pointer road_ptr = road_iter.next() )
  {
    PHPoint mui_point = road_ptr->get()->get_gap0_point();

    // get track point from last track parameters extrapolated to the muid z
    PHPoint trk_point( TMutTrackUtil::linear_track_model(
      trk_par,
      mui_point.getZ() ) );

    // calculate dg00
    double dg0 = PHGeometry::distancePointToPoint(mui_point,trk_point);
    if( dg0_min < 0 || dg0 < dg0_min ) {

      if( road_ptr_min )
      PHKey::disassociate( trk_ptr, road_ptr_min );

      road_ptr_min = road_ptr;
      dg0_min = dg0;

    } else PHKey::disassociate( trk_ptr, road_ptr );

  }

  // check min_road (useless)
  if( !road_ptr_min ) return;

  // get associated TMuiClusterO
  TMuiClusterMapO::key_iterator clus_iter( road_ptr_min->get()->get_associated< TMuiClusterO >() );
  while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
  PHKey::associate( trk_ptr, clus_ptr );
  return;

}

//______________________________________________________________________
mMutKalFit::KalmanFilterNode::KalmanFilterNode( const TMutCoordMap::pointer& coord_ptr )
{

  set_z( coord_ptr->get()->get_mean_z() );
  set_arm( coord_ptr->get()->get_arm() );
  _has_coord = true;
  _has_mui_cluster = false;
  _coord = *coord_ptr;

  // measurement
  PHGslMatrix measurement( 1,1 );
  measurement( 0,0 ) = coord_ptr->get()->get_w_absolute();

  // error on measurement
  PHGslMatrix measurement_cov( 1,1 );
  measurement_cov( 0,0 ) = MUTOO::SQUARE( coord_ptr->get()->get_error());

  // projection matrix
  PHGslMatrix h( 1, 5 );

  // store projection matrix from state vector to measurement
  double angle( TMutGeo::get_cathode_angle(
    coord_ptr->get()->get_arm(),
    coord_ptr->get()->get_station(),
    coord_ptr->get()->get_octant(),
    coord_ptr->get()->get_half_octant(),
    coord_ptr->get()->get_gap(),
    coord_ptr->get()->get_cathode(),
    coord_ptr->get()->get_peak_strip()) );

  h( 0, 3 ) = -sin( angle );
  h( 0, 4 ) = cos( angle );

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );
}

//__________________________________________________________
mMutKalFit::KalmanFilterNode::KalmanFilterNode( const TMuiClusterMapO::pointer& cluster_ptr )
{
  set_arm( cluster_ptr->get()->get_arm() );

  // stores mean z
  set_z ( 0.5*(
    cluster_ptr->get()->get_coord_begin().getZ() +
    cluster_ptr->get()->get_coord_end().getZ() ) );

  _has_coord = false;
  _has_mui_cluster = true;
  _mui_cluster = *cluster_ptr;

  // origin to first point of strip
  PHVector begin = PHVector( cluster_ptr->get()->get_coord_begin());

  // cathodes strip direction vector
  PHVector v = cluster_ptr->get()->get_coord().getDirection();
  v.normalize();

  // Base vector normal to V
  PHVector w = PHVector(v.getY(), -v.getX(), v.getZ());

  // stores measurement
  PHGslMatrix measurement( 1,1 );
  measurement( 0,0 ) = begin.dot(w);

  // stores measurement covariance
  unsigned short orientation( cluster_ptr->get()->get_orientation() );
  PHGslMatrix measurement_cov( 1,1 );
  measurement_cov( 0,0 ) = MUTOO::SQUARE(
    ( orientation == kHORIZ ) ?
    cluster_ptr->get()->get_centroidsigma().getY():
    cluster_ptr->get()->get_centroidsigma().getX()
  );

  // stores projection matrix from state vector to measurement
  PHGslMatrix h( 1, 5 );
  h( 0, 3 ) = w.getX(); // -sin(angle)
  h( 0, 4 ) = w.getY(); // +cos(angle)

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}
