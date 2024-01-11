// $Id: mFvtxKalFit.cxx,v 1.47 2017/07/13 19:12:16 phnxbld Exp $

/*!
  \file   mFvtxKalFit.cxx
  \brief   kalman filter for FVTXOO tracks.
  \author Melynda Brooks
  \version $Revision: 1.47 $
  \date   $Date: 2017/07/13 19:12:16 $
*/

// IOC
#include<TMutTrkMap.h>

// FVTXOO
#include<FVTXOO.h>
#include<TFvtxGlobalParCntrl.h>
#include<mFvtxKalFit.h>
#include<mFvtxKalFitPar.h>
#include<PHException.h>
#include<PHGeometry.h>
#include<PHTFileServer.h>
#include<PHTimer.h>
#include<PHTrackIntegratorKF.h>
#include<TMutClusMap.h>
#include<TMutCoordMap.h>
#include<TMutDatabaseInit.h>
#include<TMutGeo.h>
#include<TMutKalmanUtil.h>
#include<TMutMeasureModel.h>
#include<TMutNode.h>
#include<TMutTrackUtil.h>
#include<TMutTrkPar.hh>
#include<TFvtxPisaHitMap.h>
#include<TMutExtVtx.h>
#include<TFvtxCompactTrk_v3.h>

// GLOBAL
#include<PHLine.h>
#include<PHPoint.h>
#include<PHVector.h>

#include <gsl/gsl_randist.h>
#include <gsl/gsl_math.h>

// MUIGEOM
#include<MuiGeomClasses.hh>

// MUTGEOM
#include <MutWire.h>

#include <MuonUtil.h>

// STL/BOOST
#include<iostream>
#include<string>
#include<list>

using namespace std;

//______________________________________________________________________
mFvtxKalFit::mFvtxKalFit() :
  _use_svx_cluster(true),
  _timer(PHTimeServer::get()->insert_new("mFvtxKalFit") ),
  _top_node( 0 ),
  _mod_par( 0 ),
  _trk_map( 0 ),
  _filename( "mFvtxKalFit.root" ),
  _tree( 0 ),
  _tree2( 0 ),
  _arm( 0 ),
  _cage( 0 ),
  _station( 0 ),
  _sector( 0 ),
  _chi_square( 0 )
{
  _res.assign( 0 );
  _res_cov.assign( 0 );
  FVTXOO::TRACE("initializing module mFvtxKalFit");
  integrator = new PHTrackIntegratorKF();
}

//______________________________________________________________________
// Destructor
mFvtxKalFit::~mFvtxKalFit()
{
  finish_evaluation();
  _nodes.clear();
  if(integrator) delete integrator;
}

//______________________________________________________________________
void mFvtxKalFit::init_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };

  // check if file exist
  if( _tree ) return;

  _filename = _mod_par->get_evaluation_file();
  PHTFileServer::get().open( _filename, "RECREATE" );
  _tree = new TTree( "residuals", "single hit residuals" );
  _tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE );
  _tree->Branch( "cage", &_cage, "cage/I", BUFFER_SIZE );
  _tree->Branch( "station", &_station, "station/I", BUFFER_SIZE );
  _tree->Branch( "sector", &_sector, "sector/I", BUFFER_SIZE );
  _tree->Branch( "res", &_res[0], "res[3]/D", BUFFER_SIZE );
  _tree->Branch( "res_cov", &_res_cov[0], "res_cov[3]/D", BUFFER_SIZE );
  _tree->Branch( "chi_square", &_chi_square, "chi_square/D",  BUFFER_SIZE );
  _tree->Branch( "meas", &_meas, "meas/D",        BUFFER_SIZE );
  _tree->Branch( "znode", &_znode, "znode/D",     BUFFER_SIZE );
  _tree->Branch( "nhits", &_nhits, "nhits/I",     BUFFER_SIZE );
  _tree->Branch( "nfvtx", &_nfvtx, "nfvtx/I",     BUFFER_SIZE );
  _tree->Branch( "xmeas", &_xmeas, "xmeas/D",        BUFFER_SIZE );
  _tree->Branch( "ymeas", &_ymeas, "ymeas/D",        BUFFER_SIZE );
  _tree->Branch( "phimeas", &_phimeas, "phimeas/D",        BUFFER_SIZE );
  _tree->Branch( "xfit", &_xfit, "xfit/D",        BUFFER_SIZE );
  _tree->Branch( "yfit", &_yfit, "yfit/D",        BUFFER_SIZE );
  _tree->Branch( "inode", &_inode, "inode/I",     BUFFER_SIZE );
  _tree->Branch( "px", &_px, "px/D",        BUFFER_SIZE );
  _tree->Branch( "py", &_py, "py/D",        BUFFER_SIZE );
  _tree->Branch( "pz", &_pz, "pz/D",        BUFFER_SIZE );
  _tree->Branch( "fplanes", &_fplanes, "fplanes/I",     BUFFER_SIZE );
  _tree->SetAutoSave( AUTO_SAVE );

  // check if tree exist
  if( _tree2 ) return;

  _tree2 = new TTree( "vtx_residuals", "vtx residuals" );
  _tree2->Branch( "x", &_x, "x/D", BUFFER_SIZE );
  _tree2->Branch( "y", &_y, "y/D", BUFFER_SIZE );
  _tree2->Branch( "z", &_z, "z/D", BUFFER_SIZE );
  _tree2->Branch( "xerror", &_xerror, "xerror/D", BUFFER_SIZE );
  _tree2->Branch( "yerror", &_yerror, "yerror/D", BUFFER_SIZE );
  _tree2->SetAutoSave( AUTO_SAVE );

}

//______________________________________________________________________
void mFvtxKalFit::finish_evaluation( void )
{

  if( !_tree ) return;

  FVTXOO::TRACE("mFvtxKalFit::finish_evaluation");
  PHTFileServer::get().write( _filename );
  return;
}

//______________________________________________________________________
// Event method.
PHBoolean mFvtxKalFit::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();

  try {

    static bool init_done;

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // initialize tree if requested
    if( _mod_par->get_do_evaluation() && !init_done ) init_tree();

    init_done = true;

    // loop over tracks
    track_loop();

  } catch(exception& e) {
    FVTXOO::TRACE(e.what());
    return False;
  }
  
  _timer.get()->stop();
  
  //_trk_map->print();
  
  if(_mod_par->get_verbosity() >= FVTXOO::ALOT) _trk_map->print();
  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();
  
  return True;
}

void
mFvtxKalFit::init_run(PHCompositeNode* topNode)
{
  // Good place for initialization of the B-Field and Geant materials
  //
  TFvtxGlobalParCntrl::init_run();


  MuonUtil::initialize_magnetic_field(topNode);
  MuonUtil::initialize_geant(topNode);

  return;
}

void
mFvtxKalFit::end(PHCompositeNode* top_node)
{
  _timer.get()->print_stat();

  return;
}

//______________________________________________________________________
void mFvtxKalFit::set_interface_ptrs(PHCompositeNode* top_node)
{
  _top_node = top_node;
  _mod_par = TMutNode<mFvtxKalFitPar>::find_node(top_node,"mFvtxKalFitPar");
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");
  _ctrk_map = TMutNode<TFvtxCompactTrkMap>::find_node(top_node,"TFvtxCompactTrkMap"); 
  _coord_map = TMutNode<TFvtxCoordMap>::find_node(top_node,"TFvtxCoordMap");
  _ccoord_map = TMutNode<TFvtxCompactCoordMap>::find_node(top_node,"TFvtxCompactCoordMap"); 
}

//______________________________________________________________________
void mFvtxKalFit::track_loop()
{
  // Loop over all track in the track map
  TFvtxTrkMap::iterator trk_iter = _trk_map->range();
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    try {
      
      
      if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
        cout << "mFvtxKalFit::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;


      // perform basic checks on the track
      if( !accept_track( trk_ptr ) ) {
        trk_ptr->get()->set_kalman_fail();
        continue;
      }


      TMutKalmanFilter kalman_filter;
      kalman_filter.set_verbosity( (MUTOO::Verbosity) _mod_par->get_verbosity() );


      // initialize starting parameters and nodes
      init( trk_ptr, kalman_filter );


      // perform full fit
      if( !fit_nodes( kalman_filter ) ) {
        if( _mod_par->get_verbosity() >= FVTXOO::SOME )
          cout << "mFvtxKalFit::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " fit failed." << endl;
        trk_ptr->get()->set_kalman_fail();
        continue;
      }


      // fill the track with node informations
      fill_track( trk_ptr );


      // extrapolate track parameters toward vertex
      extrapolate_to_vertex( trk_ptr );

      // fill evaluation tree if requested
      if( _mod_par->get_do_evaluation() ) fill_tree( trk_ptr );

      // set track status
      if( !trk_ptr->get()->get_kalman_fail() ) trk_ptr->get()->set_kalman_fit();

    } catch( exception &e ) { cout << "mFvtxKalFit::track_loop - " << e.what() << endl; }

}

//______________________________________________________________________
bool mFvtxKalFit::accept_track( TFvtxTrkMap::const_pointer trk_ptr ) const
{

  int ncoords = 0;
  int nsvxhits = 0;

  // check ghost bit
  if(trk_ptr->get()->get_ghost()) return false;

  // check number of hits in cathode
  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
  int foundst[4] = {0};
  if (coord_iter.count()){
    ncoords = coord_iter.count();
    while ( TFvtxCoordMap::const_pointer trk_coord_ptr = coord_iter.next() ) {
      foundst[trk_coord_ptr->get()->get_station()] = 1;
    }
  }
  int nstations = 0;
  for (int i=0; i<4; i++) nstations += foundst[i];

  bool enough_hits = true;
  if ( !_use_svx_cluster )
    {
      TFvtxPisaHitMap::key_iterator key_iter( trk_ptr->get()->get_associated<TFvtxPisaHit>() );
      if (key_iter.count()) nsvxhits = key_iter.count();
      if ( ncoords + nsvxhits < _mod_par->get_min_n_coord() || nstations < _mod_par->get_min_n_coord() )
        enough_hits = false;
    }
  else
    {
      TFvtxSvxClusterMap::key_iterator key_iter( trk_ptr->get()->get_associated<TFvtxSvxCluster>() );
      int foundlay[4] = {0};
      if (key_iter.count()) {
        nsvxhits = key_iter.count();
        while ( TFvtxSvxClusterMap::const_pointer trk_svx_ptr = key_iter.next() ) {
          foundlay[trk_svx_ptr->get()->get_cluster()->get_layer()] = 1;
        }
      }
      int nlayers = 0;
      for (int i=0; i<4; i++) nlayers += foundlay[i];
      if ( ncoords + nsvxhits < _mod_par->get_min_n_coord() || ( nstations + nlayers < _mod_par->get_min_n_coord() ) )// && nstations < 2 ) )
        enough_hits = false;
    }

  if ( !enough_hits )
    {
      trk_ptr->get()->set_reco_min_hits();
      trk_ptr->get()->set_no_fit();

      if( _mod_par->get_verbosity()>= FVTXOO::SOME )
        cout << "mFvtxKalFit::accept_track - trk="
             << trk_ptr->get()->get_key().get_obj_key()
             << " not enough hits (" << coord_iter.count() << ").\n";

      return false;
    }

  return true;

}

//______________________________________________________________________
void mFvtxKalFit::init(
                       TFvtxTrkMap::const_pointer trk_ptr,
                       TMutKalmanFilter& kalman_filter
                       )

{
  // make sure list of nodes is empty
  _nodes.clear();

  // stores reference to starting parameters localy and initialize
  TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );

  const TMutTrkPar trk_par( *trk_ptr->get()->get_trk_par() );

  // Try to load vertex from TMutExtVtx first.  If error,
  // Take z vertex position from init values
  bool error( false );

  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if (error) {
    if( _mod_par->get_verbosity() > FVTXOO::NONE ){
      cout << "Error in TMutExtVtx, taking track vertex point" << endl;
      cout << "z_vtx = " << trk_par.get_z()<< endl;
    }
    _mod_par->set_z_reference(trk_par.get_z());
  }
  else{
    if( _mod_par->get_verbosity() > FVTXOO::NONE ){
      cout << "Using TMutExtVtx " << vtx.getZ()<< endl;
      cout << "z_vtx = " << vtx.getZ()<< endl;
    }
    _mod_par->set_z_reference( vtx.getZ() );
  }

  start_kf._direction = ( trk_ptr->get()->get_arm() == FVTXOO::South ) ? -1:1;

  // optionally add vtx hits to fit:
  float zsmear, phismear;

  //if(_mod_par->get_use_vtx())
  if ( TFvtxGlobalParCntrl::get_bool_par("use_svx") )
    {

      //switch on use PISA or real VTX hits
      if (!_use_svx_cluster)
        {
          TFvtxPisaHitMap::key_iterator key_iter(
              trk_ptr->get()->get_associated<TFvtxPisaHit>());
          while (TFvtxPisaHitMap::const_pointer ptr = key_iter.next())
            {

              //            zsmear = gsl_ran_gaussian(_rng.get(), _mod_par->get_vtx_z_smear());
              zsmear = (gsl_rng_uniform(_rng.get()) - 0.5)
                  * _mod_par->get_vtx_z_smear();
              //            phismear = gsl_ran_gaussian(_rng.get(), _mod_par->get_vtx_phi_smear());
              phismear = (gsl_rng_uniform(_rng.get()) - 0.5)
                  * _mod_par->get_vtx_phi_smear();

              int xyflag = 0;
              KalmanFilterNode nodex(*ptr->get()->get_pisa_hit(), xyflag,
                  zsmear, phismear);
              _nodes.push_back(nodex);

              xyflag = 1;
              KalmanFilterNode nodey(*ptr->get()->get_pisa_hit(), xyflag,
                  zsmear, phismear);
              _nodes.push_back(nodey);

            }
        }
      else
        {
          TFvtxSvxClusterMap::key_iterator key_iter(
              trk_ptr->get()->get_associated<TFvtxSvxCluster>());
          while (TFvtxSvxClusterMap::const_pointer ptr = key_iter.next())
            {
              /*
               int xyflag = 0;
               KalmanFilterNode nodex( *ptr->get()->get_cluster(), xyflag, _mod_par->get_vtx_phi_error(), _mod_par->get_vtx_r_error(), _mod_par->get_vtx_z_error(), trk_par.get_dxdz(), trk_par.get_dydz());
               _nodes.push_back( nodex );

               xyflag = 1;
               KalmanFilterNode nodey( *ptr->get()->get_cluster(), xyflag, _mod_par->get_vtx_phi_error(), _mod_par->get_vtx_r_error(), _mod_par->get_vtx_z_error(), trk_par.get_dxdz(), trk_par.get_dydz());
               _nodes.push_back( nodey );
               */

              int phiflag = 0;
              double drdz = sqrt(
                  trk_par.get_dxdz() * trk_par.get_dxdz()
                      + trk_par.get_dydz() * trk_par.get_dydz());
              KalmanFilterNode nodex(*ptr->get()->get_cluster(), phiflag,
                  _mod_par->get_vtx_phi_error(), _mod_par->get_vtx_r_error(),
                  _mod_par->get_vtx_z_error(), drdz);
              _nodes.push_back(nodex);

              phiflag = 1;
              KalmanFilterNode nodey(*ptr->get()->get_cluster(), phiflag,
                  _mod_par->get_vtx_phi_error(), _mod_par->get_vtx_r_error(),
                  _mod_par->get_vtx_z_error(), drdz);
              _nodes.push_back(nodey);

              if (_mod_par->get_do_evaluation() && _tree2)
                {
                  _x = nodex.get_measurement()(0, 0);
                  _xerror = nodex.get_measurement_cov()(0, 0);
                  _y = nodey.get_measurement()(0, 0);
                  _yerror = nodey.get_measurement_cov()(0, 0);
                  _z = nodex.get_z();
                  _tree2->Fill();
                }
            }

        }
    }


  /*
    adds coordinate associated to the track to the kalman filter object
  */

  // retrieve coordinates associated to the track
  list<TFvtxCoordMap::value_type> coord_list;
  typedef list< TFvtxCoordMap::value_type >::iterator coord_iterator;

  TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();

  // Check whether there are any FVTX coords as it is possible to have just barrel hits:
  if (coord_iter.count() > 0 ){
    while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()) coord_list.push_back( *coord_ptr );

    // sort the list of coordinates from vertex to muid
    coord_list.sort( coord_less_ftor() );

    // add measurements
    for( coord_iterator coord = coord_list.begin(); coord != coord_list.end();  coord++ )
        {

        // create new node
        KalmanFilterNode noder( &(*coord), 0 );

        // add node
        _nodes.push_back( noder );

        // create new node
        KalmanFilterNode nodephi( &(*coord), 1 );

        // add node
        _nodes.push_back( nodephi );

      }

  }  // If any FVTX Coords

  // sort nodes from vertex to muid
  _nodes.sort();

  if (coord_iter.count() > 0 ){
    // set z from last coordinate
    start_kf._z = coord_list.back().get()->get_mean_z();
  }
  // If no FVTX coords, need to select starting point from barrel hits:
  else{
    start_kf._z = _nodes.back().get_z();
  }

  // extrapolate track parameters to _z
  TMutTrkPar trk_par_extrap(
                            trk_par.get_x() + trk_par.get_dxdz()*(start_kf._z - trk_par.get_z()),
                            trk_par.get_y() + trk_par.get_dydz()*(start_kf._z - trk_par.get_z()),
                            start_kf._z,
                            trk_par.get_px(),
                            trk_par.get_py(),
                            trk_par.get_pz(),
                            trk_par.get_charge() );

  start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par_extrap );

  // in both cases, use the default covariance matrix for error
  start_kf._covar_kf(0, 0) = FVTXOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) ); // error on c/p
  start_kf._covar_kf(1, 1) = FVTXOO::SQUARE( _mod_par->get_angular_resolution() );  // error on dx/dz
  start_kf._covar_kf(2, 2) = FVTXOO::SQUARE( _mod_par->get_angular_resolution() );  // error on dy/dz
  start_kf._covar_kf(3, 3) = FVTXOO::SQUARE( _mod_par->get_position_resolution() ); // error on x
  start_kf._covar_kf(4, 4) = FVTXOO::SQUARE( _mod_par->get_position_resolution() ); // error on y

  // dump all nodes
  if( _mod_par->get_verbosity() >= FVTXOO::MAX )
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) {
      FVTXOO::PRINT( cout, "mFvtxKalFit::init_nodes - node");
      cout << "measurement" << node->get_measurement();
      cout << "covariance" << node->get_measurement_cov();
      cout << "h" << node->get_h();
    }

  if( _mod_par->get_verbosity() >= FVTXOO::SOME ) {
    cout << "mFvtxKalFit::init - trk="
         << trk_ptr->get()->get_key().get_obj_key()
         << " start parameters"
         << endl;
    start_kf.print();
  }

}

//___________________________________________________________________
bool mFvtxKalFit::fit_nodes( TMutKalmanFilter &kalman_filter )
{

  // loop over max number of iterations
  unsigned int iteration( 0 );
  double old_chi_square = 1e6;
  for( ; iteration < _mod_par->get_max_iterations(); iteration++ ) {

    double chi_square( 0 );

    // reset node status
    reset_nodes();

    // perform prediction and filter using reverse iterator
    for( reverse_node_iterator node = _nodes.rbegin(); node != _nodes.rend(); node ++ ){

      //if( kalman_filter.predict( *node ) && kalman_filter.filter( *node ) ) {
      bool pred = kalman_filter.predict( *node );
      bool filt = kalman_filter.filter( *node );
      if( pred && filt ) {
        chi_square += node->get_chi_square();
      }
      else {
        if( _mod_par->get_verbosity()>= FVTXOO::SOME )
          cout << "mFvtxKalFit::fit_nodes - prediction/filter failed.\n";
        return false;
      }
    }

    // initialize smoothing starting from first node (closest to vertex)
    bool first_node( true );
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node ++ ){
      if( first_node ) {
        first_node = false;
        kalman_filter.initialize_smoother( *node );
      } else if( !kalman_filter.smooth( *node ) ) {
        cout << "mFvtxKalFit::fit_nodes - prediction/filter failed.\n";
        return false;
      }
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
        since the meaningfull error given as argument is dp_tot/p_tot
        whereas the error used in the matrix is c/p_tot
      */
      start_kf._covar_kf(0, 0) = FVTXOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) ); // error on c/p

    }

    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      FVTXOO::TRACE( "mFvtxKalFit::fit_nodes - start parameters [updated]");
      start_kf.print();
    }

    // dump chisquare
    if( _mod_par->get_verbosity()>=FVTXOO::SOME )
      cout << "mFvtxKalFit::fit_nodes - iteration " << iteration << ", chi2=" << chi_square << endl;

    if( iteration && fabs( chi_square - old_chi_square ) < _mod_par->get_chi_cut()*old_chi_square ) break;
    old_chi_square = chi_square;

  }

  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    cout << "mFvtxKalFit::fit_nodes - " << iteration << " iterations.\n";

  return true;
}

//___________________________________________
bool mFvtxKalFit::fill_track( TFvtxTrkMap::pointer trk_ptr )
{

  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
    cout << "mFvtxKalFit::fill_track - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;

  // initialize track lists
  trk_ptr->get()->clear_trk_par_list(); // clear list of trak parameters
  trk_ptr->get()->clear_w_residual_list(); // clear list of w residuals
  trk_ptr->get()->clear_r_residual_list(); // clear list of w residuals

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
                       ( node->get_smoothed()._state_kf( 0,0 ) < 0 ) ? -1:1,
                       node->get_chi_square() );

    PHGslMatrix cov_mutoo( TMutKalmanUtil::cov_kalman_to_mutoo(
                                                               trk_par.get_charge(),
                                                               node->get_smoothed()._covar_kf,
                                                               mom_mutoo ) );

    for( unsigned int i=0; i<TMutTrkPar::COVAR_ROW; i++ )
      for( unsigned int j=0; j<TMutTrkPar::COVAR_ROW; j++ )
        trk_par.set_covar( i, j, cov_mutoo(i,j) );

    trk_ptr->get()->push_trk_par( trk_par );

    // store chi_square contribution into coord
    if ( node->get_fvtx_coord()){
      node->get_fvtx_coord().get()->push_chi_sqr_inc(
                                                     trk_ptr->get()->get_key().get_obj_key(),
                                                     node->get_chi_square() );

      // push residuals
      push_r_residual( trk_ptr, trk_par, node->get_fvtx_coord() );
      push_w_residual( trk_ptr, trk_par, node->get_fvtx_coord() );
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
    TFvtxTrk::get_hit_pattern calculates the hit pattern from coord to track associations
    starting from TFvtxTrk_v4, this hit pattern is stored internaly and not recalculated
  */
  //  trk_ptr->get()->set_hit_pattern( trk_ptr->get()->TFvtxTrk::get_hit_pattern() );

  // set track parameters in first gap
  trk_ptr->get()->set_trk_par(  trk_ptr->get()->get_trk_par_list()->front() );

  return true;
}

//_____________________________________________________________________________
bool mFvtxKalFit::extrapolate_to_vertex( TFvtxTrkMap::pointer trk_ptr )
{
  integrator->clear();
  // extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  double z_ref = _mod_par->get_z_reference();

  if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
    FVTXOO::PRINT( cout, "mFvtxKalFit::extrapolate_to_vertex - before extrapolation");
    TMutKalmanUtil::print_trk_par_kf( trk_ptr->get()->get_trk_par_list()->front() );
  }


  integrator->initialize( trk_ptr->get()->get_trk_par_list()->front() );
  integrator->extrapolate( z_ref );


  if( integrator->get_error() ) {
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      cout
        << "in mFvtxKalFit::extrapolate_to_vertex - extrapolation towards vertex failed ("
        << trk_ptr->get()->get_trk_par_list()->front().get_z() << "->" << z_ref << ")\n";
    }

    trk_ptr->get()->set_kalman_fail();
    return false;

  } else {
    TMutTrkPar extrap_trk_par;
    integrator->finish( extrap_trk_par );
    trk_ptr->get()->set_trk_par_vtx( extrap_trk_par );
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      FVTXOO::PRINT( cout, "mFvtxKalFit::extrapolate_to_vertex - trk_par_vtx");
      extrap_trk_par.print();
    }
  }


  return true;

}

//_____________________________________________________________________________
bool mFvtxKalFit::extrapolate_to_mutr( TFvtxTrkMap::pointer trk_ptr )
{
  integrator->clear();

  // extrapolate track parameters from last gap to z of first Mutr gap
  double z_ref( 0 );
  TMutTrkMap::const_key_iterator mutr_iter = trk_ptr->get()->get_associated<TMutTrk>();
  if( !mutr_iter.count() ) {

    // if no road is found get z from plane0 panel0 center.

    // retrieve panel 0 from plane 0 and track arm.

    // retrieve center position assign z to z_ref
    // z_ref = z_panel;

    // some dump
    //    if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
    //    cout << "mFvtxKalFit::extrapolate_to_muid - track has no associated road. Using z=" << z_panel << " (panel 0, plane 0)" << endl;

  } else {

    // get the z from first road gap0 point
    // z_ref = road_iter->get()->get_gap0_point().getZ();

  }


  integrator->initialize( trk_ptr->get()->get_trk_par_list()->back() );
  integrator->extrapolate( z_ref );
  if( integrator->get_error() ) {

    if( _mod_par->get_verbosity() >= FVTXOO::SOME ) {
      cout
        << "in mFvtxKalFit::extrapolate_to_muid - extrapolation towards MuID failed ("
        << trk_ptr->get()->get_trk_par_list()->back().get_z() << "->" << z_ref << ")\n";
    }
    return false;

  } else {

    TMutTrkPar extrap_trk_par;
    integrator->finish( extrap_trk_par );
    trk_ptr->get()->push_trk_par( extrap_trk_par );
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      FVTXOO::PRINT( cout, "mFvtxKalFit::extrapolate_to_mutr - trk_par_mutr");
      extrap_trk_par.print();
    }

  }


  return true;

}

//___________________________________________
void mFvtxKalFit::fill_tree( TFvtxTrkMap::pointer trk_ptr )
{
  if( !_tree ) return;

  _nfvtx = 0;

  _fplanes = 0;
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
  {
    if (node->get_fvtx_coord()){
      _nfvtx++;
      if (node->get_fvtx_coord().get()->get_station() == 0) _fplanes +=1;
      else if (node->get_fvtx_coord().get()->get_station() == 1) _fplanes +=2;
      else if (node->get_fvtx_coord().get()->get_station() == 2) _fplanes +=4;
      else if (node->get_fvtx_coord().get()->get_station() == 3) _fplanes +=8;
    }
  }

  // Store the momentum vector of the fitted track: 
  const TMutTrkPar trk_par( *trk_ptr->get()->get_trk_par() );
  _px = trk_par.get_px();
  _py = trk_par.get_py();
  _pz = trk_par.get_pz();

  _inode = 0;
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
    {
               // stores coord location
                if (node->get_fvtx_coord()){
                  _arm = node->get_fvtx_coord().get()->get_arm();
                  _cage = node->get_fvtx_coord().get()->get_cage();
                  _station = node->get_fvtx_coord().get()->get_station();
                  _sector = node->get_fvtx_coord().get()->get_sector();
                 _xmeas = -999;
                 _ymeas = -999;
                 _phimeas = -999;
                }
                else{
                  _arm = -1;
                  if (_inode%2 == 0) {
                    _xmeas = node->get_measurement()(0,0);
                    node_iterator nodenext = node;
                    nodenext++;
                    _ymeas = nodenext->get_measurement()(0,0);
                  }
                  else if (_inode%2 != 0) {
                    PHGslMatrix h( node->get_h() );
                    _phimeas = acos(h(0,4));
                  }
                }
                _znode = node->get_z();
                _meas = node->get_measurement()(0,0);
                _nhits = _nodes.size();

                _xfit = node->get_smoothed()._state_kf( 3, 0 );
                _yfit = node->get_smoothed()._state_kf( 4, 0 );

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
                _tree->Fill();

                _inode++;
    }

}

//__________________________________________________________
void mFvtxKalFit::push_r_residual( TFvtxTrkMap::pointer trk_ptr, const TMutTrkPar &trk_par, TFvtxCoordMap::value_type coord )
{

  return;
}

//__________________________________________________________
double mFvtxKalFit::push_w_residual( TFvtxTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par, TFvtxCoordMap::value_type coord )
{

  return 0.0;

}

//__________________________________________________________
mFvtxKalFit::KalmanFilterNode::KalmanFilterNode( const TFvtxCoordMap::pointer& coord_ptr, const int phiflag )
{

  // The KF angle used for the FVTX r, phi measurements is the angle that the x,y axes need to be rotated
  // until they are perpendicular to the strips (coordinate) in this wedge/column. 
  // The w_absolute is calculated  by getting the distance of closest approach of the line that represents 
  // the coordinate to the  z axis, at the location of the average z position of the wedge.
  // The phi "measurement" is the distance that the center of the strip is away from the rotated x,y axes.

  set_z( coord_ptr->get()->get_mean_z() );

  set_arm( coord_ptr->get()->get_arm() );
  _fvtx_coord = *coord_ptr;

  PHPoint begin = coord_ptr->get()->get_coord_begin();
  PHPoint end = coord_ptr->get()->get_coord_end();

  // store measurement and error
  PHGslMatrix measurement( 1,1 );
  PHGslMatrix measurement_cov( 1,1 );
  if ( !phiflag) {
    measurement( 0,0 ) = coord_ptr->get()->get_w_absolute();
    measurement_cov( 0,0 ) = FVTXOO::SQUARE( coord_ptr->get()->get_error());
  }
  else{

    PHVector v( end - begin );
    v.normalize();
    PHVector mid( (end + begin)*0.5 );
    measurement(0,0) = mid.dot( v );

    // The covariance gets the error *squared*
    double error2 =
      FVTXOO::SQUARE(end.getY() - begin.getY())
      + FVTXOO::SQUARE(end.getX() - begin.getX());

    measurement_cov( 0,0 ) = error2/12.0;

  }

  // The angle of the measured coordinate:
  double angle = atan2((end.getY() - begin.getY()), (end.getX() - begin.getX()));

  // if phi flag is set, we are to add the phi portion of the measurement to the coord
  // list (as opposed to the r portion of the measurement). Phi measurement orientation
  // is 90 degrees from r orientation.

  PHGslMatrix h( 1, 5 );

  if (phiflag){
    angle -= M_PI/2;
  }
  else{
    // Ensure that the range is -pi/2, pi/2 to make it consistent wth w_absolute calculation
    //
    angle = (angle < -M_PI_2) ? angle + M_PI : angle;
    angle = (angle > M_PI_2) ? angle - M_PI : angle;
  }

  h( 0, 3 ) = -sin( angle );
  h( 0, 4 ) = cos( angle );

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mFvtxKalFit::KalmanFilterNode::KalmanFilterNode( const SvxSnglPisaHit& svxhit, const int xyflag,
                                                 const float zsmear, const float phismear )
{

  set_z( svxhit.GetZGlobal() + zsmear  );

  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );
  _svx_mchit = svxhit;

  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );
  if (xyflag == 0 )
    {

      measurement( 0,0 ) =
        -svxhit.GetXGlobal() +
        phismear*(svxhit.GetXGlobal()/sqrt(svxhit.GetXGlobal()*svxhit.GetXGlobal() +
                                           svxhit.GetYGlobal()*svxhit.GetYGlobal()) ) ;

    } else if (xyflag == 1 )
    {

      measurement( 0,0 ) =
        svxhit.GetYGlobal() +
        phismear*(svxhit.GetXGlobal()/sqrt(svxhit.GetXGlobal()*svxhit.GetXGlobal() +
                                           svxhit.GetYGlobal()*svxhit.GetYGlobal()) );

    } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << xyflag << endl;
    return;
  }

  //  NEED TO SET THIS WITH A PARAMETER
  measurement_cov( 0,0 ) = 0.01;

  // store projection matrix
  PHGslMatrix h(1,5);
  if (xyflag == 0)
    {
      // x measurement, strip angle = 90 degrees
      h( 0, 3 ) = -1.0;  // -sin(angle)
      h( 0, 4 ) = 0.0;   //  cos(angle)
    }

  else if (xyflag == 1)
    {
      // y measurement, strip angle = 0 degrees
      h( 0, 3 ) = 0.0;
      h( 0, 4 ) = 1.0;
    }

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mFvtxKalFit::KalmanFilterNode::KalmanFilterNode( const SvxCluster& clus, const int phiflag, double phi_error, double r_error, double z_error,
  double drdz)
{

// fill the KF nodes with VTX points which are translated from x,y into r,phi space so 
// measurements are closer to the true VTX measurement directions:

  set_z( clus.get_xyz_global(2) );

  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );
  _svx_cluster = clus;

  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );

  // For calculating x, y measurement errors:

  double angle = atan2(clus.get_xyz_global(1), clus.get_xyz_global(0));

  if (phiflag == 0 )  
  {
    // extract the r measurement, which we assume has an error primarily associated with
    // the r alignment and extrapolating to a measured z value:

    angle -= M_PI_2;
    measurement( 0,0 ) = sqrt(clus.get_xyz_global(0)*clus.get_xyz_global(0) + 
      clus.get_xyz_global(1)*clus.get_xyz_global(1));

    // include phi measurement error, possible r alignment error, try to
    // include an error from z, projected into x:
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(r_error) +
                             FVTXOO::SQUARE(z_error*drdz);

  } else if (phiflag == 1 )
  {

    // In the nominal phi direction the KF angle has been rotated until it passes
    // through the VTX x,y point, giving a phi measurement of '0':
    // Assume for now that the propagation z measurement error into phi is minimal
    // and see if it works o.k. to ignore it:

    measurement( 0,0 ) = 0;
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(phi_error);

  } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << phiflag << endl;
    return;
  }

  // store projection matrix
  PHGslMatrix h(1,5);

  h( 0, 3 ) = -sin(angle);
  h( 0, 4 ) =  cos(angle);

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________
mFvtxKalFit::KalmanFilterNode::KalmanFilterNode( const SvxCluster& clus, const int xyflag, double phi_error, double r_error, double z_error, double dxdz, double dydz)
{
  set_z( clus.get_xyz_global(2) );

  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );
  _svx_cluster = clus;

  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );

  // For calculating x, y measurement errors:
  double rad = sqrt(FVTXOO::SQUARE(clus.get_xyz_global(0)) +
                    FVTXOO::SQUARE(clus.get_xyz_global(1)));
  double sinphi = clus.get_xyz_global(1)/rad;
  double cosphi = clus.get_xyz_global(0)/rad;

  if (xyflag == 0 )
  {

    measurement( 0,0 ) = -clus.get_xyz_global(0);
    // include phi measurement error, possible r alignment error, try to
    // include an error from z, projected into x:
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(phi_error*sinphi) +
                             FVTXOO::SQUARE(r_error*cosphi) +
                             FVTXOO::SQUARE(z_error*dxdz);

  } else if (xyflag == 1 )
  {

    measurement( 0,0 ) = clus.get_xyz_global(1);
    measurement_cov( 0,0 ) = FVTXOO::SQUARE(phi_error*cosphi) +
                             FVTXOO::SQUARE(r_error*sinphi) +
                             FVTXOO::SQUARE(z_error*dydz);

  } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << xyflag << endl;
    return;
  }


  // store projection matrix
  PHGslMatrix h(1,5);
  if (xyflag == 0)
  {
    // x measurement, strip angle = 90 degrees
    h( 0, 3 ) = -1.0;  // -sin(angle)
    h( 0, 4 ) = 0.0;   //  cos(angle)
  }

  else if (xyflag == 1)
  {
    // y measurement, strip angle = 0 degrees
    h( 0, 3 ) = 0.0;
    h( 0, 4 ) = 1.0;
  }

  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );

}

//__________________________________________________________

void mFvtxKalFit::fill_compact_track( )
{
  _ctrk_map->clear();
  TFvtxTrkMap::iterator trk_iter = _trk_map->range();
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next())
    {

      //ghost and bad track rejections
      if (trk_ptr->get()->get_ghost()) continue;
      if (not trk_ptr->get()->get_reco_success()) continue;

      TFvtxCoordMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
      TFvtxSvxClusterMap::key_iterator clus_iter = trk_ptr->get()->get_associated<TFvtxSvxCluster>();
      if (coord_iter.count() + clus_iter.count() < 2) continue;

      add_compact_track(trk_ptr, _ctrk_map);
    }

  // _ctrk_map->print(std::cout);
}

//__________________________________________________________

void 
mFvtxKalFit::add_compact_track(TFvtxTrkMap::pointer trk_ptr, TFvtxCompactTrkMap* ctrk_map)
{
  int iarm = trk_ptr->get()->get_arm();
  TFvtxCompactTrkMap::iterator ctrk_iter = ctrk_map->insert_new( iarm );
  TFvtxCompactTrkMap::pointer ctrk_ptr = ctrk_iter.current();
  PHKey::associate( ctrk_ptr, trk_ptr );
  
  TFvtxCompactTrk_v4* fvtx_trk = (TFvtxCompactTrk_v4*)ctrk_ptr->get();
  
  PHPoint pnt = trk_ptr->get()->get_trk_par_vtx()->get_point();
  fvtx_trk->set_track_vtx( pnt );
  
  PHVector vfvtx(trk_ptr->get()->get_trk_par_vtx()->get_px(),
		 trk_ptr->get()->get_trk_par_vtx()->get_py(),
		 trk_ptr->get()->get_trk_par_vtx()->get_pz());
  float phi = atan2(vfvtx.getY(), vfvtx.getX());
  float theta = atan2(sqrt(vfvtx.getX()*vfvtx.getX() + vfvtx.getY()*vfvtx.getY()),
		      vfvtx.getZ());
  fvtx_trk->set_fvtx_phi(phi);
  fvtx_trk->set_fvtx_theta(theta);
  fvtx_trk->set_chi2_ndf(trk_ptr->get()->get_w_chi_square()/trk_ptr->get()->get_ndf());
  unsigned char hit_pattern = (unsigned char)trk_ptr->get()->get_hit_pattern();
  fvtx_trk->set_hit_pattern(hit_pattern);
  hit_pattern = 0;
  for (int isvxlayer=0; isvxlayer<4; isvxlayer++)
    {
      if (trk_ptr->get()->has_svx_coord(isvxlayer))
	hit_pattern |= (1 << (isvxlayer));
    }
  fvtx_trk->set_svxhit_pattern(hit_pattern);
  for (size_t i=0; i<5; i++)
    for (size_t j=0; j<5; j++)
      fvtx_trk->set_cov(i,j, trk_ptr->get()->get_trk_par_vtx()->get_covar(i,j));
}

void mFvtxKalFit::fill_compact_coordinate( )
{
  _ccoord_map->clear();
  TFvtxCoordMap::iterator coord_iter = _coord_map->range();
  while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next())
    {
      add_compact_coord(coord_ptr, _ccoord_map);
    }
}

void 
mFvtxKalFit::add_compact_coord(TFvtxCoordMap::pointer coord_ptr, TFvtxCompactCoordMap* ccoord_map)
{
  int arm = coord_ptr->get()->get_arm();
  int cage = coord_ptr->get()->get_cage();
  int station = coord_ptr->get()->get_station();
  int sector = coord_ptr->get()->get_sector();
  int column = coord_ptr->get()->get_column();
  int index = coord_ptr->get()->get_index();

  TFvtxCompactCoordMap::iterator ccoord_iter = ccoord_map->insert_new(arm,cage,station,sector,column);
  TFvtxCompactCoordMap::pointer ccoord_ptr = ccoord_iter.current();
  PHKey::associate( ccoord_ptr, coord_ptr );
  
  TFvtxCompactCoord_v1* fvtx_coord = (TFvtxCompactCoord_v1*)ccoord_ptr->get();

  PHLine coord = coord_ptr->get()->get_coord();
  fvtx_coord->set_coord(coord);

  fvtx_coord->set_index(index);

  if (coord_ptr->get()->get_usedintrack())
    fvtx_coord->set_usedintrack();
}
