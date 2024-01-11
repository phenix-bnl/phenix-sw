// $Id: mFvtxKalFitMC.cxx,v 1.12 2017/07/13 19:12:16 phnxbld Exp $

/*!
   \file    mFvtxKalFitMC.cxx
   \brief Kalman filter fit for perfect FVTXOO tracks.
   MC hits are used for fitting in this "MC" version of fitting.
   \author  Melynda Brooks
   \version $Revision: 1.12 $
   \date    $Date: 2017/07/13 19:12:16 $
*/

// IOC
#include<TMutTrkMap.h>

// FVTXOO
#include<FVTXOO.h>
#include<PHException.h>
#include<PHGeometry.h>
#include<PHTFileServer.h>
#include<PHTimer.h>
#include<PHTrackIntegratorKF.h>
#include<TMutKalmanUtil.h>
#include<TMutNode.h>
#include<TMutTrackUtil.h>
#include<TMutTrkPar.hh>
#include<TFvtxPisaHitMap.h> 

// GLOBAL
#include<PHLine.h>
#include<PHPoint.h>
#include<PHVector.h>

#include <gsl/gsl_randist.h>
#include <gsl/gsl_math.h>
#include <cmath>

// STL/BOOST
#include<iostream>
#include<string>
#include<list>

#include "mFvtxKalFitMC.h"
#include "mFvtxKalFitMCPar.h"

using namespace std;

//______________________________________________________________________
mFvtxKalFitMC::mFvtxKalFitMC() : 
  _timer(PHTimeServer::get()->insert_new("mFvtxKalFitMC") ),
  _top_node( 0 ),
  _mod_par( 0 ),
  _trk_map( 0 ),
  _filename( "mFvtxKalFitMC.root" ),
  _tree( 0 ),
  _arm( 0 ),
  _cage( 0 ),
  _station( 0 ),
  _sector( 0 ),
  _chi_square( 0 )
{
  FVTXOO::TRACE("initializing module mFvtxKalFitMC");
  _res.assign( 0 );
  _res_cov.assign( 0 );
  integrator = new PHTrackIntegratorKF(); 
}

//______________________________________________________________________
// Destructor
mFvtxKalFitMC::~mFvtxKalFitMC() 
{ 
  finish_evaluation(); 
  _nodes.clear();
  if(integrator) delete integrator;
}

//______________________________________________________________________
void mFvtxKalFitMC::init_tree()
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
  _tree->SetAutoSave( AUTO_SAVE );
}

//______________________________________________________________________
void mFvtxKalFitMC::finish_evaluation( void )
{
  
  if( !_tree ) return;
    
  FVTXOO::TRACE("mFvtxKalFitMC::finish_evaluation");       
  PHTFileServer::get().write( _filename );    
  return;
}

//______________________________________________________________________
// Event method.
PHBoolean mFvtxKalFitMC::event(PHCompositeNode* top_node)
{

  _timer.get()->restart(); 
  
  try { 

    static bool init_done = false;

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
  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _trk_map->print();
  if(_mod_par->get_verbosity() >= FVTXOO::SOME) _timer.get()->print();  

  return True;
}

//______________________________________________________________________
void mFvtxKalFitMC::set_interface_ptrs(PHCompositeNode* top_node)
{  
  _top_node = top_node;
  _mod_par = TMutNode<mFvtxKalFitMCPar>::find_node(top_node,"mFvtxKalFitMCPar"); 
  _mod_par->print(); 
  _trk_map = TMutNode<TFvtxTrkMap>::find_node(top_node,"TFvtxTrkMap");    
}

//______________________________________________________________________
void mFvtxKalFitMC::track_loop()
{
  // Loop over all track in the track map
  TFvtxTrkMap::iterator trk_iter = _trk_map->range();    
  while(TFvtxTrkMap::pointer trk_ptr = trk_iter.next()) 
  try {
    
    if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
    cout << "mFvtxKalFitMC::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;
    
    // perform basic checks on the track
    if( !accept_track( trk_ptr ) ) {
      trk_ptr->get()->set_kalman_fail();  
      continue;
    }

    TMutKalmanFilter kalman_filter;
    //kalman_filter.set_verbosity( _mod_par->get_verbosity() );

    // initialize starting parameters and nodes
    init( trk_ptr, kalman_filter );  
  
    // perform full fit 
    if( !fit_nodes( kalman_filter ) ) {
      if( _mod_par->get_verbosity() >= FVTXOO::SOME )
      cout << "mFvtxKalFitMC::track_loop - trk=" << trk_ptr->get()->get_key().get_obj_key() << " fit failed." << endl;
      trk_ptr->get()->set_kalman_fail();  
      continue;
    }
  
    // fill the track with node informations
    fill_track( trk_ptr );
    
    // extrapolate track parameters toward vertex
    extrapolate_to_vertex( trk_ptr );
    
    // fill evaluation tree if requested
    if( _mod_par->get_do_evaluation() ) fill_tree();
    
    // set track status
    if( !trk_ptr->get()->get_kalman_fail() ) trk_ptr->get()->set_kalman_fit();
  } catch( exception &e ) { cout << "mFvtxKalFitMC::track_loop - " << e.what() << endl; }

}

//______________________________________________________________________
bool mFvtxKalFitMC::accept_track( TFvtxTrkMap::const_pointer trk_ptr ) const
{

  // check number of hits in silicon system
  TFvtxMCHitMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxMCHit>();
  if ( coord_iter.count() < _mod_par->get_min_n_coord() )  {
    trk_ptr->get()->set_reco_min_hits();
    trk_ptr->get()->set_no_fit();
    
    if( _mod_par->get_verbosity()>= FVTXOO::SOME )
    cout << "mFvtxKalFitMC::accept_track - trk=" 
      << trk_ptr->get()->get_key().get_obj_key() 
      << " not enough hits (" << coord_iter.count() << ").\n";
    
    return false; 
  }
  
  return true;

}

//______________________________________________________________________
void mFvtxKalFitMC::init( 
  TFvtxTrkMap::const_pointer trk_ptr, 
  TMutKalmanFilter& kalman_filter  
) 

{

  // make sure list of nodes is empty
  _nodes.clear();
    
  // stores reference to starting parameters localy and initialize
  TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );
  
  // Get vertex projection point from starting parameters for now (filled in mFvtxFindTrackMC)
  _mod_par->set_z_reference(start_kf._z); 

  if( _mod_par->get_verbosity() >= FVTXOO::ALOT ) start_kf.print();
  
  start_kf._direction = ( trk_ptr->get()->get_arm() == FVTXOO::South ) ? -1:1;
  
  // adds coordinate associated to the track to the kalman filter object  
  // retrieve coordinates associated to the track
  list<TFvtxMCHitMap::value_type> coord_list;
  typedef list< TFvtxMCHitMap::value_type >::iterator coord_iterator;  
  
  TFvtxMCHitMap::key_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxMCHit>();
  while(TFvtxMCHitMap::pointer coord_ptr = coord_iter.next()) coord_list.push_back( *coord_ptr );
  
  // Add a vertex point:
  // Add a bunch of points between vertex and first silicon plane, to see how trajectory
  // is deviating from a straight line:
  TMutMCTrkMap::key_iterator mctrk_iter = trk_ptr->get()->get_associated<TMutMCTrk>();
  
  
  // optionally add vtx hits to fit:
  float zsmear, phismear;
  
  if ( _mod_par->get_use_vtx() ) 
  {  
    TFvtxPisaHitMap::key_iterator key_iter( trk_ptr->get()->get_associated<TFvtxPisaHit>() );
    while( TFvtxPisaHitMap::const_pointer ptr = key_iter.next() )
    {
      
//      zsmear = gsl_ran_gaussian(_rng.get(), _mod_par->get_vtx_z_smear());
      zsmear = (gsl_rng_uniform(_rng.get()) - 0.5)*_mod_par->get_vtx_z_smear();
//      phismear = gsl_ran_gaussian(_rng.get(), _mod_par->get_vtx_phi_smear());
      phismear = (gsl_rng_uniform(_rng.get()) - 0.5)*_mod_par->get_vtx_phi_smear();
      
      int xyflag = 0;
      KalmanFilterNode nodex( *ptr->get()->get_pisa_hit(), xyflag, zsmear, phismear );
      _nodes.push_back( nodex );
      
      xyflag = 1;
      KalmanFilterNode nodey( *ptr->get()->get_pisa_hit(), xyflag, zsmear, phismear );
      _nodes.push_back( nodey );
      
    }
  }
  
  // sort the list of coordinates in z
  coord_list.sort( coord_less_ftor() );
  
  // add measurements
  for( coord_iterator coord = coord_list.begin(); coord != coord_list.end();	coord++ )
  {
  
    // select phi, r smear values over a flat distribution
    double  phismearf = (gsl_rng_uniform(_rng.get()) - 0.5)*_mod_par->get_fvtx_phi_smear();
    double  rsmear = (gsl_rng_uniform(_rng.get()) - 0.5)*_mod_par->get_fvtx_r_smear();

    // create new node for x measurement
    int xyflag = 0;
    KalmanFilterNode nodex( &(*coord), xyflag, phismearf, rsmear );
    
    if( _mod_par->get_verbosity() >= FVTXOO::ALOT ){
      cout << "measurement x" << nodex.get_measurement() << endl ;
      cout << "z = " << nodex.get_z() << endl ;
      cout << "covariance" << nodex.get_measurement_cov() << endl ;
      cout << "h" << nodex.get_h() << endl ;
    }
    
    // add node
    _nodes.push_back( nodex );
  
    // create new node for y measurement
    xyflag = 1;    
    KalmanFilterNode nodey( &(*coord), xyflag, phismearf, rsmear ); 
    
    if( _mod_par->get_verbosity() >= FVTXOO::ALOT )
    {
      cout << "measurement y" << nodey.get_measurement() << endl ;
      cout << "z = " << nodey.get_z() << endl ;
      cout << "covariance" << nodey.get_measurement_cov() << endl ;
      cout << "h" << nodey.get_h() << endl ;
    }
    
    // add node
    _nodes.push_back( nodey );
    
  }
  
  // set z from last coordinate
  start_kf._z = coord_list.back().get()->get_z();
  
  // extrapolate track parameters to _z
  const TMutTrkPar trk_par( *trk_ptr->get()->get_trk_par() );
  
  if( _mod_par->get_verbosity() >= FVTXOO::ALOT ) trk_par.print();
  
  TMutTrkPar trk_par_extrap( 
  trk_par.get_x() + trk_par.get_dxdz()*(start_kf._z - trk_par.get_z()),
  trk_par.get_y() + trk_par.get_dydz()*(start_kf._z - trk_par.get_z()),
  start_kf._z,
  trk_par.get_px(),
  trk_par.get_py(),
  trk_par.get_pz(),
  trk_par.get_charge() );
  
  if( _mod_par->get_verbosity() >= FVTXOO::ALOT ) trk_par_extrap.print();
  
  start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par_extrap );
  
  // in both cases, use the default covariance matrix for error
  start_kf._covar_kf(0, 0) = FVTXOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) );	// error on c/p
  start_kf._covar_kf(1, 1) = FVTXOO::SQUARE( _mod_par->get_angular_resolution() );	// error on dx/dz
  start_kf._covar_kf(2, 2) = FVTXOO::SQUARE( _mod_par->get_angular_resolution() );	// error on dy/dz
  start_kf._covar_kf(3, 3) = FVTXOO::SQUARE( _mod_par->get_position_resolution() ); // error on x
  start_kf._covar_kf(4, 4) = FVTXOO::SQUARE( _mod_par->get_position_resolution() ); // error on y
  
  // sort nodes from vertex to muid 
  _nodes.sort();
  
  // dump all nodes
  if( _mod_par->get_verbosity() >= FVTXOO::MAX ) 
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) 
  {
    FVTXOO::PRINT( cout, "mFvtxKalFitMC::init_nodes - node");
    cout << "measurement" << node->get_measurement() << endl;
    cout << "covariance" << node->get_measurement_cov() << endl;
    cout << "h" << node->get_h() << endl;
  }
}
  
//___________________________________________________________________
bool mFvtxKalFitMC::fit_nodes( TMutKalmanFilter &kalman_filter )
{
  
  // loop over max number of iterations
  unsigned int iteration( 0 );
  double old_chi_square = 1e6;
  for( ; iteration < _mod_par->get_max_iterations(); iteration++ ) 
  {
    
    double chi_square( 0 );
    
    // reset node status
    reset_nodes();
    
    // perform prediction and filter using reverse iterator  
    for( reverse_node_iterator node = _nodes.rbegin(); node != _nodes.rend(); node ++ )
    if( kalman_filter.predict( *node ) && kalman_filter.filter( *node ) ) chi_square += node->get_chi_square();
    else {
      if( _mod_par->get_verbosity()>= FVTXOO::SOME ) 
      cout << "mFvtxKalFitMC::fit_nodes - prediction/filter failed.\n";
      return false;
    }
          
    // initialize smoothing starting from first node (closest to vertex)
    bool first_node( true );
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node ++ )
    if( first_node ) {
      first_node = false;
      kalman_filter.initialize_smoother( *node );
    } else if( !kalman_filter.smooth( *node ) ) {
      cout << "mFvtxKalFitMC::fit_nodes - prediction/filter failed.\n";
      return false;
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
      start_kf._covar_kf(0, 0) = FVTXOO::SQUARE( _mod_par->get_momentum_resolution()*start_kf._state_kf(0,0) );  // error on c/p
    
    }
    
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      FVTXOO::TRACE( "mFvtxKalFitMC::fit_nodes - start parameters [updated]");
      start_kf.print();
    }
    
    // dump chisquare
    if( _mod_par->get_verbosity()>=FVTXOO::SOME ) 
    cout << "mFvtxKalFitMC::fit_nodes - iteration " << iteration << ", chi2=" << chi_square << endl;
    
    if( iteration && fabs( chi_square - old_chi_square ) < _mod_par->get_chi_cut()*old_chi_square ) break;
    old_chi_square = chi_square;
  
  }

  if( _mod_par->get_verbosity() >= FVTXOO::SOME ) 
  cout << "mFvtxKalFitMC::fit_nodes - " << iteration << " iterations.\n";

  return true;
}
  
//___________________________________________
bool mFvtxKalFitMC::fill_track( TFvtxTrkMap::pointer trk_ptr )
{
  
  if( _mod_par->get_verbosity() >= FVTXOO::SOME )
  cout << "mFvtxKalFitMC::fill_track - trk=" << trk_ptr->get()->get_key().get_obj_key() << endl;

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

                if( _mod_par->get_verbosity() >= FVTXOO::ALOT )trk_par.print();
    
    PHGslMatrix cov_mutoo( TMutKalmanUtil::cov_kalman_to_mutoo( 
      trk_par.get_charge(),
      node->get_smoothed()._covar_kf,
      mom_mutoo ) );
     
    for( unsigned int i=0; i<TMutTrkPar::COVAR_ROW; i++ )
    for( unsigned int j=0; j<TMutTrkPar::COVAR_ROW; j++ )
    trk_par.set_covar( i, j, cov_mutoo(i,j) );
     
    trk_ptr->get()->push_trk_par( trk_par );  
    /*
    if( node->has_fvtx_mchit() ) 
    {
      
      // store chi_square contribution into coord
      node->get_fvtx_mchit().get()->push_chi_sqr_inc( 
        trk_ptr->get()->get_key().get_obj_key(), 
        node->get_chi_square() );
        
      // push residuals
      push_r_residual( trk_ptr, trk_par, node->get_fvtx_mchit() );
      push_w_residual( trk_ptr, trk_par, node->get_fvtx_mchit() );
    
    }
    */
    
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
  // trk_ptr->get()->set_hit_pattern( trk_ptr->get()->TFvtxTrk::get_hit_pattern() );
  
  // set track parameters in first gap
  trk_ptr->get()->set_trk_par(  trk_ptr->get()->get_trk_par_list()->front() );
    
  return true;
}     

//_____________________________________________________________________________
bool mFvtxKalFitMC::extrapolate_to_vertex( TFvtxTrkMap::pointer trk_ptr )
{
  integrator->clear();
  // extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
  double z_ref = _mod_par->get_z_reference(); 
  
  if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
    FVTXOO::PRINT( cout, "mFvtxKalFitMC::extrapolate_to_vertex - before extrapolation");
    TMutKalmanUtil::print_trk_par_kf( trk_ptr->get()->get_trk_par_list()->front() );
  }
  

  integrator->initialize( trk_ptr->get()->get_trk_par_list()->front() );
  integrator->extrapolate( z_ref );
  if( integrator->get_error() ) {
    if( _mod_par->get_verbosity() >= FVTXOO::SOME ) {
      cout 
        << "in mFvtxKalFitMC::extrapolate_to_vertex - extrapolation towards vertex failed ("
        << trk_ptr->get()->get_trk_par_list()->front().get_z() << "->" << z_ref << ")\n";
    }
    
    trk_ptr->get()->set_kalman_fail();
    return false;
    
  } else {
    TMutTrkPar extrap_trk_par;
    integrator->finish( extrap_trk_par );
    trk_ptr->get()->set_trk_par_vtx( extrap_trk_par );
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      FVTXOO::PRINT( cout, "mFvtxKalFitMC::extrapolate_to_vertex - trk_par_vtx");
      extrap_trk_par.print();
    }    
  }  


  return true;

}   

//_____________________________________________________________________________
bool mFvtxKalFitMC::extrapolate_to_mutr( TFvtxTrkMap::pointer trk_ptr )
{
  integrator->clear();
  // extrapolate track parameters from last gap to z of first Mutr gap
  double z_ref( 0 );
  TMutTrkMap::const_key_iterator mutr_iter = trk_ptr->get()->get_associated<TMutTrk>();
  if( !mutr_iter.count() ) 
  {
      
    // if no road is found get z from plane0 panel0 center.
    
    // retrieve panel 0 from plane 0 and track arm.
    
    // retrieve center position assign z to z_ref
    // z_ref = z_panel;
  
    // some dump
    // if( _mod_par->get_verbosity() >= FVTXOO::ALOT ) 
    // cout << "mFvtxKalFitMC::extrapolate_to_muid - track has no associated road. Using z=" << z_panel << " (panel 0, plane 0)" << endl;
    
  } else {
  
    // get the z from first road gap0 point
    // z_ref = road_iter->get()->get_gap0_point().getZ();
  
  }
  

  integrator->initialize( trk_ptr->get()->get_trk_par_list()->back() );
  integrator->extrapolate( z_ref );
  if( integrator->get_error() ) {
    
    if( _mod_par->get_verbosity() >= FVTXOO::SOME ) {
      cout 
        << "in mFvtxKalFitMC::extrapolate_to_muid - extrapolation towards MuID failed ("
        << trk_ptr->get()->get_trk_par_list()->back().get_z() << "->" << z_ref << ")\n";
    }
    return false;
    
  } else {
  
    TMutTrkPar extrap_trk_par;
    integrator->finish( extrap_trk_par );
    trk_ptr->get()->push_trk_par( extrap_trk_par );  
    if( _mod_par->get_verbosity() >= FVTXOO::MAX ) {
      FVTXOO::PRINT( cout, "mFvtxKalFitMC::extrapolate_to_muid - trk_par_muid");
      extrap_trk_par.print();
    }    
  
  }  
  

  return true;

}

//___________________________________________
void mFvtxKalFitMC::fill_tree( void )
{
  if( !_tree ) return;
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
  {
  
    // stores coord location
    _arm = node->get_fvtx_mchit().get()->get_arm();
    _cage = node->get_fvtx_mchit().get()->get_cage();
    _station = node->get_fvtx_mchit().get()->get_station();
    _sector = node->get_fvtx_mchit().get()->get_sector();

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
  
  }

}

//__________________________________________________________
void mFvtxKalFitMC::push_r_residual( TFvtxTrkMap::pointer trk_ptr, const TMutTrkPar &trk_par, TFvtxMCHitMap::value_type coord ) 
{ return; }

//__________________________________________________________
double mFvtxKalFitMC::push_w_residual( TFvtxTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par, TFvtxMCHitMap::value_type coord ) 
{ return 0.0; }

//__________________________________________________________
mFvtxKalFitMC::KalmanFilterNode::KalmanFilterNode( const TFvtxMCHitMap::pointer& coord_ptr, const int xyflag, const double phismear, const double rsmear )
{
  set_z( coord_ptr->get()->get_z() );
  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );
  _fvtx_mchit = *coord_ptr;
  
  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );
  double dx, dy = 0;
  double cosphi, sinphi;
  if (xyflag == 0 ){
     cosphi =  coord_ptr->get()->get_x()/sqrt(FVTXOO::SQUARE(coord_ptr->get()->get_x()) + FVTXOO::SQUARE(coord_ptr->get()->get_y()));
     sinphi =  coord_ptr->get()->get_y()/sqrt(FVTXOO::SQUARE(coord_ptr->get()->get_x()) + FVTXOO::SQUARE(coord_ptr->get()->get_y()));
     dx = sqrt(FVTXOO::SQUARE(rsmear*cosphi) + FVTXOO::SQUARE(phismear*sinphi));
     measurement( 0,0 ) = -coord_ptr->get()->get_x() + dx;
  } else if (xyflag == 1 ) {
    cosphi =  coord_ptr->get()->get_x()/sqrt(FVTXOO::SQUARE(coord_ptr->get()->get_x()) + FVTXOO::SQUARE(coord_ptr->get()->get_y()));
    sinphi =  coord_ptr->get()->get_y()/sqrt(FVTXOO::SQUARE(coord_ptr->get()->get_x()) + FVTXOO::SQUARE(coord_ptr->get()->get_y()));
    dy = sqrt(FVTXOO::SQUARE(rsmear*sinphi) + FVTXOO::SQUARE(phismear*cosphi));
    measurement( 0,0 ) = coord_ptr->get()->get_y() + dy;
  } else {
    cout << "Invalid flag used for set_measurement (svxhit): " << xyflag << endl;
    return;
  }

  //  NEED TO SET THIS WITH A PARAMETER
  measurement_cov( 0,0 ) = 0.01;

  // store projection matrix
  PHGslMatrix h(1,5);
  if (xyflag == 0){     // x measurement, strip angle = 90 degrees
    h( 0, 3 ) = -1.0;  // -sin(angle)
    h( 0, 4 ) = 0.0;   //  cos(angle)
  }
  
  else if (xyflag == 1){// y measurement, strip angle = 0 degrees
    h( 0, 3 ) = 0.0;
    h( 0, 4 ) = 1.0;
  }
  
  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );
  
}
//__________________________________________________________
mFvtxKalFitMC::KalmanFilterNode::KalmanFilterNode( const SvxSnglPisaHit& svxhit, const int xyflag,
  const float zsmear, const float phismear )
{

  set_z( svxhit.GetZGlobal() + zsmear  );

  cout << "Z position of barrel hit in mFvtxKalFitMC = " <<  svxhit.GetZGlobal() << endl;
  cout << "z smear of barrel hit in mFvtxKalFitMC = " <<  zsmear << endl;

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
mFvtxKalFitMC::KalmanFilterNode::KalmanFilterNode( const PHPoint* point, const int xyflag )
{
  set_z( point->getZ() );
  set_arm( (get_z()>0) ? MUTOO::North : MUTOO::South );
  
  // store measurement and error
  PHGslMatrix measurement( 1, 1 );
  PHGslMatrix measurement_cov( 1, 1 );
  if (xyflag == 0 ) measurement( 0,0 ) = -point->getX();
  else if (xyflag == 1 ) measurement( 0,0 ) = point->getY();
  else {
    cout << "Invalid flag used for set_measurement (svxhit): " << xyflag << endl;
    return;
  }

  //  NEED TO SET THIS WITH A PARAMETER
  measurement_cov( 0,0 ) = 5.00;

  // store projection matrix
  PHGslMatrix h(1,5);
  if (xyflag == 0){     
    // x measurement, strip angle = 90 degrees
    h( 0, 3 ) = -1.0;  // -sin(angle)
    h( 0, 4 ) = 0.0;   //  cos(angle)
  }
  
  else if (xyflag == 1){
    // y measurement, strip angle = 0 degrees
    h( 0, 3 ) = 0.0;
    h( 0, 4 ) = 1.0;
  }
  
  // update node
  set_measurement( measurement, measurement_cov );
  set_h( h );
  
}
