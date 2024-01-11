// $Id: mMuiRoadFinderPass2.cxx,v 1.3 2007/03/26 23:18:44 hugo Exp $
/////////////////////////////////////////////////////////////////////////
//
// Utility class: mMuiRoadFinderPass2:
// Description: 2nd pass road-finder; 
//              Kalman-style approach to go through MUID clusters
//              after MUTR track has been reconstructed.
//              
/////////////////////////////////////////////////////////////////////////

// headers
//
#include <mMuiRoadFinderPass2.h>
#include <mMuiRoadFinderPass2Par.h>
#include <TMutMCTrkMap.h>
#include <TMui1DRoadMapO.h>
#include <MuiCommon.hh>

// PHENIX headers
//

/*! \ingroup modules */

// STL/BOOST
//
#include <iostream>
#include <string>
//INCLUDECHECKER: Removed this line: #include <map>

// ROOT
//INCLUDECHECKER: Removed this line: #include <TMath.h>

using namespace std;

// convenience typedefs, if any
typedef list< TMuiClusterMapO::value_type >::iterator mui_cluster_iterator;  
typedef list< TMuiClusterMapO::value_type >::reverse_iterator reverse_mui_cluster_iterator;  

//______________________________________________________________________
mMuiRoadFinderPass2::mMuiRoadFinderPass2() : 
  _timer( PHTimeServer::get()->insert_new("mMuiRoadFinderPass2") ),
  _mod_par( 0 ),
  _trk_map( 0 ),
  _road_map( 0 ),
  _cluster_map( 0 ),
  
  _file( 0 ),
  _tree( 0 ),
  _arm( 0 ),
  _plane( 0 ),
  _orient( 0 ),
  _chi_square( 0 ),
  _road_tree( 0 ),
  _ndf( 0 )
{
  MUIOO::TRACE("initializing module mMuiRoadFinderPass2");  
  _res.assign( 0 );
  _res_cov.assign( 0 );
  
}

//______________________________________________________________________
// Destructor
mMuiRoadFinderPass2::~mMuiRoadFinderPass2() 
{ 
  finish_evaluation();
}

//______________________________________________________________________
bool mMuiRoadFinderPass2::init_tree()
{
  enum { BUFFER_SIZE=32000 };
  enum { AUTO_SAVE=16000 };  
    
  _file = new TFile( _mod_par->get_evaluation_file().c_str(), "RECREATE" );
  
  _road_tree = new TTree( "roads", "road parameters" );
  _road_tree->Branch( "T_arm", &_arm, "T_arm/I", BUFFER_SIZE );
  _road_tree->Branch( "T_ndf", &_ndf, "T_ndf/I", BUFFER_SIZE );
  _road_tree->Branch( "T_chi_square",  &_chi_square, "T_chi_square/D", BUFFER_SIZE );
  _road_tree->SetAutoSave( AUTO_SAVE );

  _tree = new TTree( "residuals", "single hit residuals" );
  _tree->Branch( "T_arm", &_arm, "T_arm/I", BUFFER_SIZE );
  _tree->Branch( "T_plane", &_plane, "T_plane/I", BUFFER_SIZE );
  _tree->Branch( "T_orient", &_orient, "T_orient/I", BUFFER_SIZE );
  _tree->Branch( "T_res", &_res[0], "T_res[3]/D", BUFFER_SIZE );
  _tree->Branch( "T_res_cov", &_res_cov[0], "T_res_cov[3]/D", BUFFER_SIZE );
  _tree->Branch( "T_chi_square",  &_chi_square, "T_chi_square/D", BUFFER_SIZE );
  _tree->Branch( "T_meas_cov", &_meas_cov, "T_meas_cov/D", BUFFER_SIZE );
  _tree->Branch( "T_depth", &_depth, "T_depth/I", BUFFER_SIZE ); 
  _tree->Branch( "T_depth_mc", &_depth_mc, "T_depth_mc/I", BUFFER_SIZE ); 
  _tree->SetAutoSave( AUTO_SAVE );
  
  return true;

}

//______________________________________________________________________
void mMuiRoadFinderPass2::finish_evaluation( void )
{

  if( _file ) { 
    _file->Write();
    _file->Close();
    _file = 0;
  }
  
  return;
}


//______________________________________________________________________
// Event method.
PHBoolean mMuiRoadFinderPass2::event(PHCompositeNode* top_node)
{
  _timer.get()->restart(); 
  
  try { 

    // Reset IOC pointers
    //
    set_interface_ptrs(top_node);
    
    // on first call, initialize tree if requested
    if( _mod_par->get_do_evaluation() ) 
    static bool init_done __attribute__((unused)) = init_tree(); 

    track_loop();

  } catch(std::exception& e) {
    MUIOO::TRACE(e.what());
    return False;
  }

  _timer.get()->stop(); 
  if(_mod_par->get_verbosity() >= MUIOO::SOME) _timer.get()->print();  
  if(_mod_par->get_verbosity() >= MUIOO::ALOT) _road_map->print();  
  return True;
}

//______________________________________________________________________
/*! Reset IOC and external interface pointers */
void mMuiRoadFinderPass2::set_interface_ptrs(PHCompositeNode* top_node)
{  

  // module runtime parameters
  //
  _mod_par = TMutNode<mMuiRoadFinderPass2Par>::find_node(top_node,"mMuiRoadFinderPass2Par");

  // trk, road and cluster maps
  _trk_map = TMutNode<TMutTrkMap>::find_node(top_node,"TMutTrkMap");

  _road_map = TMutNode<TMuiRoadMapO>::find_node(top_node,"TMuiRoadMapO");
  _cluster_map = TMutNode<TMuiClusterMapO>::find_node(top_node,"TMuiClusterMapO");

  return;
} 

//______________________________________________________________________
void mMuiRoadFinderPass2::track_loop()
{
  // this method and the methods called are all modelled after mMutKalFit 
  // Loop over all the tracks 
  TMutTrkMap::const_iterator trk_iter = _trk_map->range();
  while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() ) 
  {

    // perform basic checks on the track                                   
    if( ! accept_track( trk_ptr ) ) continue;

    // get associated road(s)
    TMuiRoadMapO::key_iterator road_iter = trk_ptr->get()->get_associated<TMuiRoadO>();
    while ( TMuiRoadMapO::pointer road_ptr = road_iter.next() ) {

      TMutKalmanFilter kalman_filter;
      kalman_filter.set_verbosity( MUTOO::Verbosity(_mod_par->get_verbosity()) ); 

      // initialize starting parameters and nodes,
      // then perform full fit                                                       
      if( !init_and_fit_nodes(  trk_ptr, road_ptr, kalman_filter ) ) {
        // failed; move on
        continue;
      }
      
      // store road info; make associations etc, as needed
      fill_road( road_ptr );

      // fill evaluation tree if requested                                   
      if( _mod_par->get_do_evaluation() ) {
      
        // retrieve road depth
        _depth = road_ptr->get()->get_pass2_depth();
       
        // retrieve associated MC track depth, if any
        _depth_mc = -1;
        TMutMCTrkMap::key_iterator mc_trk_iter = road_ptr->get()->get_associated<TMutMCTrk>();
        if( mc_trk_iter.count() ) 
        {
          
          // retrieve all associated roads
          TMuiRoadMapO::key_iterator road_iter_2 = mc_trk_iter->get()->get_associated<TMuiRoadO>();
          while( TMuiRoadMapO::pointer road_ptr_2 = road_iter_2.next() )
          _depth_mc = max( _depth_mc, static_cast<int>(road_ptr_2->get()->get_depth()) );
        }
        
        fill_tree( road_ptr );
      }
      
    } // road loop
  } // end of trk loop

  return ;
}

//______________________________________________________________________
bool mMuiRoadFinderPass2::accept_track(TMutTrkMap::const_pointer trk_ptr ) const
{
  
  // check track was properly reconstructed
  if( !trk_ptr->get()->get_reco_success() ) return false;
  
  // more preciselly, check that track kalman filter fit worked
  if( !trk_ptr->get()->get_kalman_fit() ) return false;
  
  return true;
}

//______________________________________________________________________
bool mMuiRoadFinderPass2::init_and_fit_nodes( TMutTrkMap::const_pointer trk_ptr, 
                TMuiRoadMapO::pointer road_ptr, 
                TMutKalmanFilter& kalman_filter) 
{
  // make sure list of nodes is empty
  _nodes.clear();
  
  // stores reference to starting parameters locally and initialize
  TMutKalmanFilter::TrkPar& start_kf( kalman_filter.get_starting_parameters() );
  const TMutTrkPar& trk_par( trk_ptr->get()->get_trk_par_list()->back() );

  start_kf._state_kf = TMutKalmanUtil::get_state_vector_kf( trk_par );
  start_kf._z = trk_par.get_z();
  start_kf._direction = ( trk_ptr->get()->get_arm() == MUIOO::South ) ? -1:1;
  start_kf._covar_kf = TMutKalmanUtil::get_covar_kf( trk_par );

  if( _mod_par->get_verbosity() >= MUIOO::MAX ) {
    MUIOO::TRACE( "mMuiRoadFinderPass2::init - start parameters");
    start_kf.print();
  }
    
  // add measurements from the roads
  TMuiClusterMapO::key_iterator mui_cluster_iter = road_ptr->get()->get_associated<TMuiClusterO>();
  while(TMuiClusterMapO::pointer mui_cluster_ptr = mui_cluster_iter.next()) 
  _nodes.push_back( KalmanFilterNode( mui_cluster_ptr ) );

  // sort nodes from vertex out
  _nodes.sort();

  // dump all nodes
  if( _mod_par->get_verbosity() >= MUIOO::MAX ) 
  for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) 
  {
    MUIOO::PRINT( cerr, "mMuiRoadFinderPass2::init_nodes - node");
    cerr << "measurement" << node->get_measurement();
    cerr << "covariance" << node->get_measurement_cov();
    cerr << "h" << node->get_h();
  }
    
  // initialize chisquare
  double chi_square = 0;

  // reset node status
  reset_nodes();
  
  // perform prediction and filter using reverse iterator  
  node_iterator node = _nodes.begin();
  while(  node != _nodes.end() ) {
    
    // perform kalman prediction/filter; test success
    if( !( kalman_filter.predict( *node ) && kalman_filter.const_filter( *node ) ) ) return false;    
    
    // retrieve node residual and error
    double residual( fabs( node->get_filtered_residual()(0,0) ) );
    double residual_err( sqrt( node->get_filtered_residual_cov()(0,0) ) );
    
    // test the residual/pulls/chisquare
    bool accepted( true );
    if( _mod_par->get_residual_cut() > 0 && residual > _mod_par->get_residual_cut() ) accepted = false;
    else if( _mod_par->get_pull_cut() > 0 && residual/residual_err > _mod_par->get_pull_cut() ) accepted = false;
    else if( _mod_par->get_chi_cut() > 0 && node->get_chi_square() > _mod_par->get_chi_cut() ) accepted = false;
    
    if( accepted ) { 
      chi_square += node->get_chi_square();
      kalman_filter.set_running_trk_par( *node );
      node++;
    }else{   
    
//////////////////////////////////////////////////////
// for now we keep the associated cluster list intact
//       // erase measurement we don't want remove corresponding associations
//       PHKey::disassociate( road_ptr, &node->get_mui_cluster() );
//////////////////////////////////////////////////////
      if( _mod_par->get_verbosity() >= MUIOO::ALOT ) {
        cout << "mMuiRoadFinderPass2::init_and_fit_nodes - rejecting node " << endl;
        node->print();
      }
      
      node = _nodes.erase(node);
    
    }
    
  }       
              
  // initialize smoothing starting from last gap node
  if( _mod_par->get_do_smoothing() ) {
  
    bool first_node( true );
    for( reverse_node_iterator node = _nodes.rbegin(); node != _nodes.rend(); node ++ )
    if( first_node ) {
      first_node = false;
      kalman_filter.initialize_smoother( *node );
    } else if( !kalman_filter.smooth( *node ) ) return false;
    
  }
  
  // dump chisquare
  if( _mod_par->get_verbosity()>=MUIOO::ALOT ) 
  cerr << "mMuiRoadFinderPass2::init_and_fit_nodes - total chi2=" << chi_square << endl;

  return true;
}
  
//___________________________________________
bool mMuiRoadFinderPass2::fill_road( TMuiRoadMapO::pointer road_ptr ) const
{

  // retrieve max reached plane from associated clusters
  // and create new gap bit
  UShort_t max_plane( 0 );
  UShort_t gap_bit( 0 );
  
  // loop over remaining (i.e. not rejected) nodes. Set the pass2 gap bit and depth
  for( const_node_iterator node = _nodes.begin(); node != _nodes.end(); node++ ) {
    
    // retrieve associated mui cluster
    TMuiClusterMapO::value_type mui_cluster( node->get_mui_cluster() );

    // check cluster plane
    UShort_t plane( mui_cluster.get()->get_plane() );
    if( plane > max_plane ) max_plane = plane;
  
    // update gap bit
    UShort_t orientation( mui_cluster.get()->get_orientation() );
    UShort_t this_bit( 0x1 << plane );
    gap_bit |= ( ( orientation == kHORIZ ) ? this_bit:( this_bit << 5 ) );
  
  }
  
  // update depth and gap bit
  road_ptr->get()->set_pass2_depth( max_plane );
  road_ptr->get()->set_pass2_gapbit( gap_bit );

  // some checks/dumps
  if( _mod_par->get_verbosity() >= MUIOO::ALOT ) {
  
    if( road_ptr->get()->get_gapbit() != road_ptr->get()->get_pass2_gapbit() ) { 
      
      //! dump old/new gap bit and number of clusters
      cout 
        << "mMuiRoadFinderPass2::fill_road - road=" << road_ptr->get()->get_key().get_obj_key()
        << " gap_bit= [before] " << road_ptr->get()->get_gapbit() << " [after] " << road_ptr->get()->get_pass2_gapbit() << endl;
      
      cout 
        << "mMuiRoadFinderPass2::fill_road - #clusters=" << road_ptr->get()->get_associated<TMuiClusterO>().count() 
        << " #nodes=" << _nodes.size() << endl;
 
      // retrieve/dump associated clusters
      TMuiClusterMapO::const_key_iterator mui_cluster_iter = road_ptr->get()->get_associated<TMuiClusterO>();
      while(TMuiClusterMapO::const_pointer mui_cluster_ptr = mui_cluster_iter.next())
      cout 
        << " cluster plane=" << mui_cluster_ptr->get()->get_plane() 
        << " key=" << mui_cluster_ptr->get()->get_key().get_obj_key()
        << endl;
      
      // retrieve associated 1D roads, dumps corresponding depth and clusters
      TMui1DRoadMapO::const_key_iterator road_1d_iter = road_ptr->get()->get_associated<TMui1DRoadO>();
      while(TMui1DRoadMapO::const_pointer road_1d_ptr = road_1d_iter.next()) {
        
        cout 
          << "mMuiRoadFinderPass2::fill_road - 1d road=" << road_1d_ptr->get()->get_key().get_obj_key() 
          << " depth=" << road_1d_ptr->get()->get_depth() << endl;
 
        TMuiClusterMapO::const_key_iterator mui_cluster_iter = road_1d_ptr->get()->get_associated<TMuiClusterO>();
        
        while(TMuiClusterMapO::const_pointer mui_cluster_ptr = mui_cluster_iter.next())
        cout 
          << " 1d cluster plane=" << mui_cluster_ptr->get()->get_plane() 
          << " key=" << mui_cluster_ptr->get()->get_key().get_obj_key()
          << endl;
      
      }
      
    }
    
    if( road_ptr->get()->get_depth() != road_ptr->get()->get_pass2_depth() ) 
    cout 
      << "mMuiRoadFinderPass2::fill_road - road=" << road_ptr->get()->get_key().get_obj_key() 
      << " depth=" << road_ptr->get()->get_depth() 
      << " pass2_depth=" << road_ptr->get()->get_pass2_depth() << endl;
    
  }
  
  // return if smoothing was not done
  if( !_mod_par->get_do_smoothing() ) return true;
  
  // update the road fit parameters
  // retrieve first smoothed node
  const KalmanFilterNode *smoothed_node( 0 );
  for( const_node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
  if( node->smooth_done() ) {
    smoothed_node = &(*node);
    break;
  }
  
  if( !smoothed_node ) {
    if( _mod_par->get_verbosity() >= MUIOO::SOME ) 
    cerr << "mMuiRoadFinderPass2::fill_road - could not find any smoothed node." << endl;
    return false;
  }

  // retrieve/update chisquare and number of degrees of freedom
  double chi_square = 0;
  UShort_t ndf = 0;
  
  for( const_node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
  if( node->filter_done() ) {
    ndf++;
    chi_square += node->get_chi_square();
  }
  
  if( ndf >= 5 ) ndf -= 5; 
  else ndf = 0;
  
  road_ptr->get()->set_freedom( ndf );
  road_ptr->get()->set_road_quality( static_cast< Float_t >( (ndf) ? chi_square/ndf : 0.0 ) );
  
  // create FitParameters  
  TMutFitPar fit_par( 
    smoothed_node->get_smoothed().get_x(),
    smoothed_node->get_smoothed().get_y(),
    smoothed_node->get_smoothed().get_z(),
    smoothed_node->get_smoothed().get_dxdz(),
    smoothed_node->get_smoothed().get_dydz(),
    chi_square );
  
  // update covariance matrix (is 4x4 )
  for( unsigned int i=0; i < 4; i++ )
  for( unsigned int j=0; j < 4; j++ )
  fit_par.set_covar( i, j, smoothed_node->get_smoothed()._covar_kf( i+1, j+1 ) );
  
  // assign fit_parameters to the road 
  road_ptr->get()->set_fit_par( fit_par );
  
  return true;
}     
   
//___________________________________________
void mMuiRoadFinderPass2::fill_tree( TMuiRoadMapO::const_pointer road_ptr )
{
  if( _tree ) {
    for( node_iterator node = _nodes.begin(); node != _nodes.end(); node++ )
    {
    
      // stores mui_cluster location
      _arm = node->get_mui_cluster().get()->get_arm();
      _plane = node->get_mui_cluster().get()->get_plane();
      _orient = node->get_mui_cluster().get()->get_orientation();
  
      // initialise residuals and errors
      _res.assign(0);
      _res_cov.assign(0);
      _chi_square = 0;
      _meas_cov = node->get_measurement_cov()(0,0);
    
      // fill predicted residuals
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
    
      // fill smoothed residuals
      if( node->smooth_done() ) {
        _res[2] = node->get_smoothed_residual()(0,0);
        _res_cov[2] = node->get_smoothed_residual_cov()(0,0);
      }
  
      // fill tree
      _tree->Fill();
    
    }
  }
  
  if( _road_tree ) {

    _arm = road_ptr->get()->get_arm();
    _ndf = road_ptr->get()->get_freedom();
    _chi_square = road_ptr->get()->get_const_fitpar()->get_chi_square();
    _road_tree->Fill();

  }
  return;
}

//__________________________________________________________
mMuiRoadFinderPass2::KalmanFilterNode::KalmanFilterNode( const TMuiClusterMapO::pointer& cluster_ptr )
{
  set_arm( cluster_ptr->get()->get_arm() );
  
  // stores mean z
  set_z ( 0.5*(
    cluster_ptr->get()->get_coord_begin().getZ() +
    cluster_ptr->get()->get_coord_end().getZ() ) );
  
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
  UShort_t orientation( cluster_ptr->get()->get_orientation() );
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


