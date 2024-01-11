// $Id: mMutStraightFit.cxx,v 1.16 2011/12/24 04:48:31 slash Exp $

/*!
\file    mMutStraightFit.cxx
\brief   straight track fit (magnetic field = 0)
\author  Hugo Pereira
\version $Revision: 1.16 $
\date    $Date: 2011/12/24 04:48:31 $
*/

#include <PHCompositeNode.h>
#include <MUTOO.h>
#include <MuiCommon.hh>
#include <PHGeometry.h>
#include <PHTrackIntegratorKF.h>
#include <TMutClusMap.h>
#include <TMutExtVtx.h>
#include <TMutGeo.h>
#include <TMuiGeo.h>
#include <TMutNode.h>
#include <TMutMeasureModel.h>
#include <TMutTrackUtil.h>

#include "mMutStraightFit.h"
#include "mMutStraightFitPar.h"
#include <string>
using namespace std;

//__________________________________________________________
mMutStraightFit::mMutStraightFit( void ):
  _vertex_z(0),
  _trk_map(0),
  _mod_par(0),
  _timer(PHTimeServer::get()->insert_new("mMutStraightFit"))
{
  MUTOO::TRACE("initializing module mMutStraightFit",MUTOO::ALOT);
}

//__________________________________________________________
PHBoolean mMutStraightFit::event( PHCompositeNode *top_node )
{

  _timer.get()->restart();

  try {

    // Reset IOC pointers
    set_interface_ptrs(top_node);

    // load vertex z
    load_ext_vertex( top_node );

    // loop over roads
    track_loop();

  } catch(exception& e) {
    MUTOO::TRACE(e.what());
    return False;
  }

  _timer.get()->stop();
  return True;

}

//__________________________________________________________
void mMutStraightFit::set_interface_ptrs( PHCompositeNode *top_node )
{

  // load road map
  _trk_map = TMutNode< TMutTrkMap >::find_node( top_node, "TMutTrkMap" );
  _mod_par = TMutNode< mMutStraightFitPar >::find_node( top_node, "mMutStraightFitPar" );
}

//______________________________________________________________________
void mMutStraightFit::load_ext_vertex( PHCompositeNode* top_node )
{

  // reference z is used by default
  _vertex_z = _mod_par->get_z_reference();

  // try load external vertex
  bool error( false );
  PHPoint vtx( TMutExtVtx::get().get_vtx( error ) );
  if( error )
  {

    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    { cout << "mMutStraightFit::load_ext_vertex - wrong external vertex.\n"; }

  } else {
    _vertex_z = vtx.getZ();
  }

  return;

}

//__________________________________________________________
void mMutStraightFit::track_loop( void )
{

  // loop over roads
  TMutTrkMap::iterator trk_iter = _trk_map->range();
  while( TMutTrkMap::pointer trk_ptr = trk_iter.next() )
  {	 fit_track( trk_ptr ); }

}

//__________________________________________________________
bool mMutStraightFit::fit_track( TMutTrkMap::pointer trk_ptr ) const
{
  // allocate new TrackFitObject
  TMutStraightTrackFit trk_fit;
  trk_fit.set_z( 0 );

  // retrieve associated coordinates
  TMutCoordMap::key_iterator coord_iter( trk_ptr->get()->get_associated<TMutCoord>() );
  while( TMutCoordMap::pointer coord_ptr = coord_iter.next() )
  {

    // look if it is a desactivated detectors
    if(_mod_par->get_mutr_desactivated(
      coord_ptr->get()->get_arm(),
      coord_ptr->get()->get_station(),
      coord_ptr->get()->get_gap(),
      coord_ptr->get()->get_cathode())) continue;

    trk_fit.add_node( LocalNode( coord_ptr, trk_fit.get_z() ) );
    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    {
      cout
        << "mMutStraightFit::track_loop - adding coord "
        << coord_ptr->get()->get_key().get_obj_key()
        << " to track "
        << trk_ptr->get()->get_key().get_obj_key()
        << endl;
    }
  }

  // add muid hits if required
  if( _mod_par->get_use_muid() )
  {

    // set association to best muid road clusters
    associate_road( trk_ptr );

    // loop over found clusters
    TMuiClusterMapO::key_iterator clus_iter( trk_ptr->get()->get_associated<TMuiClusterO>() );
    while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
    {

      // look if it is a desactivated detectors
      if(_mod_par->get_muid_desactivated(
        clus_ptr->get()->get_arm(),
        clus_ptr->get()->get_plane(),
        clus_ptr->get()->get_panel() )) continue;

      trk_fit.add_node( LocalNode( clus_ptr, trk_fit.get_z() ) );

      if( _mod_par->get_verbosity() >= MUTOO::ALOT )
      {
        cout
          << "mMutStraightFit::track_loop - adding muid_cluster "
          << clus_ptr->get()->get_key().get_obj_key()
          << " to track "
          << trk_ptr->get()->get_key().get_obj_key()
          << endl;
      }
    }
  }

  // do the fit
  if( !trk_fit.fit() )
  {

    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    { cout << "mMutStraightFit::fit_track - fit failed" << endl; }
    return false;

  }

  // fill track object from fitter output
  fill_track( trk_ptr, trk_fit );

  // extrapolate track parameters toward vertex
  extrapolate_to_vertex( trk_ptr );

  // set track status
  if( !trk_ptr->get()->get_kalman_fail() )
  {
    trk_ptr->get()->set_status( TMutTrk::STRAIGHT_TRACK, true );
    trk_ptr->get()->set_kalman_fit();
  }

  return true;

}

//________________________________________________________
mMutStraightFit::LocalNode::LocalNode( TMutCoordMap::pointer coord_ptr, const double &z )
{

  // retrieve cluster position/error
  PHGslMatrix m( 1, 1 );
  m( 0, 0 ) = coord_ptr->get()->get_w_absolute();

  PHGslMatrix cov( 1, 1 );
  cov( 0, 0 ) = MUTOO::SQUARE( coord_ptr->get()->get_error() );

  // retrieve strip angle
  double angle( TMutGeo::get_cathode_angle(
    coord_ptr->get()->get_arm(),
    coord_ptr->get()->get_station(),
    coord_ptr->get()->get_octant(),
    coord_ptr->get()->get_half_octant(),
    coord_ptr->get()->get_gap(),
    coord_ptr->get()->get_cathode(),
    coord_ptr->get()->get_peak_strip()) );

  // transfer matrix
  PHGslMatrix h( 1, 4 );
  h( 0, 0 ) = -sin( angle );
  h( 0, 1 ) = cos( angle );
  h( 0, 2 ) = -sin(angle)*(coord_ptr->get()->get_coord_midpoint().getZ() - z);
  h( 0, 3 ) = cos(angle)*(coord_ptr->get()->get_coord_midpoint().getZ() - z);

  // set measurement to private node
  set_measurement( m, cov );

  // set transfer (projection) matrix
  set_h( h );

}

//________________________________________________________
mMutStraightFit::LocalNode::LocalNode( TMuiClusterMapO::pointer cluster_ptr, const double &z )
{

  // coordinate
  PHGslMatrix m( 1, 1 );
  m( 0, 0 ) = cluster_ptr->get()->get_w_absolute();

  PHGslMatrix cov( 1, 1 );
  cov( 0, 0 ) = MUTOO::SQUARE( cluster_ptr->get()->get_error() );

  //! transfer matrix
  double angle( TMuiGeo::get_panel_angle( cluster_ptr->get()->get_location() ) );
  PHGslMatrix h( 1, 4 );
  h(0,0) = -sin( angle );
  h(0,1) = cos( angle );
  h(0,2) = -sin( angle )*(cluster_ptr->get()->get_mean_z() - z);
  h(0,3) = cos( angle )*(cluster_ptr->get()->get_mean_z() - z);

  // set measurement to private node
  set_measurement( m, cov );

  // set transfer (projection) matrix
  set_h( h );

}

//__________________________________________________________
void mMutStraightFit::fill_track( TMutTrkMap::pointer trk_ptr, const TMutStraightTrackFit &trk_fit ) const
{

  // dump track parameters from the fit
  if( _mod_par->get_verbosity() >= MUTOO::SOME )
  {
    cout
      << "mMutStraightFit::fill_track - "
      << trk_ptr->get()->get_key().get_obj_key()
      << " ["
      << trk_fit.get_state()(0,0) << ","
      << trk_fit.get_state()(1,0) << ","
      << trk_fit.get_z() << ","
      << trk_fit.get_state()(2,0) << ","
      << trk_fit.get_state()(3,0) << "]"
      << " chi_square=" << trk_fit.get_chisquare()
      << " ndf=" << trk_fit.get_ndf()
      << " [beta version]"
      << endl;
  }

  /*
  default Pz momentum for all tracks [GeV]
  is needed to convert the TMutFitPars into TrkPar, where px, pz and pz
  are needed whereas only px/pz and py/pz are fitted.
  3.6 GeV is more or less the average value for single muons
  */
  static const double default_pz = 3.6;

  // change pz sign depending on arm
  double pz( (trk_ptr->get()->get_arm()==MUTOO::North ) ? default_pz:-default_pz );

  /*
  create TMutTrkPar from the fit output
  charge is unknown. Set to 1.
  Would be better set to 0 but a non zero charge is needed
  later for geant extrapolation
  */
  TMutTrkPar trk_par(
    trk_fit.get_state()(0,0),
    trk_fit.get_state()(1,0),
    trk_fit.get_z(),
    pz*trk_fit.get_state()(2,0),
    pz*trk_fit.get_state()(3,0),
    pz,
    1,
    trk_fit.get_chisquare() );

  // create covariance matrix
  PHGslMatrix gain( trk_fit.get_gain() );
  PHGslMatrix cov( gain.invert() );

  // transform cov matrix to have it in term of px,pz
  PHGslMatrix m( 4, 4 );
  m(0, 0) = MUTOO::SQUARE( 1 );
  m(1, 1) = MUTOO::SQUARE( 1 );
  m(2, 2) = MUTOO::SQUARE( pz ); 	// dpx/d(px/pz)
  m(3, 3) = MUTOO::SQUARE( pz );	// dpy/d(py/pz)
  cov = m*cov*m;

  // put into trk_par,
  for( int i=0; i<4; i++ )
    for( int j=0; j<4; j++ )
    trk_par.set_covar( i, j, cov( i,j ) );

  // dump track parameters from the fit
  if( _mod_par->get_verbosity() >= MUTOO::ALOT )
  {
    MUTOO::PRINT( cout, "mMutStraightFit::fill_trk - track parameters [0]" );
    trk_par.print();
  }

  // add very large pz error (100GeV squared)
  static const double cov_pz( 100 );
  trk_par.set_covar( 4, 4, cov_pz );

  // initialize track lists
  trk_ptr->get()->clear_trk_par_list();		 // clear list of trak parameters
  trk_ptr->get()->clear_w_residual_list(); // clear list of w residuals
  trk_ptr->get()->clear_r_residual_list(); // clear list of w residuals

  // loop over associated coordinates
  TMutCoordMap::key_iterator coord_iter( trk_ptr->get()->get_associated<TMutCoord>() );
  while( TMutCoordMap::pointer coord_ptr = coord_iter.next() )
  {

    // extrapolate track parameters to current z
    TMutTrkPar local_trk_par( TMutTrackUtil::linear_propagation( trk_par, coord_ptr->get()->get_coord_midpoint().getZ() ) );

    // add to track
    trk_ptr->get()->push_trk_par( local_trk_par );

    // push residuals
    push_r_residual( trk_ptr, local_trk_par, coord_ptr );
    push_w_residual( trk_ptr, local_trk_par, coord_ptr );

  }

  // loop over associated muid clusters
  TMuiClusterMapO::key_iterator clus_iter( trk_ptr->get()->get_associated<TMuiClusterO>() );
  while( TMuiClusterMapO::pointer clus_ptr =  clus_iter.next() )
  {

    // extrapolate track parameters to cluster z
    TMutTrkPar local_trk_par( TMutTrackUtil::linear_propagation( trk_par, clus_ptr->get()->get_mean_z() ) );

    // add to track
    trk_ptr->get()->push_trk_par( local_trk_par );

  }

  // fill track chi_squares
  trk_ptr->get()->set_w_chi_square( trk_fit.get_chisquare() );

  // the r chisquare is set to 0 since it makes no sense with kalman filter fit.
  trk_ptr->get()->set_r_chi_square(0);

  // fill track number of freedom (number of nodes - number of parameters)
  trk_ptr->get()->set_ndf( trk_fit.get_ndf() );

  /*
  fill track hit pattern. Calling the baseclass method
  TMutTrk::get_hit_pattern calculates the hit pattern from coord to track associations
  starting from TMutTrk_v4, this hit pattern is stored internaly and not recalculated
  */
  trk_ptr->get()->set_hit_pattern( trk_ptr->get()->TMutTrk::get_hit_pattern() );

  {
    // parse track par list to get track parameters closest to the vertex
    const vector<TMutTrkPar>& trk_par_list( *trk_ptr->get()->get_trk_par_list() );
    TMutTrkPar trk_par( trk_par_list.front() );
    for( vector<TMutTrkPar>::const_iterator iter = trk_par_list.begin(); iter != trk_par_list.end(); iter++ )
      if( fabs( iter->get_z() ) < fabs( trk_par.get_z() ) )
      trk_par = *iter;

    // set track parameters in first gap
    trk_ptr->get()->set_trk_par( trk_par );

    // dump track parameters from the fit
    if( _mod_par->get_verbosity() >= MUTOO::ALOT )
    {
      MUTOO::PRINT( cout, "mMutStraightFit::fill_trk - track parameters [0]" );
      trk_par.print();
    }
  }

}


//_____________________________________________________________________________
bool mMutStraightFit::extrapolate_to_vertex( TMutTrkMap::pointer trk_ptr ) const
{

  PHTrackIntegratorKF integrator;
  const TMutTrkPar& down_trk_par( trk_ptr->get()->get_trk_par_list()->front() );
  integrator.initialize( down_trk_par );

  // extrapolate to z_ref
  integrator.extrapolate( _vertex_z );
  if( integrator.get_error() )
  {
    if( _mod_par->get_verbosity() >= MUTOO::SOME )
    {
      cout
        << "in mMutStraightFit::extrapolate_to_vertex - extrapolation towards vertex failed ("
        << trk_ptr->get()->get_trk_par_list()->front().get_z() << "->" << _vertex_z << ")\n";
    }

    trk_ptr->get()->set_kalman_fail();
    return false;

  } else {

    TMutTrkPar extrap_trk_par;
    integrator.finish( extrap_trk_par );
    trk_ptr->get()->set_trk_par_vtx( extrap_trk_par );

    // debug output
    if( _mod_par->get_verbosity() >= MUTOO::MAX )
    {
      MUTOO::PRINT( cout, "mMutStraightFit::extrapolate_to_vertex - trk_par_vtx");
      extrap_trk_par.print();
    }

  }

  return true;

}


//__________________________________________________________
void mMutStraightFit::push_r_residual( TMutTrkMap::pointer trk_ptr, const TMutTrkPar &trk_par, TMutCoordMap::pointer coord )
{

  // stores smoothed track point
  PHPoint trk_point( trk_par.get_x(), trk_par.get_y(), trk_par.get_z() );

  // loop over associated TMutGapCoord and fill r residuals.	The check on
  // cathode is to prevent writing 2 r residuals for each gap coord.	If
  // no gap coord then we don't write a r residual (SK 6/17/03)
  if(coord->get()->get_cathode() == 0) {
    TMutGapCoordMap::key_iterator gap_coord_iter = coord->get()->get_associated<TMutGapCoord>();
    while(TMutGapCoordMap::pointer gap_ptr = gap_coord_iter.next()){
      double delta_r = TMutMeasureModel::anode_measure(trk_point, gap_ptr->get());
      trk_ptr->get()->push_r_residual(delta_r);
    }
  }

  return;
}

//__________________________________________________________
void mMutStraightFit::push_w_residual( TMutTrkMap::pointer trk_ptr, const TMutTrkPar& trk_par, TMutCoordMap::pointer coord )
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
  MUTOO::cathode_locator location = coord->get()->get_location();
  double r_trk        = TMutGeo::get_point_cathode_rdist(trk_point, location, coord->get()->get_peak_strip());
  double cos_th_wz    = PHGeometry::dot(TMutGeo::get_w_axis(location, coord->get()->get_peak_strip()),trk_tangent);
  double cos_th_r     = PHGeometry::dot(TMutGeo::get_strip_direction(location, coord->get()->get_peak_strip()),trk_tangent);
  double w_fit_error  = TMutMeasureModel::get_w_error(trk_point, trk_errors, coord->get());
  double delta_w      = TMutMeasureModel::cathode_measure( trk_point, coord->get(), false );
  double w_meas       = coord->get()->get_w();					  // measurement from mutoo
  double w_trk        = coord->get()->get_w() + delta_w;  // 'corrected' track position offset

  // calculate cluster width
  TMutClusMap::const_key_iterator clus_iter = coord->get()->get_associated<TMutClus>();
  unsigned short clus_wid = 0;
  if(!clus_iter.at_end()) clus_wid = clus_iter->get()->get_n_strip();

  trk_ptr->get()->push_w_residual(TMutTrkRes(location,
    fit_par,
    coord->get()->get_cos_theta_wire(),
    w_trk,
    w_meas,
    r_trk,
    cos_th_wz,
    cos_th_r,
    coord->get()->get_q_peak(),
    coord->get()->get_q_tot(),
    w_fit_error,
    clus_wid,
    coord->get()->get_peak_strip(),
    coord->get()->get_status())
    );
  return;
}

//__________________________________________________________
void mMutStraightFit::associate_road( TMutTrkMap::pointer trk_ptr ) const
{

  // do nothing if there are already clusters associated to the track.
  if( trk_ptr->get()->get_associated<TMuiClusterO>().count() ) return;

  // get associated roads
  TMuiRoadMapO::key_iterator road_iter( trk_ptr->get()->get_associated<TMuiRoadO>() );
  if( _mod_par->get_verbosity() >= MUTOO::ALOT ) cout << "mMutStraightFit::associate_road - n_roads=" << road_iter.count() << endl;
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

    // calculate dg0
    double dg0 = PHGeometry::distancePointToPoint(mui_point,trk_point);
    if( dg0_min < 0 || dg0 < dg0_min ) {

      if( road_ptr_min ) PHKey::disassociate( trk_ptr, road_ptr_min );

      road_ptr_min = road_ptr;
      dg0_min = dg0;
    } else PHKey::disassociate( trk_ptr, road_ptr );

  }

  // check min_road (useless)
  if( !road_ptr_min ) return;

  // get associated TMuiClusterO
  TMuiClusterMapO::key_iterator clus_iter( road_ptr_min->get()->get_associated< TMuiClusterO >() );
  while( TMuiClusterMapO::pointer clus_ptr = clus_iter.next() )
  { PHKey::associate( trk_ptr, clus_ptr ); }

  return;

}
