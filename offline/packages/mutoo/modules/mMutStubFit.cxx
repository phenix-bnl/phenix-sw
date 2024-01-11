// $Id: mMutStubFit.cxx,v 1.31 2017/07/11 16:13:14 phnxbld Exp $
#include <mMutStubFitPar.h>
#include <mMutStubFit.h>
#include <TMutNode.h>
#include <TMutGeo.h>
#include <TMutMeasureModel.h>
#include <TMutLinearModel.h>
#include <PHException.h>
#include <MUTOO.h>
#include <PHTimer.h>
#include <TMutStubMap.h>
#include <TMutClusMap.h>
#include <TMutCoordMap.h>
#include <TMutGapCoordMap.h>
#include<TMutStubFinder.h>

// PHENIX headers
//
#include <PHGeometry.h>

// STL/BOOST
//
#include <boost/array.hpp>
#include <iostream>
#include <string>
#include <list>
#include <stack>

using namespace std;

// GSL required file-scope functions 
//
int stub_fit_function_f(const gsl_vector*, void*, gsl_vector*);
int stub_fit_function_df(const gsl_vector*, void*, gsl_matrix*);
int stub_fit_function_fdf(const gsl_vector*, void*, gsl_vector*, gsl_matrix*);

/*! \ingroup modules */

//______________________________________________________________________________________________
mMutStubFit::mMutStubFit() : 
  _timer(PHTimeServer::get()->insert_new( "mMutStubFit" )), 
  _z_reference(0),
  _use_section(false),
  _arm(0),
  _station(0)
{
  MUTOO::TRACE("initializing module mMutStubFit",MUTOO::ALOT);
}

//______________________________________________________________________________________________
PHBoolean mMutStubFit::event(PHCompositeNode* top_node, unsigned short arm, unsigned short station)
{
  _use_section = true;
  _arm = arm;
  _station = station;
  
  PHBoolean out = event(top_node);
  _use_section = false;
  
  return out;
  
}

//______________________________________________________________________________________________
PHBoolean mMutStubFit::event(PHCompositeNode* top_node)
{

  _timer.get()->restart();
  
  try { 

    // Reset IOC pointers
    /*
      In _use_section mode, set_interface_pointer is not called.
      It must be called externally
    */
    if( !_use_section ) set_interface_ptrs(top_node);

    // Loop over stubs and fit 
    stub_loop();

    // Do stub ghost rejection
    if(_mod_par->get_ghost_reject()) reject_ghosts();
    
  } catch(std::exception& e) {    

    MUTOO::TRACE(e.what());
    return False;    

  }
  
  _timer.get()->stop();
  if(_mod_par->get_verbosity() >= MUTOO::ALOT) _stub_map->print();
  if(_mod_par->get_verbosity() >= MUTOO::SOME) _timer.get()->print();  

  return True;
}

//______________________________________________________________________________________________
/*! 
  Reset IOC and external interface pointers 
*/
void 
mMutStubFit::set_interface_ptrs(PHCompositeNode* top_node)
{  
  // runtime parameters
  //
  _mod_par = TMutNode<mMutStubFitPar>::find_node(top_node,"mMutStubFitPar");  
  
  // track IOC
  //
  _stub_map = TMutNode<TMutStubMap>::find_node(top_node,"TMutStubMap");
} 

//______________________________________________________________________________________________
/*!
  Fit tracks using selected fit scheme.
*/
void 
mMutStubFit::stub_loop()
{
  // Loop over all stubs or over stubs in a particular sub-section
  //
  TMutStubMap::iterator stub_iter = 
    (_use_section) ? _stub_map->get(_arm,_station) : _stub_map->range();
  
  while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
    // If station is not active then skip it
    //
    if(!_mod_par->get_station_active(stub_ptr->get()->get_station())) continue;
    fit_loop(stub_ptr);
  }   
}

//______________________________________________________________________________________________
/*!
  Fit loop handles the masking of individual hits when we are in UNBIASED mode.
  If we are in NORMAL mode we just call fit_track for the current track, 
  otherwise we sequentially mask each hit associated with the track, execute the
  fit and only write a TMutTrkRes object (residual object) for the masked hit.
*/
void 
mMutStubFit::fit_loop(TMutStubMap::pointer stub_ptr)
{
  if(_mod_par->get_residual_mode() == mMutStubFitPar::NORMAL) {

    if( _mod_par->get_use_fast_stub_fit() ) fast_fit_stub(stub_ptr);
    else fit_stub(stub_ptr);

  } else if (_mod_par->get_residual_mode() == mMutStubFitPar::UNBIASED) {
    // In UNBIASED mode we sequentially mask each hit and only write a
    // TMutTrkRes object for the masked plane.
    //
    initialize_mask_stack(stub_ptr);
    while(pop_mask_stack()){

      if( _mod_par->get_use_fast_stub_fit() ) fast_fit_stub(stub_ptr);
      else fit_stub(stub_ptr);

    };
  }
}

//______________________________________________________________________________________________
/*! 
  Build a list of TMutCoord and TMutGapCoord used to constrain the
  current track.  The (gap)coord object pointers are stored in class
  scope sortable storage. Before the TMutCoord is appended to the list
  we check that it is not masked.  Masked TMutCoord are not included in 
  the fit.
*/
bool
mMutStubFit::build_hit_list(TMutStubMap::pointer stub_ptr)
{
  // Clear class scope sortable storage
  //
  _coord_list.clear();
  _gap_coord_list.clear();
  
  // Get an iterator to the TMutCoord associated with track
  //
  TMutCoordMap::key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
    
    // If the coordinate key equals current mask then do not include in the
    // list of hits to be fit.  Update the _z_refernce to the location of 
    // the skipped hit. This only happens when the mode is UNBIASED
    //
    if(coord_ptr->get()->get_key() == _cathode_mask) {
      _z_reference = TMutGeo::get_anode_plane_position(coord_ptr->get()->get_arm(),
						       coord_ptr->get()->get_station(),
						       coord_ptr->get()->get_octant(),
						       coord_ptr->get()->get_half_octant(),
						       coord_ptr->get()->get_gap()).getZ();
      continue;    
    }    
    _coord_list.push_back(coord_ptr->get());
  }
  
  // If we are using the anode projected measurement model then build a list
  // of TMutGapCoord.
  //
  if(_mod_par->get_use_anodes()){
    TMutGapCoordMap::key_iterator gap_iter = stub_ptr->get()->get_associated<TMutGapCoord>();
    while(TMutGapCoordMap::pointer gap_ptr = gap_iter.next()){
      _gap_coord_list.push_back(gap_ptr->get());
    }
  }
  
  // Update the reference z to be gap0 of the upstream hit station, unless we
  // are in UNBIASED mode for which the z_reference is always at the z location
  // of the skipped hit.
  //
  if(_mod_par->get_residual_mode() != mMutStubFitPar::UNBIASED) {
    // Set the z reference for this track to gap 0 
    //
    _z_reference = TMutGeo::get_anode_plane_position(stub_ptr->get()->get_arm(),
						     stub_ptr->get()->get_station(),
						     stub_ptr->get()->get_octant(),
						     stub_ptr->get()->get_half_octant(),
						     0).getZ();
  }
    
  // Minimum gap coords cut
  //
  if( _mod_par->get_use_anodes() && 
      (_gap_coord_list.size() < _mod_par->get_min_n_gap_coord()) ) return false;

  // Need at least NPAR measurements to proceed with fit
  //				     
  if( _coord_list.size() + _gap_coord_list.size() < _track_model.get_n_par() ) return false;
  
  return true;
}


//______________________________________________________________________________________________
mMutStubFit::Node::Node( TMutCoord* coord_ptr, const double& z_reference ):
  _h( 1,4 ),
  _m( 1,1 ),
  _g( 1,1 )
{

  // store projection matrix from state vector to measurement 
  double angle( TMutGeo::get_cathode_angle(
    coord_ptr->get_arm(),
  	coord_ptr->get_station(),
	  coord_ptr->get_octant(),
  	coord_ptr->get_half_octant(),
  	coord_ptr->get_gap(),
  	coord_ptr->get_cathode(),
  	coord_ptr->get_peak_strip()) );
  
  // stores mean z
  double z( coord_ptr->get_mean_z() );
  
  // projection matrix from state vector to measurement
 	_h( 0, 0 ) = -sin( angle );
 	_h( 0, 1 ) = cos( angle ); 
 	_h( 0, 2 ) = -sin( angle )*( z - z_reference );
 	_h( 0, 3 ) = cos( angle )*( z - z_reference ); 

  // measurement and gain matrix
	_m( 0,0 ) = coord_ptr->get_w_absolute();
  _g( 0,0 ) = 1.0/MUTOO::SQUARE( coord_ptr->get_error());

}

//______________________________________________________________________________________________
void mMutStubFit::fast_fit_stub(TMutStubMap::pointer stub_ptr)
{  

  double dxdz_init = stub_ptr->get()->get_fit_par()->get_dxdz();
  double dydz_init = stub_ptr->get()->get_fit_par()->get_dydz();
  
  // initialize hit list
  if(!build_hit_list(stub_ptr)) return;
  
  // loop over coordinates
  // build local list of nodes
  node_list nodes;
  for( coord_list::iterator coord_iter = _coord_list.begin(); coord_iter != _coord_list.end(); coord_iter++ )
  nodes.push_back( Node( *coord_iter, _z_reference ) );
  
  /* 
    allocate left and right matrix for inversion
    left*state_vector=right
  */
  PHGslMatrix left(4,4);  
  PHGslMatrix right(4,1); 
  
  // loop over nodes, fill matrices
  for( node_iterator node = nodes.begin(); node != nodes.end(); node++ ) {
   
    /* 
      update left and right matrices using coordinate contribution
      left += H^t.G.H
      right += H^t.G.M
    */
    left  += PHGslMatrix::get_AtBC( node->_h, node->_g, node->_h );
    right += PHGslMatrix::get_AtBC( node->_h, node->_g, node->_m );
  }

  // calculate state vector
  PHGslMatrix state_vector( 4,1 );
  state_vector = left.invert()*right;
  
  // calculate chisquare and covariance matrix
  double chi_square( 0 );
  PHGslMatrix cov_inv(4,4);
  for( node_iterator node = nodes.begin(); node != nodes.end(); node++ ) {
    chi_square += MUTOO::SQUARE( ( node->_h*state_vector )(0,0) - node->_m(0,0) )*node->_g(0,0);
    cov_inv += PHGslMatrix::get_AtBC( node->_h, node->_g, node->_h );
  }  
  PHGslMatrix cov( cov_inv.invert() );
  
  // update stub finder fit parameters
  TMutFitPar local_fit_par(
    state_vector( 0, 0 ),
    state_vector( 1, 0 ),
    _z_reference,
    state_vector( 2, 0 ),
    state_vector( 3, 0 ),
    chi_square
  );
  
  local_fit_par.set_z_begin( stub_ptr->get()->get_fit_par()->get_z_end() );
  local_fit_par.set_z_end( stub_ptr->get()->get_fit_par()->get_z_end() );

  // update covariance matrix
  for(unsigned int i=0;i<4;++i)
  for(unsigned int j=0;j<4;++j)
  local_fit_par.set_covar( i, j, cov(i,j) );

  stub_ptr->get()->set_fit_par(&local_fit_par);  

  // Write residuals and 
  _track_model.initialize( stub_ptr );
  write_residuals(stub_ptr);

  // for station3 we keep the derivatives unchanged
  if (stub_ptr->get()->get_station() == MUTOO::Station3){
    TMutFitPar lcl_fit_par = *stub_ptr->get()->get_fit_par();
    lcl_fit_par.set_dxdz(dxdz_init);
    lcl_fit_par.set_dydz(dydz_init);
    stub_ptr->get()->set_fit_par(&lcl_fit_par);
  }
    
}

//______________________________________________________________________________________________
/*!  
  LSF to current list of TMutTrk & TMutGapCoord.  This routine
  encapsulates the setup and iteration of the GSL solver.
*/
void mMutStubFit::fit_stub(TMutStubMap::pointer stub_ptr)
{  
  // Append current tracks associated TMutCoord and TMutGapCoord to
  // data member lists _coord_list, _gap_coord_list.  If build_hit_list
  // returns false this is indicative of too few hits to proceed with
  // the fit hence we return immediately 
  double x3p_init = stub_ptr->get()->get_fit_par()->get_dxdz();
  double y3p_init = stub_ptr->get()->get_fit_par()->get_dydz();

  if(!build_hit_list(stub_ptr)) return;
  
  // Initialize the track model with current TMutStub  
  // 
  _track_model.initialize(stub_ptr, _z_reference);
  
  // Number of data points in fit
  //
  size_t n_data = get_number_data_points();

  // Number of fit parameters track model
  //
  static const size_t N_FIT_PAR = _track_model.get_n_par();
  
  // Fill gsl vector of fit parameters from current values in track model object
  //
  gsl_vector* fit_vector_ptr = gsl_vector_alloc(N_FIT_PAR);
  for(unsigned short ipar=0;ipar<N_FIT_PAR;++ipar){
    gsl_vector_set(fit_vector_ptr,ipar,_track_model.get_parameter(ipar));
  }
    
  // Initialize GSL fit function, stub_fit_function_f fills a vector of residuals, 
  // stub_fit_function_df fills the Jacobian matrix, stub_fit_function_fdf fills both.
  //
  gsl_multifit_function_fdf fit_func;
  fit_func.f = &stub_fit_function_f;
  fit_func.df = &stub_fit_function_df;
  fit_func.fdf = &stub_fit_function_fdf;
  fit_func.n = n_data;
  fit_func.p = N_FIT_PAR;


  // Put all the other stuff the fitter needs into the FitData struct. 
  // (FitData is a private class scope struct defined in mMutStubFit.h) 
  // This gets passed to the fit routines by the GSL minimizer as a void* 
  // 
  FitData fit_data = {
    stub_ptr->get(), 
    _coord_list, 
    _gap_coord_list, 
    &_track_model,
    _mod_par->get_use_anodes()
  };

  fit_func.params = &fit_data;
  
  // Initialized a Levenberg-Marquardt solver for
  // n_data data points and N_FIT_PAR fit parameters.
  // See GSL manual "Non-Linear Least Squares Fitting"
  //
  const gsl_multifit_fdfsolver_type* solver_type_ptr;
  gsl_multifit_fdfsolver* solver_ptr;
  solver_type_ptr = gsl_multifit_fdfsolver_lmsder;
  solver_ptr = gsl_multifit_fdfsolver_alloc(solver_type_ptr,
					    n_data,
					    N_FIT_PAR);
  gsl_multifit_fdfsolver_set(solver_ptr,
			     &fit_func,
			     fit_vector_ptr);
    
  if(_mod_par->get_verbosity() == MUTOO::ALOT) MUTOO::PRINT(std::cout,"Stub Fit");
    
    // Iterate until we reach max iterations or the fit converges
    //
  int status=0;
  int iter=0;
  double epsabs = _mod_par->get_epsabs();
  do {
    status = gsl_multifit_fdfsolver_iterate(solver_ptr);  
    if(_mod_par->get_verbosity() == MUTOO::ALOT) {
      print_solver(solver_ptr);
    }
      
    if(status) break;
      
    status = gsl_multifit_test_delta(solver_ptr->dx, 
				     solver_ptr->x,
				     epsabs,
				     0);
    ++iter;
      
  } while(status == GSL_CONTINUE && iter<_mod_par->get_max_iterations());
  
  if(_mod_par->get_verbosity() == MUTOO::ALOT) MUTOO::PRINT(std::cout,"End Stub Fit");
  
  // Update Stub Model with best fit parameters
  //
  _track_model.update(solver_ptr);
  
  // Update track model with covariance matrix
  //
  _track_model.set_covariance(solver_ptr);

  // Update the TMutStub
  //
  double z = TMutGeo::get_anode_plane_position(stub_ptr->get()->get_arm(),
					       stub_ptr->get()->get_station(),
					       stub_ptr->get()->get_octant(),
					       stub_ptr->get()->get_half_octant(),
					       MUTOO::Gap1).getZ();
  _track_model.finish(stub_ptr, z);

  // Write residuals and chi_square to TMutStub
  //
  write_residuals(stub_ptr);

  if (stub_ptr->get()->get_station() == 2){
    TMutFitPar lcl_fit_par = *stub_ptr->get()->get_fit_par();
    lcl_fit_par.set_dxdz(x3p_init);
    lcl_fit_par.set_dydz(y3p_init);
    stub_ptr->get()->set_fit_par(&lcl_fit_par);
  }

  // cleanup
  //
  gsl_vector_free(fit_vector_ptr);
  gsl_multifit_fdfsolver_free(solver_ptr);
}

//______________________________________________________________________________________________
void 
mMutStubFit::write_residuals(TMutStubMap::pointer stub_ptr)
{  
  //   Loop over coords [
  //     Extrapolate track parameters to z of current coordinate
  //     Calculate distance from track to coord
  //     Form residual from distance and sigma
  //     Gather info pertinent to resolution
  //     Push TMutTrkRes (residual) object on TMutTrk residual list
  //   ]
  //   update TMutTrk object with chi_square
  
  // Define local typedefs for readibility
  // 
  typedef TMutCoordMap::key_iterator coord_iterator;  
  double chi_sqr_w=0;
  double chi_sqr_r=0;
  unsigned short cathode_mask_plane = 999;
  coord_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){          

    
    // Extrapolate track parameters to current z 
    //
    double z = coord_ptr->get()->get_coord().getBasepoint().getZ();
    const PHPoint trk_point = _track_model.extrapolate_point(z);
    
    // Calculate w pull
    //
    double delta_w =  TMutMeasureModel::cathode_measure(trk_point, coord_ptr->get(), _mod_par->get_use_anodes());  
    
    // Get pull (residual/sigma)
    //
//    double pull = delta_w/mMutStubFit::get_w_error();
    double pull = delta_w/coord_ptr->get()->get_error();
    
    if(_mod_par->get_residual_mode() == mMutStubFitPar::UNBIASED &&
       !(coord_ptr->get()->get_key() == _cathode_mask)) chi_sqr_w+=MUTOO::SQUARE(pull);
    
    // Stub offset is TMutCoord offset + residual
    //
    double w_trk = coord_ptr->get()->get_w() + delta_w;
    
    // Temp for readibility
    //
    const MUTOO::cathode_locator location = coord_ptr->get()->get_location(); 
    
    // Project track point onto axis of peak strip and calculated distance from
    // projection to end of strip
    //
    double r_trk = TMutGeo::get_point_cathode_rdist(trk_point,
						    location,
						    coord_ptr->get()->get_peak_strip());
    
    // Fit parameters extrapolated to plane of current coordinate
    //
    TMutFitPar local_fit_par(trk_point.getX(),
			     trk_point.getY(),
			     trk_point.getZ(),
			     stub_ptr->get()->get_fit_par()->get_dxdz(),
			     stub_ptr->get()->get_fit_par()->get_dydz());
    
    // Cosine of angle between tangent to track and w_axis
    //
    double cos_th_wz = PHGeometry::dot(
			TMutGeo::get_w_axis(
				location,coord_ptr->get()->get_peak_strip()),
				local_fit_par.get_tangent( stub_ptr->get()->get_arm() ) );

    // Cosine of angle between tangent to track and strip
    //
    double cos_th_r = PHGeometry::dot(
			TMutGeo::get_strip_direction(
				location,coord_ptr->get()->get_peak_strip()),
				local_fit_par.get_tangent( stub_ptr->get()->get_arm() ) );


    // Propagate fit error to w (ie error_w = sqrt ( (dM/dx error_x)^2 + dM/dy (error_y)^2 )
    //
    PHPoint errors(stub_ptr->get()->get_fit_par()->get_covar(0,0),
		   stub_ptr->get()->get_fit_par()->get_covar(1,1),
		   0);		   

    double w_fit_error = TMutMeasureModel::get_w_error(trk_point,errors,coord_ptr->get());

    // Get the width of the underlying cluster
    //
    TMutClusMap::const_key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
    unsigned short clus_wid = 0;
    if(!clus_iter.at_end()) clus_wid = clus_iter->get()->get_n_strip();
    
    // Gather all the relevant info, construct a TMutTrkRes object and
    // append to TMutStub residual list
    //
    // If the coordinate key doesn't equal the current mask then skip it (UNBIASED mode)
    // In UNBIASED mode we only write a residual for hits that were *excluded* from the
    // fit. 
    //
    if(_mod_par->get_residual_mode() == mMutStubFitPar::UNBIASED &&
       !(coord_ptr->get()->get_key() == _cathode_mask)){
      continue;    
    }
    else if(_mod_par->get_residual_mode() == mMutStubFitPar::UNBIASED &&
             coord_ptr->get()->get_key() == _cathode_mask){
      cathode_mask_plane = 2*coord_ptr->get()->get_gap() + coord_ptr->get()->get_cathode();
    }

    stub_ptr->get()->push_w_residual(TMutTrkRes(location,
						local_fit_par,
						coord_ptr->get()->get_cos_theta_wire(),
						w_trk,
						coord_ptr->get()->get_w(),
						r_trk,
						cos_th_wz,
						cos_th_r,
						coord_ptr->get()->get_q_peak(),
						coord_ptr->get()->get_q_tot(),
						w_fit_error,
						clus_wid,
						coord_ptr->get()->get_peak_strip(),
						coord_ptr->get()->get_status()));
  }

  // Get an iterator to the TMutGapCoords associated with this stub
  //
  typedef TMutGapCoordMap::key_iterator gap_coord_iterator;  
  gap_coord_iterator gap_coord_iter = stub_ptr->get()->get_associated<TMutGapCoord>();
  
  while(TMutGapCoordMap::pointer gap_ptr = gap_coord_iter.next()){
    
    // Extrapolate to the z coord of the gap coord
    //
    double z = gap_ptr->get()->get_coord().getZ();    
    PHPoint trk_point = _track_model.extrapolate_point(z);

    // Calculate the distance for the track to the anode
    //
    double delta_r = TMutMeasureModel::anode_measure(trk_point, gap_ptr->get());      
    
    // Append r residual to TMutTrk r residual vector and update chi square
    //
    // Get error on coordinates going into gap coordinate and update error
    // accordingly
    typedef TMutCoordMap::key_iterator coord_iterator_g;
    coord_iterator_g coord_iter_g = gap_ptr->get()->get_associated<TMutCoord>();

    unsigned short icoord = 0;
    //    double slope[2] = {0};
    //    double coord_error[2] = {0};
    //    double r_error = 0;
    while(TMutCoordMap::pointer coord_ptr_g = coord_iter_g.next()){
      PHPoint begin = coord_ptr_g->get()->get_coord_begin();
      PHPoint end = coord_ptr_g->get()->get_coord_end();
      //      slope[icoord] = (begin.getX()-end.getX())/(begin.getY()-end.getY());
      //      coord_error[icoord] = coord_ptr_g->get()->get_error();
      icoord++;
    }
    //double denom = tan(atan(slope[0]) - atan(slope[1]));
    //    if (denom != 0){
      // r_error = MUTOO::SQUARE(1/denom)*
      //           (MUTOO::SQUARE(coord_error[0]) +
      //            MUTOO::SQUARE(coord_error[1]));
    //    }

    // Get pull (residual/sigma)
    //
    if(_mod_par->get_residual_mode() == mMutStubFitPar::UNBIASED &&
       gap_ptr->get()->get_gap() != 1) chi_sqr_r += MUTOO::SQUARE(delta_r/get_r_error());
//    chi_sqr_r += MUTOO::SQUARE(delta_r)/(MUTOO::SQUARE(mMutStubFit::get_r_error()) + r_error);
    stub_ptr->get()->push_r_residual(delta_r);
  }
  
  // Update chi square 
  //
  if(_mod_par->get_residual_mode() == mMutStubFitPar::NORMAL) {
    
    stub_ptr->get()->set_w_chi_square(chi_sqr_w);
    stub_ptr->get()->set_r_chi_square(chi_sqr_r);
  
  } else {

//     // If mode is UNBIASED the we append to the chi-square
//     // (only one chi-square increment per fit)
//     double last_w = stub_ptr->get()->get_w_chi_square();
//     double last_r = stub_ptr->get()->get_r_chi_square();

    // To study resolution, don't want to include in chi-square the plane that
    // you want to study.  Choose for now to store the chi-square that has
    // the straight plane in the middle gap at stations 1 and 2 removed:
    if ((stub_ptr->get()->get_station()==1 && cathode_mask_plane == 2) ||
        (stub_ptr->get()->get_station()!=1 && cathode_mask_plane == 3)){
      stub_ptr->get()->set_w_chi_square(chi_sqr_w);
      stub_ptr->get()->set_r_chi_square(chi_sqr_r);
    }
  }
}

//______________________________________________________________________________________________
/*!  
  This routine calculates a vector of residuals for selected track
  model.  The measurements are the list of TMutCoord/TMutGapCoord 
  objects associated with the track.  
*/
int 
stub_fit_function_f(const gsl_vector* fit_par, 
	       void* parameters, 
	       gsl_vector* residuals)
{  
  // GSL -- void* generic interface, cast back to FitData struct.  
  //
  mMutStubFit::FitData* fit_data_ptr = reinterpret_cast<mMutStubFit::FitData*>(parameters);

  // Define some locals for readibility.
  //
  mMutStubFit::coord_list& coord_list = fit_data_ptr->coord_list_ref;
  mMutStubFit::gap_coord_list& gap_coord_list = fit_data_ptr->gap_coord_list_ref;
  TMutTrackModel* track_model = fit_data_ptr->track_model_ptr;
  bool use_anodes = fit_data_ptr->use_anodes;

  // Update the track model with current fit parameters
  //
  track_model->update(fit_par);

  //   Calculation of cathode pulls (w residuals)
  //
  //   Loop over coords [
  //     - Extrapolate track pars to z of current coordinate
  //     - Calculate w pull (w_model - w_measured)/sigma
  //     - Append pull to residual vector
  //   ]

  int imeasure=0;
  mMutStubFit::coord_list::iterator coord_iter = coord_list.begin();
  for(;coord_iter!=coord_list.end();++coord_iter) {

    // Extrapolate track parameters to z if current TMutCoord
    //
    double z = (*coord_iter)->get_coord().getBasepoint().getZ();
    PHPoint trk_point = track_model->extrapolate_point(z);

    // Calculate w pull.  Note if the use_anodes flag is false then we use the
    // non-anode projected measurment model.  That is we simply minimize the
    // DCA between the track and the TMutCoord.
    //
    double delta_w =  TMutMeasureModel::cathode_measure(trk_point, *coord_iter, use_anodes);  

    // Get pull (residual/sigma)
    //
//    double pull = delta_w/mMutStubFit::get_w_error();
    double pull = delta_w/(*coord_iter)->get_error();

    // Push current pull onto GSL residual vector
    //
    gsl_vector_set(residuals,imeasure++,pull);
  } 

  //   Calculation of anode pulls (r residuals)
  //   Loop over coords [
  //     - Extrapolate track pars to z of current coordinate
  //     - Calculate r pull (r_anode - r_gap_coord)/sigma
  //     - Append pull to residual vector
  //   ]

  mMutStubFit::gap_coord_list::iterator gap_iter = gap_coord_list.begin();
  for(;gap_iter!=gap_coord_list.end();++gap_iter){
    // Extrapolate track parameters to z of current TMutGapCoord
    //
    double z = (*gap_iter)->get_coord().getZ();    
    PHPoint trk_point = track_model->extrapolate_point(z);

    // Calculate r pull
    //
    double delta_r = TMutMeasureModel::anode_measure(trk_point, *gap_iter);  
    
    // Get error on coordinates going into gap coordinate and update error
    // accordingly
    typedef TMutCoordMap::key_iterator coord_iterator_g;
    coord_iterator_g coord_iter_g = (*gap_iter)->get_associated<TMutCoord>();

    unsigned short icoord = 0;
    // double slope[2] = {0};
    // double coord_error[2] = {0};
    //    double r_error = 0;
    while(TMutCoordMap::pointer coord_ptr_g = coord_iter_g.next()){
      PHPoint begin = coord_ptr_g->get()->get_coord_begin();
      PHPoint end = coord_ptr_g->get()->get_coord_end();
      // slope[icoord] = (begin.getX()-end.getX())/(begin.getY()-end.getY());
      // coord_error[icoord] = coord_ptr_g->get()->get_error();
      icoord++;
    }
    // double denom = tan(atan(slope[0]) - atan(slope[1]));
    // if (denom != 0){
    //   r_error = MUTOO::SQUARE(1/denom)*
    //             (MUTOO::SQUARE(coord_error[0]) +
    //              MUTOO::SQUARE(coord_error[1]));
    // }

    // Get pull (residual/sigma)
    //
//    double pull = delta_r/sqrt(MUTOO::SQUARE(mMutStubFit::get_r_error())
//                                 + r_error);
    
  double pull = delta_r/mMutStubFit::get_r_error();
    
    // Push current pull onto GSL residual vector
    //
    gsl_vector_set(residuals,imeasure++,pull);
  }

  return GSL_SUCCESS;
}


//______________________________________________________________________________________________
/*!
  This routine calculates the Jacobian of M(T(z)) for each TMutCoord/TMutGapCoord
  in track. M is the measurement model, T is the track model.
*/
int
stub_fit_function_df(const gsl_vector* fit_par, 
		void* parameters, 
		gsl_matrix* jacobian)
{
  // GSL -- void* generic interface, cast back to FitData struct.  
  //
  mMutStubFit::FitData* fit_data_ptr = reinterpret_cast<mMutStubFit::FitData*>(parameters);

  // Define some locals for readibility.
  //
  mMutStubFit::coord_list& coord_list = fit_data_ptr->coord_list_ref;
  mMutStubFit::gap_coord_list& gap_coord_list = fit_data_ptr->gap_coord_list_ref;
  TMutTrackModel* track_model = fit_data_ptr->track_model_ptr;
  bool use_anodes = fit_data_ptr->use_anodes;

  // Update the track model with current fit parameters
  //
  track_model->update(fit_par);

  // Loop over coords [
  //   Calculate contribute to jacobian
  // ]
  int imeasure=0;
  mMutStubFit::coord_list::iterator coord_iter = coord_list.begin();
  for(;coord_iter!=coord_list.end();++coord_iter) {

    // Extrapolate track parameters to z if current TMutCoord
    //
    double z = (*coord_iter)->get_coord().getBasepoint().getZ();
    PHPoint trk_point = track_model->extrapolate_point(z);
  
    // Measurement model derivates with respect to extrapolated track
    //
    double dm_dx = TMutMeasureModel::d_cathode_measure_dx(trk_point,*coord_iter,use_anodes);
    double dm_dy = TMutMeasureModel::d_cathode_measure_dy(trk_point,*coord_iter,use_anodes);

    // Stub model derivatives 
    // 
    track_model->calculate_jacobian(z);
    const TMutTrackModel::value_array& dt_dx = track_model->jacobian(0);
    const TMutTrackModel::value_array& dt_dy = track_model->jacobian(1);		   
    
    // Chain rule (dChi/dpar = dM/dx dx/dpar + dM/dy dy/dpar)
    //
    for(unsigned short ipar=0; ipar<dt_dx.size(); ++ipar){
//      double dchi_dpar = (dm_dx*dt_dx[ipar] + dm_dy*dt_dy[ipar])/mMutStubFit::get_w_error();
      double dchi_dpar = (dm_dx*dt_dx[ipar] + dm_dy*dt_dy[ipar])/
//	                    mMutStubFit::get_w_error(); 
	                    (*coord_iter)->get_error();
      gsl_matrix_set(jacobian,imeasure,ipar,dchi_dpar);
    }
    ++imeasure;
  }

  // Loop over gap coords [
  //   Calculate contribute to jacobian
  // ]

  mMutStubFit::gap_coord_list::iterator gap_coord_iter = gap_coord_list.begin();
  for(;gap_coord_iter!=gap_coord_list.end();++gap_coord_iter) {

    // Extrapolate track parameters to z if current TMutCoord
    //
    double z = (*gap_coord_iter)->get_coord().getZ();
    PHPoint trk_point = track_model->extrapolate_point(z);
  
    // Measurement model derivates with respect to extrapolated track
    //
    double dm_dx = TMutMeasureModel::d_anode_measure_dx(trk_point,*gap_coord_iter);
    double dm_dy = TMutMeasureModel::d_anode_measure_dy(trk_point,*gap_coord_iter);

    // Stub model derivatives 
    // 
    track_model->calculate_jacobian(z);
    const TMutTrackModel::value_array& dt_dx = track_model->jacobian(0);
    const TMutTrackModel::value_array& dt_dy = track_model->jacobian(1);		   

    // Get error on coordinates going into gap coordinate and update error
    // accordingly
    typedef TMutCoordMap::key_iterator coord_iterator_g;
    coord_iterator_g coord_iter_g = (*gap_coord_iter)->get_associated<TMutCoord>();

    unsigned short icoord = 0;
    // double slope[2] = {0};
    // double coord_error[2] = {0};
    //    double r_error = 0;
    while(TMutCoordMap::pointer coord_ptr_g = coord_iter_g.next()){
      PHPoint begin = coord_ptr_g->get()->get_coord_begin();
      PHPoint end = coord_ptr_g->get()->get_coord_end();
      // slope[icoord] = (begin.getX()-end.getX())/(begin.getY()-end.getY());
      // coord_error[icoord] = coord_ptr_g->get()->get_error();
      icoord++;
    }
    // double denom = tan(atan(slope[0]) - atan(slope[1]));
    // if (denom != 0){
    //   r_error = MUTOO::SQUARE(1/denom)*
    //             (MUTOO::SQUARE(coord_error[0]) +
    //              MUTOO::SQUARE(coord_error[1]));
    // }

    // Chain rule (dChi/dpar = dM/dx dx/dpar + dM/dy dy/dpar)
    //
    for(unsigned short ipar=0; ipar<dt_dx.size(); ++ipar){
      double dchi_dpar = (dm_dx*dt_dx[ipar] + dm_dy*dt_dy[ipar])/mMutStubFit::get_r_error();
//      double dchi_dpar = (dm_dx*dt_dx[ipar] + dm_dy*dt_dy[ipar])/
//	      sqrt(MUTOO::SQUARE(mMutStubFit::get_r_error())+ r_error);
      gsl_matrix_set(jacobian,imeasure,ipar,dchi_dpar);
    }
    ++imeasure;
  }

  return GSL_SUCCESS;
}


//______________________________________________________________________________________________
int
stub_fit_function_fdf(const gsl_vector* fit_par, 
		 void* parameters,
		 gsl_vector* residuals,
		 gsl_matrix* jacobian)
{
  // This should be reimplemented as a single function call.
  // (This I believe is the point of requiring the _fdf func)
  //
  stub_fit_function_f(fit_par,parameters,residuals);
  stub_fit_function_df(fit_par,parameters,jacobian);
  return GSL_SUCCESS;
}


//______________________________________________________________________________________________
unsigned short
mMutStubFit::get_number_data_points() const 
{
  return  _coord_list.size() + _gap_coord_list.size();
}

//______________________________________________________________________________________________
void
mMutStubFit::print_solver(const gsl_multifit_fdfsolver* solver_ptr, std::ostream& os) const
{
  // fit parameters
  //
  os << setw(10) << setprecision(5) << setiosflags(ios::fixed);
  os << " x:" << gsl_vector_get(solver_ptr->x,0);
  os << " y:" << gsl_vector_get(solver_ptr->x,1);
  os << " dxdz:" << gsl_vector_get(solver_ptr->x,2);
  os << " dydz:" << gsl_vector_get(solver_ptr->x,3);  
  os << " chi_square:" << gsl_blas_dnrm2(solver_ptr->f) << std::endl;
}

//______________________________________________________________________________________________
bool
mMutStubFit::is_station_active(unsigned short station)
{
  return _mod_par->get_station_active(station);
}

//______________________________________________________________________________________________
void 
mMutStubFit::initialize_mask_stack(TMutStubMap::const_pointer stub_ptr)
{  
  // Stack should be empty
  //
  if(!_mask_stack.empty()) {
    while(!_mask_stack.empty()) _mask_stack.pop();
    throw std::runtime_error(DESCRIPTION("mask stack not empty at initialization"));
  }
    
  // Intialize the cathode mask stack by pushing the cathode location
  // of each hit onto the stack. 
  //
  TMutCoordMap::const_key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()) {    
    // Hit in masked stations are already masked so they don't
    // go into the mask stack.
    //
    if(!is_station_active(coord_ptr->get()->get_station())) continue;
    _mask_stack.push(coord_ptr->get()->get_key());
  }
}

//______________________________________________________________________________________________
bool
mMutStubFit::pop_mask_stack()
{
  // if mask empty [
  //   return false
  // ] else [
  //   set current mask
  //   pop the mask stack
  // ]
  if(_mask_stack.empty()){
    return false;
  } else {
    _cathode_mask = _mask_stack.top();
    _mask_stack.pop();
    return true;
  }
}

//______________________________________________________________________________________________
void 
mMutStubFit::write_stub_pars(TMutStubMap::pointer stub_ptr) {
  // Loop over stations
  //  if station is active [
  //    copy TMutFitPar from TMutTrk into stub par data member in TMutTrk
  //  ]
  // ]
  for(int station=0; station<MUTOO::NumberOfStations;++station){
    if(is_station_active(station)){
      // Stub fit reference surface is z of first gap in station
      //
      double z_ref = TMutGeo::get_anode_plane_position(stub_ptr->get()->get_arm(),
						       station,
						       stub_ptr->get()->get_octant(),
						       0,
						       0).getZ();

      unsigned short last_gap = TMutGeo::get_n_gaps(stub_ptr->get()->get_arm(),station)-1;
      
      // Stub endpoint is z of last gap in station
      //
      double z_end = TMutGeo::get_anode_plane_position(stub_ptr->get()->get_arm(),
						       station,
						       stub_ptr->get()->get_octant(),
						       0,
						       last_gap).getZ();
      // Update TMutTrk fit/track parameters
      //
      _track_model.finish(stub_ptr,z_ref);
      
      TMutFitPar lcl_fit_par = *stub_ptr->get()->get_fit_par();
      lcl_fit_par.set_z_begin(z_ref);
      lcl_fit_par.set_z_end(z_end);

      // Update TMutTrk stub parameters
      //
      stub_ptr->get()->set_fit_par(&lcl_fit_par);
    }
  }
}

//______________________________________________________________________________________________
void
mMutStubFit::reject_ghosts(){  

  // Loop over octant
  //  Build local lists of stubs (3||4 hits & 5||6 hits)
  //   Loop over local lists
  //    if(3||4) reject based upon delta stub vector
  //    if(5||6) reject based upon chi_sq
  //   ]
  //  ]
  // ]
  for(int arm=0; arm<MUTOO::NumberOfArms;++arm){
    for(int sta=0; sta<MUTOO::NumberOfStations;++sta){
      for(int oct=0; oct<MUTOO::NumberOfOctants;++oct){	

	// Only reject in requested section 
	//
	if(_use_section && (arm!=_arm || sta!=_station)) continue;

	// Local sortable storage
	//
	typedef std::list<TMutStubMap::value_type> local_list_type;
	local_list_type local_34;
	local_list_type local_56;

	// Build local lists
	//
	TMutStubMap::iterator stub_iter = _stub_map->get(arm,sta,oct);
	while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
	  unsigned short ncoord = stub_ptr->get()->get_associated<TMutCoord>().count();
	  unsigned short ngap = stub_ptr->get()->get_associated<TMutGapCoord>().count();
	  if(ncoord==3||ncoord==4) local_34.push_back(*stub_ptr);
	  if((ncoord==5||ncoord==6) && ngap>1) local_56.push_back(*stub_ptr);
	}

	// Stubs with 3 or 4 coords are rejected based upon delta stub vector
	//
	local_34.sort(stub_less_ftor());
	local_34.unique(stub_equal_ftor());

	// Now erase stubs that are not in the unique list	
	//
	stub_iter.reset();
	while(TMutStubMap::pointer stub_ptr = stub_iter.next()){
	  unsigned short ncoord = stub_ptr->get()->get_associated<TMutCoord>().count();
	  if(ncoord==3||ncoord==4){
	    local_list_type::iterator lcl_iter = std::find_if(local_34.begin(), local_34.end(), 
							      stub_unary_equal_ftor(stub_ptr->get()->get_key().get_obj_key()) );
	    if(lcl_iter == local_34.end()) _stub_map->erase(stub_ptr->get()->get_key());
	  }
	}
	
	// Stubs with 5 or 6 coords with n or more common coords are rejeted based upon chi_square
	//
	if(local_56.size() < 2) continue;
	local_list_type::iterator lcl_iter1 = local_56.begin(); 
	for(;lcl_iter1 != local_56.end(); ++lcl_iter1){
	  local_list_type::iterator lcl_iter2 = lcl_iter1; ++lcl_iter2;
	  for(;lcl_iter2 != local_56.end(); ++lcl_iter2){
	    std::pair<bool,TMutStubMap::value_type> reject = reject_ghost_chi_square(*lcl_iter1, *lcl_iter2);
	    if(reject.first) _stub_map->erase(reject.second.get()->get_key());
	  }
	}	
      }      
    }
  }  
}

//______________________________________________________________________________________________
std::pair<bool,TMutStubMap::value_type>
mMutStubFit::reject_ghost_chi_square(TMutStubMap::value_type stub1, TMutStubMap::value_type stub2)
{

//   // Count the number of coordinates the two stubs have in common
//   //
//   unsigned short n_overlap=0;
//   TMutCoordMap::key_iterator coord_iter1 = stub1.get()->get_associated<TMutCoord>();
//   while(TMutCoordMap::pointer coord_ptr1 = coord_iter1.next()){
//     TMutCoordMap::key_iterator coord_iter2 = stub2.get()->get_associated<TMutCoord>();
//     while(TMutCoordMap::pointer coord_ptr2 = coord_iter2.next()){
//       if(coord_ptr1->get()->get_key() == coord_ptr2->get()->get_key()) ++n_overlap;
//     }
//   }
  
  // Count the number of coordinates the two stubs have in common
  unsigned short n_overlap=0;
  TMutCoordMap::key_iterator coord_iter1 = stub1.get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr1 = coord_iter1.next())
  if( coord_ptr1->get()->is_associated<TMutStub>( &stub2 ) ) ++n_overlap;

  //  Add protect against using stubs which did not get fit:
  if(n_overlap >= _mod_par->get_common_coord_threshold() &&
     stub1.get()->get_chi_pdf() > 0 &&
     stub2.get()->get_chi_pdf() > 0)
    return std::make_pair(true,
			  ( stub1.get()->get_chi_pdf() < stub2.get()->get_chi_pdf() ) ?
			  stub2 : stub1 );
  else return std::make_pair(false,stub1);

}

//______________________________________________________________________________________________
double
mMutStubFit::get_w_error() 
{
  static double err = 0.05;
  return err;
}

//______________________________________________________________________________________________
double
mMutStubFit::get_r_error() 
{
  static double err = 0.5/std::sqrt(12.0);
  return err;
}

//______________________________________________________________________________________________
void
mMutStubFit::print_summary()
{
  MUTOO::PRINT(std::cout,"mMutStubFit::print_summary");
  TMutStubMap::const_iterator stub_iter = _stub_map->range();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()){
    std::cout << "stub summary: key: " << stub_ptr->get()->get_key().get_obj_key() 
	      << " chi: " << stub_ptr->get()->get_chi_pdf() << " arm: " 
	      << stub_ptr->get()->get_arm() << " sta: " << stub_ptr->get()->get_station() 
	      << " oct: " << stub_ptr->get()->get_octant(); 
    std::cout << " coords: ";
    TMutCoordMap::const_key_iterator coord_iter = stub_ptr->get()->get_associated<TMutCoord>();
    while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
      std::cout << coord_ptr->get()->get_key().get_obj_key() << " "; 
    }
    std::cout << std::endl;
  }
  MUTOO::PRINT(std::cout,"**");
}



