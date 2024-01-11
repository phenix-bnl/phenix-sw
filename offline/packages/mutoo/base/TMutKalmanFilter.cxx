// $Id: TMutKalmanFilter.cxx,v 1.1 2007/11/18 13:26:28 hpereira Exp $

/*!
	\file TMutKalmanFilter.cxx
	\brief general kalman filter methods
	\author Hugo Pereira
	\version $Revision: 1.1 $
	\date $Date: 2007/11/18 13:26:28 $
*/

#include "TMutKalmanFilter.h"
#include "PHTrackIntegratorKF.h"

using namespace std;
	
//__________________________________________________________
PHGslMatrix TMutKalmanFilter::Node::get_predicted_residual( void ) const
{

	if( !_prediction_done ) 
	throw std::logic_error( DESCRIPTION( "no prediction.\n" ) );
	
	PHGslMatrix out( _measurement );
	out -= _h*_predicted._state_kf;
	return out;

}
	
//__________________________________________________________
PHGslMatrix TMutKalmanFilter::Node::get_predicted_residual_cov( void ) const
{

	if( !_prediction_done ) 
	throw std::logic_error( DESCRIPTION( "no prediction.\n" ) );
	
	PHGslMatrix out( _measurement_cov );
	out -= PHGslMatrix::get_ABCt( _h, _predicted._covar_kf, _h );
	return out;

}
		
//__________________________________________________________
PHGslMatrix TMutKalmanFilter::Node::get_filtered_residual( void ) const
{

	if( !_filter_done ) 
	throw std::logic_error( DESCRIPTION( "no filtering.\n" ) );
	
	PHGslMatrix out( _measurement );
	out -= _h*_filtered._state_kf;
	return out;

}
	
//__________________________________________________________
PHGslMatrix TMutKalmanFilter::Node::get_filtered_residual_cov( void ) const
{

	if( !_filter_done ) 
	throw std::logic_error( DESCRIPTION( "no filtering.\n" ) );
	
	PHGslMatrix out( _measurement_cov );
	out -= PHGslMatrix::get_ABCt( _h, _filtered._covar_kf, _h );
	return out;

}

//__________________________________________________________
PHGslMatrix TMutKalmanFilter::Node::get_smoothed_residual( void ) const
{

	if( !_smooth_done ) 
	throw std::logic_error( DESCRIPTION( "no smoothing.\n" ) );
	
	PHGslMatrix out( _measurement );
	out -= _h*_smoothed._state_kf;
	return out;

}
	
//__________________________________________________________
PHGslMatrix TMutKalmanFilter::Node::get_smoothed_residual_cov( void ) const
{

	if( !_smooth_done ) 
	throw std::logic_error( DESCRIPTION( "no smoothing.\n" ) );
	
	PHGslMatrix out( _measurement_cov );
	out -= PHGslMatrix::get_ABCt( _h, _smoothed._covar_kf, _h );
	return out;

}

//__________________________________________________________________
bool TMutKalmanFilter::predict( Node& node )
{
	
	// check if prediction has already been done
	if( node.prediction_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::predict - prediction already done.\n";
		return true;
	}
	
	// stores node position along the beam localy
	double z_pred = node.get_z();
	
	// make prediction
	PHTrackIntegratorKF integrator; 
	integrator.set_kf( 
		_running_trk_par._state_kf, 
		_running_trk_par._covar_kf, 
		_running_trk_par._z, 
		_running_trk_par._direction );

	
	integrator.extrapolate( z_pred );

	if( _verbosity >= MUTOO::MAX )
	cerr 
		<< "TMutKalmanFilter::predict - " 
		<< _running_trk_par._z << "->" 
		<< z_pred << " cm" << endl;
	
	if( integrator.get_error() ) {
		
		if( _verbosity >= MUTOO::ALOT ) {
			cerr 
				<< "TMutKalmanFilter::predict - extrapolation failed "
				<< " (" << _running_trk_par._z << "->" << z_pred << ")" << endl;
		}
		
		return false;
	}
	
	// get extrapolated result into predicted track parameters
	node.get_predicted()._state_kf = integrator.get_state_vector_kf();
	node.get_predicted()._covar_kf = integrator.get_covar_kf();
	node.get_predicted()._z = z_pred;
	node.get_predicted()._direction = integrator.get_direction();
		
	// store predicted state vector and propagator derivatives
	node.get_dout_din() = integrator.get_dout_din_kf();
	node.set_prediction_done( true );
		
	// dump, if required
	if( _verbosity >= MUTOO::ALOT ) {  
		MUTOO::PRINT( cout, "TMutKalmanFilter::filter - predicted");
		node.get_predicted().print( cerr );
	}    
		
	return true;
	
}

//__________________________________________________________________
bool TMutKalmanFilter::filter( Node& node )
{
	
	// perform filtering (no update)
	if( !const_filter( node ) ) return false;
		
	// update running parameters (for next prediction) if required
	set_running_trk_par( node );
	
	return true;

}

//__________________________________________________________________
bool TMutKalmanFilter::const_filter( Node& node ) const
{
	
	// check if filtering has already been done
	if( node.filter_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::const_filter - node already filtered.\n";
		return true;
	}
	
	// check if prediction is available
	if( !node.prediction_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::const_filter - node prediction.\n";
		return false;
	}
	
	// calculate filtered state vector
	// stores predicted state vector and covariance matrix
	const PHGslMatrix &p_pred( node.get_predicted()._state_kf );
	const PHGslMatrix &c_pred( node.get_predicted()._covar_kf );
	const PHGslMatrix &m( node.get_measurement() );  
	const PHGslMatrix &m_cov( node.get_measurement_cov() );
	const PHGslMatrix &h( node.get_h() );
	
	// calculate filtered covariance matrix 
	// c_filter = ( c_pred^{-1} + h^t*G*h )^{-1}
	PHGslMatrix c_pred_inv( c_pred.invert() ); // inverse of the predicted covbariance matrix.
	PHGslMatrix g( m_cov.invert() ); // gain matrix (1x1)
	PHGslMatrix htgh( PHGslMatrix::get_AtBC( h, g, h ) );   
	
	PHGslMatrix c_filter_inv( c_pred_inv ); 
	c_filter_inv += htgh;
	
	PHGslMatrix c_filter( c_filter_inv.invert() );
	
	// calculate filtered state vector
	// p_filter = c_filter.( c_pred^{-1}.p_pred + h^t.g.meas )
	PHGslMatrix htgm( PHGslMatrix::get_AtBC( h, g, m ) );
	PHGslMatrix p_filter( c_pred_inv*p_pred );
	p_filter += htgm;
	p_filter = PHGslMatrix( c_filter*p_filter );
	
	// calculate filtered residual and associated covariance
	// r_filter = m - h.p_filter
	// r_cov_filter = m_cov - h.c_filter.h^t
	PHGslMatrix r_filter( m );
	r_filter -= (h*p_filter);
	PHGslMatrix r_cov_filter( m_cov ); 
	r_cov_filter -= PHGslMatrix::get_ABCt( h, c_filter, h ); 
	
	// calculate filtered parameter contribution to chisquare 
	// chi2+= chi2m+chi2p
	// with chi2m = r_filter^t.g.r_filter (contribution from measurement)
	// and chi2p = (p_filter-p_pred)^t.c_pred^{-1}.(p_filter-p_pred) (contribution from extrapolator)
	double chi2m = PHGslMatrix::get_AtBC( r_filter, g, r_filter )(0,0);
	
	PHGslMatrix dp( p_filter ); 
	dp -= p_pred;
	double chi2p = PHGslMatrix::get_AtBC( dp, c_pred_inv, dp )(0,0);
	
	// store filtered state vector
	node.get_filtered()._state_kf = p_filter;
	node.get_filtered()._covar_kf = c_filter;
	node.get_filtered()._z = node.get_predicted()._z;
	node.get_filtered()._direction = node.get_predicted()._direction;
	node.set_filter_done( true );
	
	// sets node chisquare
	node.set_chi_square( chi2m+chi2p );
			
	// dump, if required
	if( _verbosity >= MUTOO::ALOT ) {  
		MUTOO::PRINT( cout, "TMutKalmanFilter::const_filter - filtered");
		node.get_filtered().print( cerr );
	}    

	
	return true;
}

//__________________________________________________________________
bool TMutKalmanFilter::initialize_smoother( Node& node )
{
	
	// check if filtering has already been done
	if( !node.filter_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::initialize_smoother - no filter.\n";
		return false;
	}
	
	// if node has not been smoothed, init smoothed parameters from filtered
	if( !node.smooth_done() ) {
		node.get_smoothed() = node.get_filtered();
		node.set_smooth_done( true );
	}
	
	// initialize last_smoothed_node
	_last_smoothed_node = node;
	return true;

}
		
//__________________________________________________________________
bool TMutKalmanFilter::smooth( 
	Node& node, 
	const bool& update_running_par )
{
	
	// check if filtering has already been done
	if( node.smooth_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::smooth - smoothing already done.\n";
		return true;
	} 
	
	// check if filtering is available
	if( !node.filter_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::smooth - no filter.\n";
		return false;
	}
	
	// check if last_smoothed_node was smoothed (!)
	if( !_last_smoothed_node.smooth_done() ) {
		if( _verbosity >= MUTOO::ALOT ) cerr << "TMutKalmanFilter::smooth - previous node not smoothed.\n";
		return false;
	}
	
	// calculate smoothed state vector
	// p_smo = p_filter + a.(p_prev_smo - p_prev_pred)
	// with a=c_filter.dout_din_prev^t.c_prev_pred^{-1}
	const PHGslMatrix &c_filter( node.get_filtered()._covar_kf );
	const PHGslMatrix &dout_din_prev( _last_smoothed_node.get_dout_din() );
	const PHGslMatrix &c_prev_pred( _last_smoothed_node.get_predicted()._covar_kf );
	PHGslMatrix a( PHGslMatrix::get_ABtCinv( c_filter,  dout_din_prev, c_prev_pred ) );
		
	const PHGslMatrix &p_filter( node.get_filtered()._state_kf );
	const PHGslMatrix &p_prev_pred( _last_smoothed_node.get_predicted()._state_kf );
	const PHGslMatrix &p_prev_smo( _last_smoothed_node.get_smoothed()._state_kf );
	
	PHGslMatrix dp_prev( p_prev_smo ); 
	dp_prev -= p_prev_pred;
	
	PHGslMatrix adp_prev( a*dp_prev );
	PHGslMatrix p_smo( p_filter ); 
	p_smo += adp_prev;
	
	// calculate smoothed covariance matrix
	// c_smo = c_filter + a.( c_prev_smo - c_prev_pred).a^t
	const PHGslMatrix &c_prev_smo( _last_smoothed_node.get_smoothed()._covar_kf );
	
	PHGslMatrix dc_prev( c_prev_smo ); 
	dc_prev -= c_prev_pred;
	
	PHGslMatrix c_smo( c_filter ); 
	c_smo += PHGslMatrix::get_ABCt( a, dc_prev, a );
	
	// fill smoothed track parameter
	node.get_smoothed()._state_kf = p_smo;
	node.get_smoothed()._covar_kf = c_smo;
	node.get_smoothed()._z = node.get_filtered()._z;
	node.get_smoothed()._direction =  node.get_filtered()._direction;
	node.set_smooth_done( true );
	
	// update last smoothed node (for next node smoothing) if required
	if( update_running_par ) set_last_smoothed_node( node );

	if( _verbosity >= MUTOO::MAX ) {
		MUTOO::PRINT( cout, "mMutKalFit::smooth");
		node.get_smoothed().print();
	}    

	return true;

}
