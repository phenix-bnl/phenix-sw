// $Id: TMutKalmanFilter.h,v 1.2 2011/12/24 04:48:19 slash Exp $
#ifndef __TMUTKALMANFILTER_HH__
#define __TMUTKALMANFILTER_HH__

/*!
	\file TMutKalmanFilter.h
	\brief general kalman filter methods
	\author Hugo Pereira
	\version $Revision: 1.2 $
	\date $Date: 2011/12/24 04:48:19 $
*/

#include "MUTOO.h"
#include "PHException.h"
#include "PHGslMatrix.h"
#include "TMutTrkPar.hh"

#include "TMutKalmanUtil.h"

#include <list>

/*!
	\class TMutKalmanFilter
	\brief general kalman filter methods
*/
class TMutKalmanFilter
{
	public: 

	//! constructor
	TMutKalmanFilter():
		_verbosity( MUTOO::NONE )
		{}
	
	//! destructor
	~TMutKalmanFilter()
	{}
		
	//! set verbosity
	void set_verbosity( MUTOO::Verbosity verbosity )
	{ _verbosity = verbosity; }
	
	//! kalman style track parameters
	struct TrkPar {
	
		//! empty constructor
		TrkPar( void ):
			_state_kf( PHGslMatrix( 5,1 ) ),
			_covar_kf( PHGslMatrix( 5,5 ) ),
			_z( 0 ),
			_direction( 0 )
		{}
		
		//! constructor based on TMutTrkPar
		TrkPar( const TMutTrkPar& trk_par ):
			_state_kf( TMutKalmanUtil::get_state_vector_kf( trk_par ) ),
			_covar_kf( TMutKalmanUtil::get_covar_kf( trk_par ) ),
			_z( trk_par.get_z() ),
			_direction( (trk_par.get_pz() < 0) ? -1:1 )
		{}

		//! dumps object to stream
		void print( std::ostream& out = std::cout ) const
		{
			MUTOO::PRINT( out, "mMutKalFit::TrkPar");
			out << "z=" << _z << "cm, direction=" << _direction << std::endl;
			out << "state vector: " << _state_kf;
			out << "covariance: " << _covar_kf;
		}
		
		//! retrieve x position of kf state vector
		double get_x( void ) const
		{ return _state_kf( 3, 0 ); }
		
		//! retrieve y position of kf state vector
		double get_y( void ) const
		{ return _state_kf( 4, 0 ); }
		
		//! retrieve z position of kf state vector
		double get_z( void ) const
		{ return _z; }
		
		//! retrieve dxdz
		double get_dxdz( void ) const
		{ return _state_kf( 1, 0 ); }
		
		//! retrieve dydz
		double get_dydz( void ) const
		{ return _state_kf( 2, 0 ); }
		
		PHGslMatrix _state_kf;
		PHGslMatrix _covar_kf;
		double _z;
		double _direction;
	
	};
	

	/*!
		kalman filter container. Stores the measurement and error; 
		the predicted, filtered and smoothed track parameters.
	*/
	class Node {	
		
		public:
		
		//! empty constructor
		Node( const double z = 0 ):
			_measurement( 1,1 ),
			_measurement_cov( 1,1 ),
			_h( 1,5 ),
			_dout_din( 5,5 ),
			_z( z ),
			_arm( MUTOO::South ),
			_prediction_done( false ),
			_filter_done( false ),
			_smooth_done( false )
		{ }
		
		//! destructor
		virtual ~Node( void )
		{}
			
		/*! 
			sort nodes	from vertex to muid
			this is a little tricky since some nodes may have same z, in which case we use the key
		*/
		virtual bool operator < (const Node& node ) const
		{			 
			return ( ( _arm == MUTOO::North ) ? ( _z < node._z ) : ( _z > node._z ) ); 
		} 
		
		//! dumps object to stream
		virtual void print( std::ostream& out = std::cout ) const
		{
			MUTOO::PRINT( out, "mMutKalFit::Node");
			
			// dump the measurement 
			out << "z=" << _z << "cm\n";
			
			out << "measurement: " << _measurement;
			out << "measurement covariance: " << _measurement_cov;
			
			// dump the state vectors
			MUTOO::PRINT( out, "mMutKalFit::Node - prediction");
			_predicted.print( out );
			
			MUTOO::PRINT( out, "mMutKalFit::Node - filter");
			_filtered.print( out );

			MUTOO::PRINT( out, "mMutKalFit::Node - smoothed");
			_smoothed.print( out );
			
			MUTOO::PRINT( out, "**");
		}
		
		//! reset flags
		virtual void reset_flags( void )
		{
			_prediction_done = false;
			_filter_done = false;
			_smooth_done = false;
			_chi_square = 0;
		}
		
		//! reference to predicted state vector
		virtual TrkPar& get_predicted( void )
		{ return _predicted; }
		
		//! reference to filtered state vector
		virtual TrkPar& get_filtered( void )
		{ return _filtered; }
		
		//! reference to smoothed state vector
		virtual TrkPar& get_smoothed( void )
		{ return _smoothed; }
		
		//! reference to predicted state vector
		const TrkPar& get_predicted( void ) const 
		{ return _predicted; }
		
		//! reference to filtered state vector
		virtual const TrkPar& get_filtered( void ) const 
		{ return _filtered; }
		
		//! reference to smoothed state vector
		virtual const TrkPar& get_smoothed( void ) const 
		{ return _smoothed; }
		 
		//! measurement
		virtual PHGslMatrix& get_measurement( void )
		{ return _measurement; }
		
		//! measurement covariance matrix
		virtual PHGslMatrix& get_measurement_cov( void )
		{ return _measurement_cov; }
		
		//! measurement covariance matrix (const)
		virtual const PHGslMatrix& get_measurement_cov( void ) const
		{ return _measurement_cov; }
		
		//! projection matrix
		virtual PHGslMatrix& get_h( void )
		{ return _h; }

		//! derivatives matrix
		virtual PHGslMatrix& get_dout_din( void )
		{ return _dout_din; }

		//! node position along the beam
		virtual double get_z( void ) const
		{ return _z; }

		//! arm
		virtual unsigned int get_arm( void ) const
		{ return _arm; }
		
		//! chisquare associated to filtering stage
		virtual const double& get_chi_square( void ) const
		{ return _chi_square; }
		 
		//! sets chisquare associated to filtering stage
		virtual void set_chi_square( const double& value )
		{ _chi_square = value; }
	 
		//! true if prediction was done
		virtual bool prediction_done( void ) const
		{ return _prediction_done; }
	 
		//! true if filter was done
		virtual bool filter_done( void ) const
		{ return _filter_done; }
	 
		//! true if smoothing was done
		virtual bool smooth_done( void ) const
		{ return _smooth_done; }
	 
		//! tells prediction was done
		virtual void set_prediction_done( bool value )
		{ _prediction_done = true; }
	 
		//! tells filter was done
		virtual void set_filter_done( bool value )
		{ _filter_done = value; }
	 
		//! true if smoothing was done
		virtual void set_smooth_done( bool value )
		{ _smooth_done = value; }
			 
		//! predicted residual [use cautiously - recalculated at every call]
		virtual PHGslMatrix get_predicted_residual( void ) const;
		
		//! predicted residual error [use cautiously - recalculated at every call]
		virtual PHGslMatrix get_predicted_residual_cov( void ) const;
	 
		//! filtered residual [use cautiously - recalculated at every call]
		virtual PHGslMatrix get_filtered_residual( void ) const;
		
		//! filtered residual error [use cautiously - recalculated at every call]
		virtual PHGslMatrix get_filtered_residual_cov( void ) const;
	 
		//! smoothed residual [use cautiously - recalculated at every call]
		virtual PHGslMatrix get_smoothed_residual( void ) const;
		
		//! smoothed residual error [use cautiously - recalculated at every call]
		virtual PHGslMatrix get_smoothed_residual_cov( void ) const;
		
		protected:
		
		//! set node z
		void set_z( const double& z )
		{ _z = z; }
		
		//! set node arm
		void set_arm( const unsigned int& arm )
		{ _arm = arm; }
				
		//! set measurement matrix and associated covariance
		void set_measurement( const PHGslMatrix& measurement, const PHGslMatrix& measurement_cov )
		{ 
			_measurement = measurement;
			_measurement_cov = measurement_cov;
		}
		
		//! set h (projection) matrix
		void set_h( const PHGslMatrix& h )
		{ _h = h; }
				
		private:
		
		//! associated predicted track parameter, from previous node in track
		TrkPar _predicted;	
		
		//! associated filtered track parameter
		TrkPar _filtered;	 
		
		//! associated smoothed track parameter
		TrkPar _smoothed;	 
		
		//! measurement matrix
		PHGslMatrix _measurement;	
		
		//! measurement covariance matrix	 
		PHGslMatrix _measurement_cov; 
		
		//! projection matrix from state vector to w
		PHGslMatrix _h;					
		
		//! propagator derivatives		 
		PHGslMatrix _dout_din;				
		
		//! coordinate mean z
		double _z;								
		
		//! arm to which the node belong		
		unsigned int _arm;						
		
		//! node chisquare; defined at filter
		double _chi_square;
		
		//! true when predict() was called successfuly for this node
		bool _prediction_done;	
		
		//! true when filter() was called successfuly for this node
		bool _filter_done;			
		
		//! true when smooth() was called successfuly for this node
		bool _smooth_done;			
		
	};
	
	//! retrieves reference to the starting parameters
	TrkPar& get_starting_parameters( void )
	{ return _running_trk_par; }

	//! calculated predicted parameters for measurement_id starting from running TrkPar
	bool predict( Node& node );
	
	//! make filtering for measurement_id; update running track parameters
	bool filter( Node& node );
	
	//! make filtering for measurement_id; do not update the running track parameters
	bool const_filter( Node& node ) const;
	
	/*! \brief
		update the running parameters (used for prediction of next node) from given node.
		this method is already called from the filter() if update_running_par is set to true.
		It is made separate to allow more tweaking such as outlier rejection
	*/
	bool set_running_trk_par( const Node& node )
	{
		if(!node.filter_done() ) {
			if( _verbosity >= MUTOO::ALOT ) std::cerr << "TMutKalmanFilter::set_running_trk_par - no filter.\n";
			return false;
		}
		_running_trk_par = node.get_filtered();	
		return true;
	}

	/*! \brief
	      initialization of the track parameters
	 */
	bool set_running_trk_par( void )
	{
	  _running_trk_par = TrkPar();	
	  return true;
	}
	
	//! initialize the smoother starting from measurement_id
	bool initialize_smoother( Node& node );
	
	//! make smoothing from measurement_id; update running track parameters if required
	bool smooth( Node& node, const bool& update_running_par = true );
	
	/*! \brief
		update the reference to the last valid smoothed node, used for smoothing the next one
		this method is already called from the smooth() if update_running_par is set to true.
		It is made separate to allow more tweaking such as outlier rejection.
	*/
	bool set_last_smoothed_node( const Node& node )
	{ 
		if(!node.smooth_done() ) {
			if( _verbosity >= MUTOO::ALOT ) std::cerr << "TMutKalmanFilter::set_last_smoothed_node - no smooth.\n";
			return false;
		}
 
		_last_smoothed_node = node; 
		return true;
	}

	private:
	
	// object verbosity
	MUTOO::Verbosity _verbosity;
	
	//! running track parameters. used to get the prediction/filter at measurement i
	TrkPar _running_trk_par;
	
	//! last smoothed node (needed for smoothing measurement i) 
	Node _last_smoothed_node;
	 
};

#endif
