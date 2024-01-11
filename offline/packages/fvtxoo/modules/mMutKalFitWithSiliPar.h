// $Id: mMutKalFitWithSiliPar.h,v 1.3 2013/03/18 00:07:13 jinhuang Exp $
#ifndef __MMutKalFitWithSiliPar_H__
#define __MMutKalFitWithSiliPar_H__

/*!
	 \file mMutKalFitWithSiliPar.h
	 \brief Runtime parameter object for mMutKalFit analysis module
	 \author Sean Kelly, Hugo Pereira
	 \version $Revision: 1.3 $
	 \date $Date: 2013/03/18 00:07:13 $
*/

#include<MUTOO.h>
#include<PHObject.h>
#include<PHException.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>

#ifndef __CINT__
#include<boost/array.hpp>
#endif

/*! 
	\class mMutKalFitWithSiliPar
	\brief Runtime parameter object for mMutKalFit analysis module
*/

class mMutKalFitWithSiliPar : public TMutParBase
{	
 public: 

	/*! default constructor */
	mMutKalFitWithSiliPar():
		_momentum_resolution( 0.5 ),
		_angular_resolution( 0.2 ),
		_position_resolution( 100 ),
		_z_reference( 0 ),
		_chi_cut( 0.01 ),
		_min_n_coord( 6 ),
		_max_iterations( 100 ),
		_use_anodes( true ),
		_use_muid( false ),
		_use_vtx( true ),
		_update_cov_matrix( true ),
		_extrapolate_to_muid( false ),
		_do_evaluation( false ),
		_evaluation_file( "mMutKalFit.root" ),
                _vtx_z_smear(0.0400),
                _vtx_phi_smear(0.0050)
	{ 
		
		_mutr_detector_active.assign( true );
		_mutr_detector_weight.assign( 1 );
		
		TMutParameterDB::get().get<unsigned short>( "mMutKalFit_verbosity", _verbosity );
		TMutParameterDB::get().get<Double_t>( "mMutKalFit_momentum_resolution", _momentum_resolution );
		TMutParameterDB::get().get<Double_t>( "mMutKalFit_angular_resolution", _angular_resolution );
		TMutParameterDB::get().get<Double_t>( "mMutKalFit_position_resolution", _position_resolution );
		TMutParameterDB::get().get<Double_t>( "mMutKalFit_z_reference", _z_reference );
		TMutParameterDB::get().get<Double_t>( "mMutKalFit_chi_cut", _chi_cut );
		TMutParameterDB::get().get<unsigned short>( "mMutKalFit_min_n_coord", _min_n_coord );
		TMutParameterDB::get().get<unsigned short>( "mMutKalFit_max_iterations", _max_iterations );

		TMutParameterDB::get().get<bool>( "mMutKalFit_use_anodes", _use_anodes );
		TMutParameterDB::get().get<bool>( "mMutKalFit_use_muid", _use_muid );
		TMutParameterDB::get().get<bool>( "mMutKalFit_use_vtx", _use_vtx );
		TMutParameterDB::get().get<bool>( "mMutKalFit_update_cov_matrix", _update_cov_matrix );
		TMutParameterDB::get().get<bool>( "mMutKalFit_extrapolate_to_muid", _extrapolate_to_muid );
		TMutParameterDB::get().get<bool>( "mMutKalFit_do_evaluation", _do_evaluation );
		TMutParameterDB::get().get<std::string>( "mMutKalFit_evaluation_file", _evaluation_file );
	}
	
	//! destructor
	~mMutKalFitWithSiliPar(){ }	
	
	//! PHOOL inteface requirement
	void identify(std::ostream& os = std::cout) const 
	{ os << "mMutKalFitWithSiliPar";}

	//! gets starting point momentum resolution
	const Double_t& get_momentum_resolution( void ) const 
	{ return _momentum_resolution; }

	//! sets starting point momentum resolution
	void set_momentum_resolution( const Double_t& value ) 
	{ _momentum_resolution = value; }

	//! gets starting point angular resolution
	const Double_t& get_angular_resolution( void ) const 
	{ return _angular_resolution; }	
	
	//! sets starting point angular resolution
	void set_angular_resolution( const Double_t& value ) 
	{ _angular_resolution = value; }	

	//! gets starting point position resolution
	const Double_t& get_position_resolution( void ) const 
	{ return _position_resolution; }
	
	//! sets starting point position resolution
	void set_position_resolution( const Double_t& value ) 
	{ _position_resolution = value; }

	//! gets reference z for extrapolation upstream of the absorber
	const Double_t& get_z_reference( void ) const 
	{ return _z_reference; }
	
	//! sets reference z for extrapolation upstream of the absorber
	void set_z_reference( const Double_t& value ) 
	{ _z_reference = value; }

	//! gets min number of coordinates / track to be fitted
	unsigned short get_min_n_coord( void ) const 
	{ return _min_n_coord; } 
	
	//! sets min number of coordinates / track to be fitted
	void set_min_n_coord( unsigned short value ) 
	{ _min_n_coord = value; }
	
	//! sets max number of iterations during fit
	void set_max_iterations( unsigned short value ) 
	{ _max_iterations = value; }
 
	//! gets max number of iterations during fit
	unsigned short get_max_iterations( void ) const 
	{ return _max_iterations; } 

	//! gets cut on relative chi_square difference to stop iterations
	const Double_t& get_chi_cut( void ) const 
	{ return _chi_cut; } 
 
	//! sets cut on relative chi_square difference to stop iterations
	void set_chi_cut( const Double_t& value ) 
	{ _chi_cut = value; }

	//! gets use_anodes correction flag
	bool get_use_anodes() const 
	{ return _use_anodes; }
 
	//! sets use_anodes correction flag
	void set_use_anodes( bool value ) 
	{ _use_anodes = value; }
 
	//! gets use_muid flag
	bool get_use_muid() const 
	{ return _use_muid; }
 
	//! sets use_muid flag
	void set_use_muid( bool value ) 
	{ _use_muid = value; }
 
	//! gets use_vtx flag
	bool get_use_vtx() const 
	{ return _use_vtx; }
 
	//! sets use_vtx flag
	void set_use_vtx( bool value ) 
	{ _use_vtx = value; }
 
	//! gets update_cov_matrix (from starting point, between iterations)
	const bool& get_update_cov_matrix() const 
	{ return _update_cov_matrix; }
	
	//! sets update_cov_matrix (from starting point, between iterations)
	void set_update_cov_matrix( bool value ) 
	{ _update_cov_matrix = value; }
	
	//! extrapolation to muid
	const bool& get_extrapolate_to_muid( void ) const
	{ return _extrapolate_to_muid; }
	
	//! extrapolation to muid
	void set_extrapolate_to_muid( bool value )
	{ _extrapolate_to_muid = value; }

	//! gets histogram flag
	const bool& get_do_evaluation( void ) const 
	{ return _do_evaluation; }
	
	//! sets histogram flag
	void set_do_evaluation( bool value ) 
	{ _do_evaluation = value; }

	//! gets histogram file
	const std::string& get_evaluation_file( void ) const 
	{ return _evaluation_file; }
	
	//! sets histogram file
	void set_evaluation_file( const std::string& value )
	{ _evaluation_file = value; }

  //! smearing to add to a z position of vtx hit (cm)
  const Double_t& get_vtx_z_smear( void ) const
  {return _vtx_z_smear; } 
  
  //! sets smearing to add to a z position of vtx hit (cm)
  void set_vtx_z_smear( const Double_t& value ) 
  { _vtx_z_smear = value; } 

  //! smearing to add to a phi position of vtx hit (cm)
  const Double_t& get_vtx_phi_smear( void ) const
  {return _vtx_phi_smear; } 

  //! sets smearing to add to a phi position of vtx hit (cm)
  void set_vtx_phi_smear( const Double_t& value ) 
  { _vtx_phi_smear = value; } 
  
  //! desactivated mutr detectors
  bool get_desactivated( int arm, int station, int gap, int cathode ) const
  { return _mutr_detector_active[ get_mutr_index( arm, station, gap, cathode ) ] == false; }

	//! desactivate	mutr detector
	void set_desactivated( int arm, int station, int gap, int cathode ) 
	{ _mutr_detector_active[ get_mutr_index( arm, station, gap, cathode ) ] = false; }

	/*! 
		activate	mutr detectors
		by default, all detectors are active
		this one is used to "reactivate" already desactivated detectors
	*/
	void set_activated( int arm, int station, int gap, int cathode ) 
	{ _mutr_detector_active[ get_mutr_index( arm, station, gap, cathode ) ] = true; }

	//! mutr detector weight in chisquare
	double get_mutr_detector_weight( int arm, int station, int gap, int cathode ) const
	{ return _mutr_detector_weight[ get_mutr_index( arm, station, gap, cathode ) ]; }
	
	//! mutr detector weight in chisquare
	void set_mutr_detector_weight( int arm, int station, int gap, int cathode, double weight )
	{ _mutr_detector_weight[ get_mutr_index( arm, station, gap, cathode ) ] = weight; }

	//! dump all parameters	
	void print( std::ostream& out = std::cout ) 
	{
		MUTOO::PRINT( out, "mMutKalFitWithSiliPar" );
		out << "_verbosity = " << _verbosity << ".\n";
		out << "_momentum_resolution = " << _momentum_resolution << "GeV.\n";
		out << "_angular_resolution = " << _angular_resolution << "rad.\n";
		out << "_position_resolution = " << _position_resolution << "cm.\n";
		out << "_z_reference = " << _z_reference << "cm.\n";
		out << "_use_anodes = " << _use_anodes << ".\n";
		out << "_update_cov_matrix = " << _update_cov_matrix << ".\n";
		out << "_min_n_coord = " << _min_n_coord << ".\n";
		out << "_max_iterations = " << _max_iterations << ".\n";
		out << "_chi_cut = " << _chi_cut << ".\n";
		
		out << "_extrapolate_to_muid = " << _extrapolate_to_muid << "\n";
		out << "_do_evaluation = " << _do_evaluation << ".\n";
		out << "_evaluation_file = " << _evaluation_file << ".\n";
		out << "_vtx_z_smear = " << _vtx_z_smear << ".\n";
		out << "_vtx_phi_smear = " << _vtx_phi_smear << ".\n";
		
		// print desactivated detectors
		for( int arm = 0; arm < MUTOO::NumberOfArms; arm++ )
		for( int station = 0; station < MUTOO::NumberOfStations; station++ )
		for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
		for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
		if( get_desactivated( arm, station, gap, cathode ) )
		out 
			<< "mutr arm=" << arm << " station=" << station << " gap=" << gap << " cathode=" << cathode 
			<< " desactivated" 
			<< std::endl;

		MUTOO::PRINT( out, "**" );
	}
	
 private:
	
	//! get unique index from mutr arm, station gap and cathode.
	static int get_mutr_index( int arm, int station, int gap, int cathode )
	{
		return 
			cathode + 
			MUTOO::NumberOfCathodePlanes*( gap + 
			MUTOO::NumberOfGaps*( station + 
			MUTOO::NumberOfStations*arm ) );
	}
	
	//! relative momentum resolution for starting covariance matrix [NO UNIT]. default is 0.5.
	Double_t _momentum_resolution;	
 
	//! absolute angular resolution for starting covariance matrix	[tangent(rad)]. default is 0.2.
	Double_t _angular_resolution;	 
	
	//! position along the beam for kalman filter end-extrapolation. default is 0.
	Double_t _position_resolution;	
	
	//! position along the beam for kalman filter end-extrapolation. default is 0
	Double_t _z_reference;					
	
	//! cut on relative chi2 increment to stop fit iterations [NO UNIT] default is 0.01.
	Double_t _chi_cut;							

	//! minimal number of TMutCoord in track before filter [NO UNIT]. default is 6.
	unsigned short _min_n_coord;		
	
	//! max number of filter/smooth iterations [NO UNIT]. default is 50.
	unsigned short _max_iterations; 
	
	//! if true, anode wire position is used when calculating w_residuals. default is true
	bool _use_anodes;

	//! if true, adds the muid cluster parameters to the fit
	bool _use_muid;						
	
	//! if true, adds the VTX hits to the fit
	bool _use_vtx;						
	
	//! shortcut to total number of cathodes
	enum { number_of_cathodes = 
			MUTOO::NumberOfArms*
			MUTOO::NumberOfStations*
			MUTOO::NumberOfGaps*
			MUTOO::NumberOfCathodePlanes };

#ifndef __CINT__
	//! boolean array to tell if a detector is active or not
	boost::array< bool, number_of_cathodes > _mutr_detector_active;
	
	/*! 
		boolean array to weight detector covariance in chisquare
		the smaller the weight, the smaller the station contributes to the 
		fit
	*/
	boost::array< double, number_of_cathodes > _mutr_detector_weight;
#endif

	/*!	
		if true, starting covariance matrix is updated between KF iteration using 
		last hit smoothed parameters from previous iteration
	*/
	bool _update_cov_matrix; 

	//! if true, track parameters at station 3 are swimmed to muid gap0 through the field
	bool _extrapolate_to_muid;

	//! if true evaluation histograms are booked and filled
	bool _do_evaluation;
	
	//! filname where to write evaluation histograms
	std::string _evaluation_file;

        //! smearing to add to z position of vtx hit (cm)
        Double_t _vtx_z_smear;

        //! smearing to add to phi position of vtx hit (cm)
        Double_t _vtx_phi_smear;

        ClassDef(mMutKalFitWithSiliPar, 1);
};

#endif







