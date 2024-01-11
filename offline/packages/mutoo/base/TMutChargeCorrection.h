// $Id: TMutChargeCorrection.h,v 1.6 2009/12/06 19:05:38 hpereira Exp $
//////////////////////////////////////////////////////////////////
/*!
   \file    TMutChargeCorrection.h
   \brief   Utility class r-q correction and inverse
   \author  H. Pereira
   \version $Revision: 1.6 $
   \date    $Date: 2009/12/06 19:05:38 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMutChargeCorrection_h__
#define __TMutChargeCorrection_h__

// MUTOO
#include "MUTOO.h"

#ifndef __CINT__
#include "MUTOO_HASH_MAP.h"
#endif 

#include <cmath>
#include <iostream>
#include <string>
#include <vector>

/*! \ingroup classes */
/*! 
	\class TMutChargeCorrection
	\brief Utility class r-q correction and inverse
*/
/*! 
	investigating real data charge correlation for coordinates on different cathodes
	but belonging to the same particle, it was found out that it was biased, the bias depending on
	r the distance of the particle to the beam and on the gap index. This utility class
	correct this bias using a polynomial fit based on real data and read from a calibration file
	the inverse correction must be applied to MC charges for consistancy with real data
*/

class TMutChargeCorrection
{

	public:
  
  //! destructor
  virtual ~TMutChargeCorrection() {}
        
	//! defines file from which correction are read
	static void set_file( const std::string& filename )
	{ _filename = filename; }
		
	//! turn correction on/off
	void set_do_correction( bool value )
	{ _do_correction = value; }
	
	#ifndef __CINT__
	
	/*! \brief 
		returns charge correction for a given (x,y) point in a given gap
		the way the correction is applied is: 
		q_new = q_old/( 1 - (cathode==0)?0:correction)
	*/
	static double get_correction(
		const MUTOO::gap_locator& location,
    const double& x,
    const double& y );
	
	#endif

	private:
		
	//! charge correction parameters
	class ParameterSet {
	
		public:
		
		//! empty constructor
		ParameterSet( void ):
			_params( std::vector<double>( N_PARAM, 0 ) )
			{}
		
		//! filled constructor
		ParameterSet( std::istream &in ):
			_params( std::vector<double>( N_PARAM, 0 ) )
			{ read( in ); }
		
		//! read parameters from stream
		void read( std::istream &in );
			
		//! calculate correction for a given x,y point
		double get_correction( const double& x, const double& y ) const
		{
			double out( 0 );
			double r( std::sqrt( MUTOO::SQUARE(x) + MUTOO::SQUARE(y) ) );
			for( int i= _params.size() - 1; i >= 0; i-- )
			out = _params[i] + r*out;
			return out;
		}
		
		private:
		
		//! number of parameters used for polynomial correction
		enum{ N_PARAM = 7 };

		//! vector of parameters
		std::vector< double > _params;
    
	};
	
	#ifndef __CINT__
	
	// charge correction hash_map
  typedef MUTOO::hash_map<ULong_t, ParameterSet >::type ChargeCorrectionMap;

	//! retrieve correctionMap singleton
	static const ChargeCorrectionMap& get_map( void )
	{ 
		static ChargeCorrectionMap _map = initialize();
		return _map;
	}

	#endif

	//! read corrections from file, initialize correction map
	static ChargeCorrectionMap initialize( void );

	//! turn on/off the charge correction
	static bool _do_correction;

	//! file from which corrections are to be read
	static std::string _filename;
 
  ClassDef(TMutChargeCorrection,1)
	
};

#endif
