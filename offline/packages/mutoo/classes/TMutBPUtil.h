#ifndef __TMUTBPUTIL_H__
#define __TMUTBPUTIL_H__

// $Id: TMutBPUtil.h,v 1.16 2011/12/24 04:48:20 slash Exp $

/*!
	\file TMutBPUtil.h
	\brief Utility class for track bend plane fit
	\author S.Kelly
	\version $Revision: 1.16 $
	\date $Date: 2011/12/24 04:48:20 $
*/

#include<TDataType.h>
#include<MUTOO.h>
#include<PHException.h>
#include<PHPoint.h>
#include<TMutTrkMap.h>
#include<boost/array.hpp>

//! Utility class for track bend plane fit
class TMutBPUtil
{
 public:	

	//! mode for BP 
	enum Mode {
		NORMAL,	 //!< normal mode (muid to vertex)
		REVERSE,	//!< from vertex to muid
		FIELD_OFF //!< straight tracks
	};
	
	//! get bend plane z from station 1 and 2 (static parametrization)
	static double get_zbp_sta12(int arm, float theta);
	
	//! get Bend plane z from station 2 and 3 (static parametrization)
	static double get_zbp_sta23(int arm, float theta);
	
	//! get transverse momentum from station 1 to 2
	static double get_pt_kick_sta12(int arm, float theta);
	
	//! get transverse momentum from station 2 to 3
	static double get_pt_kick_sta23(int arm, float theta);
	
	//! calculate bendplane window in given station when extrapolating track
	static void get_bp_window(TMutTrkMap::const_pointer trk_ptr, unsigned short station);
	
	//! define module verbosity
	static void set_verbosity(MUTOO::Verbosity value) 
	{ _verbosity = value; }
	
	//! defines BP mode
	static void set_mode(Mode mode) 
	{ _mode = mode; }
 
	//! get BP mode
	static Mode get_mode( void ) 
	{ return _mode; }
 
	//! dump mode
	static void print_mode( std::ostream& out = std::cout )
	{ 
		out << "TMutBPUtil::print_mode - ";
		switch( _mode )
		{
			case REVERSE: out << "REVERSE" << std::endl; break;
			case NORMAL: out << "NORMAL" << std::endl; break;
			case FIELD_OFF: out << "FIELD_OFF" << std::endl; break;
			default: out << "unrecognized" << std::endl; break; 
		}
	}
	
	//! retrieves momentum at vertex from BP fit
	static boost::array<double,3> get_mom_vtx(const PHPoint& vtx_point, TMutTrkMap::const_pointer trk_ptr);
	
	//! retrieve distance of closest approach from two tracks
	static int get_distcls(
		const PHPoint& xx1, 
		const PHPoint& xx2, 
		const double tanx1[2], 
		const double tanx2[2],
		PHPoint& x1c, PHPoint& x2c);

 private:
	
	//! module verbosity
	static MUTOO::Verbosity _verbosity;
		
	//! bend plane mode
	static Mode _mode;
	
	//! min theta angle for south
	static const double SOUTH_PISTON_ANGLE;
	
	//! min theta angle for north
	static const double NORTH_PISTON_ANGLE;

	//! check arm number is valid
	static void sanity_check(unsigned short arm) {
		if(arm>=MUTOO::NumberOfArms) 
		throw std::invalid_argument(DESCRIPTION("Invalid arm specifier")); 
	}

};

#endif











