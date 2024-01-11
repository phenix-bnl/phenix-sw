// $Id: TMutTrackUtil.h,v 1.2 2011/12/24 04:48:19 slash Exp $

#ifndef __TMUTTRACKUTIL_H__
#define __TMUTTRACKUTIL_H__

/*
  \file TMutTrackUtil.h
  \brief Utility class for track fitting and measurement model
  \author: S.Kelly 
  \version $Revision: 1.2 $
  \date    $Date: 2011/12/24 04:48:19 $
*/

#include<PHPoint.h>
#include<PHLine.h>
#include<boost/tuple/tuple.hpp>
#include<boost/array.hpp>
#include"TMutTrkPar.hh"

class TMutFitPar;
class TMutTrackUtil
{
 public:

	//! Extrapolates TMutFitPar from reference to z. 
	static PHPoint linear_track_model(const TMutFitPar* par, double z)
  { return linear_track_model( *par, z ); }

	//! Extrapolates TMutTrktPar from reference to z. 
	static PHPoint linear_track_model(const TMutTrkPar* par, double z)
  { return linear_track_model( *par, z ); }

	//!Extrapolates TMutFitPar from reference to z. 
	static PHPoint linear_track_model(const TMutFitPar&, double z);							

	//! Extrapolates TMutTrktPar from reference to z. 
	static PHPoint linear_track_model(const TMutTrkPar&, double z);							

	/*! 
    Extrapolates TMutFitPar from reference to z.
    Update both point and cov matrix 
	*/
  static TMutFitPar linear_propagation(const TMutFitPar&, double z);

	/*! 
    Extrapolates TMutTrktPar from reference to z.
    Update both point and cov matrix 
	*/
  static TMutTrkPar linear_propagation(const TMutTrkPar&, double z);
 
	/*! delta phi is between station 2 and 3 */
	static double get_us_mom_point_point(unsigned short arm, double theta, double delta_phi);
	static double get_us_mom_point_point2(unsigned short arm, double theta, double delta_phi);

	/* delta phi is sum of dphi 12 and dphi23 corrected for dwdz of stub */
	static double get_us_mom_stub_point(unsigned short arm, double theta, double delta_phi);

 private:
	
	static double calc_power_law(const boost::array<double,3>& pars, double arg);
	
	static double calc_spline(const boost::array<double,5>& pars, double arg);
	
};

#endif







