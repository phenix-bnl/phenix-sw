// $Id: TMutTrackUtil.cxx,v 1.9 2014/11/28 13:14:38 slash Exp $

/*
  \file TMutTrackUtil.cxx
  \brief Utility class for track fitting and measurement model
  \author: S.Kelly 
  \version $Revision: 1.9 $
  \date    $Date: 2014/11/28 13:14:38 $
*/

#include<TMutGeo.h>
#include<TMutTrackUtil.h>
#include<TMutFitPar.hh>
#include<PHGeometry.h>
#include<MutWire.h>
#include<MUTOO.h>
#include<PHGslMatrix.h>

// BOOST/GSL
//
#include<gsl/gsl_math.h>
#include<gsl/gsl_diff.h>
#include<boost/array.hpp>

//____________________________________________________________________________
PHPoint TMutTrackUtil::linear_track_model(const TMutFitPar& par, double z)
{
  double x = par.get_x() + (z-par.get_z())*par.get_dxdz();
  double y = par.get_y() + (z-par.get_z())*par.get_dydz();
  return PHPoint(x,y,z);
}

//____________________________________________________________________________
PHPoint TMutTrackUtil::linear_track_model(const TMutTrkPar& par, double z)
{
  double x = par.get_x() + (z-par.get_z())*par.get_dxdz();
  double y = par.get_y() + (z-par.get_z())*par.get_dydz();
  return PHPoint(x,y,z);
}

//____________________________________________________________________________
TMutFitPar TMutTrackUtil::linear_propagation(const TMutFitPar& par, double z)
{
  // propagate position and tangents
  double x = par.get_x() + (z-par.get_z())*par.get_dxdz();
  double y = par.get_y() + (z-par.get_z())*par.get_dydz();
  TMutFitPar out( x, y, z, par.get_dxdz(), par.get_dydz(), par.get_chi_square() );
  
  // propagate covariance matrix
  PHGslMatrix cov( 4, 4 );
  for( unsigned int i=0; i<4; i++ )
  for( unsigned int j=0; j<4; j++ )
  cov(i,j) = par.get_covar(i,j);
  
  // propagation matrix (inverted)
  PHGslMatrix m( 4, 4 );
  m(0,0) = 1;
  m(0,2) = par.get_z()-z;
  m(1,1) = 1;
  m(1,3) = par.get_z()-z;
  m(2,2) = 1;
  m(3,3) = 1; 

  // update covariance matrix
  cov = m*cov*m.transpose();

  // assign to output parameters
  for( unsigned int i=0; i<4; i++ )
  for( unsigned int j=0; j<4; j++ )
  out.set_covar(i,j, cov(i,j) );
  
  return out;
  
}

//____________________________________________________________________________
TMutTrkPar TMutTrackUtil::linear_propagation(const TMutTrkPar& par, double z)
{
  double x = par.get_x() + (z-par.get_z())*par.get_dxdz();
  double y = par.get_y() + (z-par.get_z())*par.get_dydz();
  TMutTrkPar out( x, y, z, par.get_px(), par.get_py(), par.get_pz(), par.get_charge(), par.get_chi_square() );
  
  // propagate covariance matrix
  PHGslMatrix cov( 5, 5 );
  for( unsigned int i=0; i<5; i++ )
  for( unsigned int j=0; j<5; j++ )
  cov(i,j) = par.get_covar(i,j);
  
  // propagation matrix (inverted)
  PHGslMatrix m( 5, 5 );
  m(0,0) = 1;
  m(0,2) = (par.get_z()-z)/par.get_pz();
  m(1,1) = 1;
  m(1,3) = (par.get_z()-z)/par.get_pz();
  m(2,2) = 1;
  m(3,3) = 1; 
  m(4,4) = 1; 

  // update covariance matrix
  cov = m*cov*m.transpose();

  // assign to output parameters
  for( unsigned int i=0; i<5; i++ )
  for( unsigned int j=0; j<5; j++ )
  out.set_covar(i,j, cov(i,j) );

  return out;
  
}

//____________________________________________________________________________
double TMutTrackUtil::get_us_mom_point_point(unsigned short arm, double theta, double delta_phi)
{
	typedef boost::array<double,3> tuple;
	typedef boost::array<tuple,5> parameter_array; 
	
	// I'm prety sure these parameters are not the same as in north arm.
	// Needs to be revisited. CLS 11/26/2014
	static const parameter_array south_pars = 
	  {{
	      {{18.806,-0.878377,-0.0108998}},
	      {{10.2475,-0.925394,-0.00375413}},
	      {{5.93095,-0.860671,-0.00761713}},
	      {{3.80933,-0.774974,-0.00794547}},
	      {{3.16569,-0.753883,-0.0413415}}
	    }};

	static const parameter_array north_pars = 
	  {{
	      {{18.806,-0.878377,-0.0108998}},
	      {{10.2475,-0.925394,-0.00375413}},
	      {{5.93095,-0.860671,-0.00761713}},
	      {{3.80933,-0.774974,-0.00794547}},
	      {{3.16569,-0.753883,-0.0413415}}
	    }};
	
	static const double THETA_BIN_WIDTH=5;
	const parameter_array& parameters = (arm==MUTOO::South) ? south_pars : north_pars;
	
	double momentum=0;
	if(theta<12.5) {		
		momentum = calc_power_law(parameters[0], delta_phi);		
	} else if(theta>12.5 && theta<=17.5){
		
		double p1 = calc_power_law(parameters[0], delta_phi);
		double p2 = calc_power_law(parameters[1], delta_phi);
		double u = (theta-12.5)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>17.5 && theta<=22.5) {
		
		double p1 = calc_power_law(parameters[1], delta_phi);
		double p2 = calc_power_law(parameters[2], delta_phi);
		double u = (theta-17.5)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>22.5 && theta<=27.5) {
		
		double p1 = calc_power_law(parameters[2], delta_phi);
		double p2 = calc_power_law(parameters[3], delta_phi);
		double u = (theta-22.5)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>27.5 && theta<=32.5) {		
		
		double p1 = calc_power_law(parameters[3], delta_phi);
		double p2 = calc_power_law(parameters[4], delta_phi);
		double u = (theta-22.5)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>32.5) {		
		momentum = calc_power_law(parameters[4], delta_phi);				
	}

	return momentum;
}

//____________________________________________________________________________
double TMutTrackUtil::get_us_mom_stub_point(unsigned short arm, double theta, double delta_phi)
{
	typedef boost::array<double,3> tuple;
	typedef boost::array<tuple,10> parameter_array; 

//	static const parameter_array north_pars = 
//		{{
//      {{10.7707,-0.867273,-0.0411376}},
//		  {{7.32823,-0.94014,-0.0242739}},
//		  {{4.89033,-0.931233,-0.0349575}},
//		  {{3.4005,-0.93723,-0.0162803}},
//		  {{2.55277,-0.860369,-0.0299485}},
//		  {{1.77841,-0.973191,0.00260344}},
//		  {{1.30219,-1.25417,0.0179938}},
//		  {{0.920385,-1.27104,0.032351}},
//		  {{0.658067,-1.23293,0.0509874}},
//		  {{0.549315,-1.25285,0.0467101}}
//    }};

	// I'm prety sure these parameters are not the same as in north arm.
	// Needs to be revisited. CLS 11/26/2014
	static const parameter_array south_pars = 
	  {{
	      {{10.8261,-0.822418,-0.0957302}},
	      {{7.37735,-0.911649,-0.0503494}},
	      {{4.91151,-0.913069,-0.0376149}},
	      {{3.51512,-0.902782,-0.0641732}},
	      {{2.67931,-0.849884,-0.0968311}},
	      {{1.91627,-0.90742,-0.0744153}},
	      {{1.36512,-0.983505,-0.0239162}},
	      {{0.995581,-1.00802,-0.0108249}},
	      {{0.70835,-1.07326,0.0470287}},
	      {{0.577759,-1.08516,0.0750026}}
	    }};
	
	static const parameter_array north_pars = 
	  {{
	      {{10.8261,-0.822418,-0.0957302}},
	      {{7.37735,-0.911649,-0.0503494}},
	      {{4.91151,-0.913069,-0.0376149}},
	      {{3.51512,-0.902782,-0.0641732}},
	      {{2.67931,-0.849884,-0.0968311}},
	      {{1.91627,-0.90742,-0.0744153}},
	      {{1.36512,-0.983505,-0.0239162}},
	      {{0.995581,-1.00802,-0.0108249}},
	      {{0.70835,-1.07326,0.0470287}},
	      {{0.577759,-1.08516,0.0750026}}
	    }};
	
	static const double THETA_BIN_WIDTH=2.5;
	const parameter_array& parameters = (arm==MUTOO::South) ? south_pars : north_pars;
	
	double momentum=0;
	if(theta<11.25) {		
		
		momentum = calc_power_law(parameters[0], delta_phi);		

	} else if(theta>11.25 && theta<=13.75){
		
		double p1 = calc_power_law(parameters[0], delta_phi);
		double p2 = calc_power_law(parameters[1], delta_phi);
		double u = (theta-11.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>13.75 && theta<=16.25) {
		
		double p1 = calc_power_law(parameters[1], delta_phi);
		double p2 = calc_power_law(parameters[2], delta_phi);
		double u = (theta-13.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>16.25 && theta<=18.75) {		
		
		double p1 = calc_power_law(parameters[2], delta_phi);
		double p2 = calc_power_law(parameters[3], delta_phi);
		double u = (theta-16.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>18.75 && theta<=21.25) {		
		
		double p1 = calc_power_law(parameters[3], delta_phi);
		double p2 = calc_power_law(parameters[4], delta_phi);
		double u = (theta-18.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>21.25 && theta<=23.75) {		
		
		double p1 = calc_power_law(parameters[4], delta_phi);
		double p2 = calc_power_law(parameters[5], delta_phi);
		double u = (theta-21.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>23.75 && theta<=26.25) {		
		
		double p1 = calc_power_law(parameters[5], delta_phi);
		double p2 = calc_power_law(parameters[6], delta_phi);
		double u = (theta-23.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>26.25 && theta<=28.75) {		
		
		double p1 = calc_power_law(parameters[6], delta_phi);
		double p2 = calc_power_law(parameters[7], delta_phi);
		double u = (theta-26.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>28.75 && theta<=31.25) {		
		
		double p1 = calc_power_law(parameters[7], delta_phi);
		double p2 = calc_power_law(parameters[8], delta_phi);
		double u = (theta-28.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>31.25 && theta<=33.75) {		
		
		double p1 = calc_power_law(parameters[8], delta_phi);
		double p2 = calc_power_law(parameters[9], delta_phi);
		double u = (theta-31.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>33.75) {		
		
		momentum = calc_power_law(parameters[9], delta_phi);				

	}
	return momentum;
}

double
TMutTrackUtil::get_us_mom_point_point2(unsigned short arm, double theta, double delta_phi)
{
	typedef boost::array<double,3> tuple;
	typedef boost::array<tuple,10> parameter_array; 

	static const parameter_array north_pars = 
		{{
      {{16.4432,-0.850575,-0.0180498}},
		  {{11.1159,-0.828106,-0.0134217}},
		  {{8.52332,-0.93556,-0.0060213}},
		  {{6.35891,-0.975817,-0.00409551}},
		  {{4.87229,-1.02966,-0.000718993}},
		  {{3.76322,-1.03285,0.00301169}},
		  {{3.43408,-1.25414,0.0105775}},
		  {{2.53274,-1.18087,0.0125971}},
		  {{2.03056,-1.17504,0.0144957}},
		  {{1.81518,-1.21819,0.0171894}}
     }};
	
//	static const parameter_array north_pars = 
//		{{{{21.2064,-1.04985,-0.00491134}},
//		{{13.2229,-1.01025,-0.00386037}},
//		{{9.19805,-1.02593,-0.00312959}},
//		{{6.58113,-1.03429,-0.00110653}},
//		{{4.87295,-1.02405,-0.00242315}},
//		{{3.74486,-1.02078,-0.0040099}},
//		{{3.37184,-1.20274,0.0103352}},
//		{{2.47932,-1.12169,0.00890008}},
//		{{2.02695,-1.18162,0.016494}},
//		{{1.77018,-1.18413,0.0187875}}}};
	
	static const parameter_array south_pars = 
		{{{{13.3626,-0.9854,0.00207766}},
		{{10.73,-0.998017,-0.0039054}},
		{{7.31751,-0.995402,-0.002546}},
		{{5.25114,-1.03829,0.000726888}},
		{{3.81846,-1.03616,0.00151917}},
		{{2.86592,-1.02365,-0.00314993}},
		{{2.6376,-1.27312,0.00635498}},
		{{1.84187,-1.24217,0.0162329}},
		{{1.43533,-1.35145,0.0292046}},
		{{1.19749,-1.38281,0.0340843}}}};

	static const double THETA_BIN_WIDTH=2.5;
	const parameter_array& parameters = (arm==MUTOO::South) ? south_pars : north_pars;
	
	double momentum=0;
	if(theta<11.25) {		
		
		momentum = calc_power_law(parameters[0], delta_phi);		

	} else if(theta>11.25 && theta<=13.75){
		
		double p1 = calc_power_law(parameters[0], delta_phi);
		double p2 = calc_power_law(parameters[1], delta_phi);
		double u = (theta-11.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>13.75 && theta<=16.25) {
		
		double p1 = calc_power_law(parameters[1], delta_phi);
		double p2 = calc_power_law(parameters[2], delta_phi);
		double u = (theta-13.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>16.25 && theta<=18.75) {		
		
		double p1 = calc_power_law(parameters[2], delta_phi);
		double p2 = calc_power_law(parameters[3], delta_phi);
		double u = (theta-16.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>18.75 && theta<=21.25) {		
		
		double p1 = calc_power_law(parameters[3], delta_phi);
		double p2 = calc_power_law(parameters[4], delta_phi);
		double u = (theta-18.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>21.25 && theta<=23.75) {		
		
		double p1 = calc_power_law(parameters[4], delta_phi);
		double p2 = calc_power_law(parameters[5], delta_phi);
		double u = (theta-21.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>23.75 && theta<=26.25) {		
		
		double p1 = calc_power_law(parameters[5], delta_phi);
		double p2 = calc_power_law(parameters[6], delta_phi);
		double u = (theta-23.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>26.25 && theta<=28.75) {		
		
		double p1 = calc_power_law(parameters[6], delta_phi);
		double p2 = calc_power_law(parameters[7], delta_phi);
		double u = (theta-26.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>28.75 && theta<=31.25) {		
		
		double p1 = calc_power_law(parameters[7], delta_phi);
		double p2 = calc_power_law(parameters[8], delta_phi);
		double u = (theta-28.75)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>31.25 && theta<=33.75) {		
		
		double p1 = calc_power_law(parameters[8], delta_phi);
		double p2 = calc_power_law(parameters[9], delta_phi);
		double u = (theta-31.25)/THETA_BIN_WIDTH;
		momentum = (1-u)*p1 + u*p2;
		
	} else if(theta>33.75) momentum = calc_power_law(parameters[9], delta_phi);				
	return momentum;
}

//____________________________________________________________________________
double TMutTrackUtil::calc_power_law(const boost::array<double,3>& pars, double arg)
{
	// Power law parameterization y	= par1 * arg^par2
	return	pars[0] * std::pow(arg,pars[1]) + pars[2]*arg;
}

double
TMutTrackUtil::calc_spline(const boost::array<double,5>& pars, double arg)
{
	// cubic + linear (c1 continuous)
	//
	if(arg<pars[0]) return pars[1] + pars[2]*arg + pars[3]*arg*arg + pars[4]*arg*arg*arg;
	else return (pars[0] + pars[1]*pars[0] + pars[2]*pars[0]*pars[0] + pars[3]*pars[0]*pars[0]*pars[0]) 
		+ (pars[1]+2*pars[2]*pars[0]+3*pars[3]*pars[0]*pars[0])*(arg-pars[0]);
}
