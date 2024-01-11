//////////////////////////////////////////////////////////////////
//
// Utility class: TMutKalmanUtil
// Author: H.Pereira 
// Date: 2/24/01
// Description: Utility class convert kalman objects into mutoo 
//              and vice-versa
//////////////////////////////////////////////////////////////////

#ifndef __TMUTKALMANUTIL_H__
#define __TMUTKALMANUTIL_H__

#include <PHVector.h>

#include "PHGslMatrix.h"
#include "TMutTrkPar.hh"

//! Utility class convert kalman objects into mutoo and vice-versa
class TMutKalmanUtil
{
	public:
	
	/* !
		\fn static PHGslMatrix get_covar_kf( const TMutTrkPar trk_par )
		\brief convert trk_par covariance matrix using mutoo state vector (x,y,px,py,pz)
		into kalman covariance matrix using (c/p,px/pz,py/pz,x,y)
		\param trk_par the track parameter object whose covariance matrix is to be converted
	*/
	static PHGslMatrix get_covar_kf( const TMutTrkPar& trk_par );
	
	/* 
		\fn static PHGslMatrix get_state_vector_kf( const TMutTrkPar trk_par )
		\brief convert trk_par state vector (x,y,px,py,pz)
		into kalman state vector (c/p,px/pz,py/pz,x,y)
		\param trk_par the track parameter object whose covariance matrix is to be converted
	*/
	static PHGslMatrix get_state_vector_kf( const TMutTrkPar& trk_par );
	
	//! prints kf state vector and covariance matrix using trk_par 
	static void print_trk_par_kf( const TMutTrkPar& trk_par );
	
	//_______________________________________
	// state vector conversions
	/*! \fn static PHGslMatrix cov_kalman_to_mutoo( double charge, TMatrix *cov_kalman, PHVector p, )
		\brief returns covariance matrix using mutoo state vector, from kalman cov matrix
		kalman state vector is (c/p, px/pz, py/pz, x, y); mutoo state vector is (x, y, px, py, pz)
		\param charge track charge (usualy 1 or -1)
		\param cov_kalman kalman covariance matrix. 
		\param p the momentum vector (mutoo style: px, py, pz)
	*/
	static PHGslMatrix cov_kalman_to_mutoo( double charge, PHGslMatrix cov_kalman, PHVector p );
			
	/* \fn static PHVector mom_kalman_to_mutoo( PHVector kal_mom )
		\brief converts a kalman momentum (c/p, px/pz, py/pz ) into mutoo (px, py, pz)
		\param arm the arm to wich the momentum belongs. arm==MUTOO::south then pz<0 otherwise pz>0
		\param kal_mom the momentum 3 vector to be converted
	*/
	static PHVector mom_kalman_to_mutoo( int arm, PHVector kal_mom );
		
	/*! \fn static PHGslMatrix cov_vtx_kalman_to_mutoo( double charge, TMatrix *cov_kalman, PHVector p, )
		\brief returns 3x3 covariance matrix using mutoo vertex state vector, from kalman vertex cov matrix
		kalman vertex state vector (momentum only) is (c/p, px/pz, py/pz); 
		mutoo vertex state vector (momentum only) is (px, py, pz)
		\param charge track charge (usualy 1 or -1)
		\param cov_kalman kalman covariance matrix. 
		\param p the momentum vector
	*/
	static PHGslMatrix cov_vtx_kalman_to_mutoo( double charge, PHGslMatrix cov_kalman, PHVector p );

	/*! \fn static PHGslMatrix cov_vtx_mutoo_to_kalman( double charge, TMatrix *cov_mutoo, PHVector p, )
		\brief returns 3x3 covariance matrix using kalman vertex state vector, from mutoo vertex cov matrix
		kalman vertex state vector (momentum only) is (c/p, px/pz, py/pz); 
		mutoo vertex state vector (momentum only) is (px, py, pz)
		\param charge track charge (usualy 1 or -1)
		\param cov_mutoo mutoo covariance matrix. 
		\param p the momentum vector
	*/
	static PHGslMatrix cov_vtx_mutoo_to_kalman( double charge, PHGslMatrix cov_mutoo, PHVector p );
};

#endif
