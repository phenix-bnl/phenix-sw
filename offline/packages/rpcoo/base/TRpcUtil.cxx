// $Id: TRpcUtil.cxx,v 1.3 2008/08/28 00:49:44 kempel Exp $

/*
  \file TRpcUtil.cxx
  \brief Utility class for RPC measurement model
  \author: H. Pereira Da Costa 
  \version $Revision: 1.3 $
  \date    $Date: 2008/08/28 00:49:44 $
*/

#include "TRpcUtil.h"
#include "RPCOO.h"
#include <cmath>

using namespace std;

//___________________________________________
PHGslMatrix TRpcUtil::get_covar_xy( const PHPoint& point, double error_r, double error_phi )
{

	// create r/phi covariance matrix
	PHGslMatrix covar_rphi( 2, 2 );
	covar_rphi(0,0) = RPCOO::SQUARE( error_r );
	covar_rphi(1,1) = RPCOO::SQUARE( error_phi );
	
	return get_covar_xy( point, covar_rphi );
		
}

//___________________________________________
PHGslMatrix TRpcUtil::get_covar_xy( const PHPoint& point, const PHGslMatrix& covar_rphi )
{
	
	// create rotation matrix
	PHGslMatrix m(2,2);
	
	double x( point.getX() );
	double y( point.getY() );
	double r( sqrt( RPCOO::SQUARE( x ) + RPCOO::SQUARE( y ) ) );
	
	double cos_phi( x/r );
	double sin_phi( y/r );
	
	m(0,0) = cos_phi;			// dx/dr
	m(0,1) = sin_phi;			// dy/dr
	m(1,0) = -r*sin_phi;	// dx/dphi
	m(1,1) = r*cos_phi;   // dy/dphi
	
	return m.transpose()*covar_rphi*m;
	
}
