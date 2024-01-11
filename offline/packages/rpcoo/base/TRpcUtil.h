// $Id: TRpcUtil.h,v 1.2 2008/08/28 00:49:44 kempel Exp $

#ifndef __TRpcUtil_h__
#define __TRpcUtil_h__

/*
  \file TRpcUtil.h
  \brief Utility class for RPC measurement model
  \author: H. Pereira Da Costa 
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:49:44 $
*/

#include <PHGslMatrix.h>
#include <PHPoint.h>

//! Utility class for RPC measurement model
class TRpcUtil
{

	public:
	
	//! converts r/phi error into xy covariance matrix
	static PHGslMatrix get_covar_xy( const PHPoint& point, double r_error, double phi_error ); 
	
	//! converts r/phi error into xy covariance matrix
	static PHGslMatrix get_covar_xy( const PHPoint& point, const PHGslMatrix& covar_r_phi ); 
		
};

#endif
