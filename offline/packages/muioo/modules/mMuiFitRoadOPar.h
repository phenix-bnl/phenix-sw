// $Id: mMuiFitRoadOPar.h,v 1.1 2006/04/22 01:59:14 hpereira Exp $
#ifndef __mMuiFitRoadOPar_h__
#define __mMuiFitRoadOPar_h__

/*!
  \file    mMuiFitRoadOPar.h
  \brief   parameters for road straight track fit
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:14 $
*/

#include<TMuiParBase.h>
//!  Runtime parameter object for mMuiFitRoadO analysis module
class mMuiFitRoadOPar : public TMuiParBase
{
  public:
			
  /*! default constructor */
  mMuiFitRoadOPar()
	{}
  
  /*! destructor */
  ~mMuiFitRoadOPar()
	{}
 	
	//! dumper
	void print( std::ostream& out = std::cout )
	{ 
    MUIOO::PRINT( out, "mMuiFitRoadOPar" );
		out << "_verbosity = " << _verbosity << std::endl;
    MUIOO::PRINT( out, "mMuiFitRoadOPar" );
	}		
		
};
#endif
