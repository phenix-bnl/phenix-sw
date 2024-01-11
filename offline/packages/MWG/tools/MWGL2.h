// $Id: MWGL2.h,v 1.2 2008/02/13 14:57:39 hpereira Exp $
#ifndef __MWGL2_H__
#define __MWGL2_H__

/*! 
	\file MWGL2.h
	\brief namespace to handle MWG to L2 associations
*/
#include <MUTOO.h>

namespace MWGL2 
{

  //! cut on delta theta for level2 to offline road matching (rads)
  static const double _delta_theta_cut = 2*MUTOO::DEG_TO_RAD;
  
  //! cut on delta phi for level2 to offline road matching (rads)
  static const double _delta_phi_cut = 6*MUTOO::DEG_TO_RAD;
  
  //! stores data associated to a given primitive
  class L2MuidData
  {
    
  public:
    
    //! constructor
    L2MuidData( double theta=0, double phi=0 ):
      _theta( theta ),
      _phi( phi )
    {}
    
    
    //! angle wrt beam
    double _theta;
    
    //! azimutal angle
    double _phi;
    
  };
  
};
#endif
