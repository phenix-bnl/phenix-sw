//////////////////////////////////////////////////////////////////
//
// Utility class: TMutCathErrors:
// Author: S.Kelly 
// Date: 5/30/02
// Description: Utility class for encapsulating per plane errors
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUTCATHERRORS_H__
#define __TMUTCATHERRORS_H__

// BOOST/SL includes
//
#include<boost/array.hpp>
#include<map>
#include<vector>
#include<cmath>
#include<climits>

// MUTOO includes
//
#include<MUTOO.h>
#include<MutStrip.h>


/*! \ingroup classes */
//! Utility class that encapsulates cathode errors.

/*!
*/

class TMutCathErrors {
  
 public:

  static double get_error(unsigned short arm, 
			  unsigned short station,
			  unsigned short octant,
			  unsigned short half_octant,
			  unsigned short gap,
			  unsigned short cathode);	      

};

#endif



