#ifndef PHGslRng_h
#define PHGslRng_h

// $Id: PHGslRng.h,v 1.1 2007/10/29 11:41:56 hpereira Exp $

//////////////////////////////////////////////////////////////////
/*! 
  \file PHGslRng.h
  \brief wrapper around gsl randome generator initialization
  \author	Hugo Pereira
  \version $Revision: 1.1 $
  \date		$Date: 2007/10/29 11:41:56 $
*/
//////////////////////////////////////////////////////////////////

#include<gsl/gsl_rng.h>
#include<iostream>
#include<string>

//! wrapper around gsl randome generator initialization
class PHGslRng
{
  
  public:
  
  //! constructor
  PHGslRng( unsigned long int seed = 0, bool verbose = false );
  
  //! destructor
  ~PHGslRng( void );
  
  //! pointer to internal rng structure
  gsl_rng* get() const
  { return _rng; }
  
  private:
  
  //! gsl random number generator initialization structure
  gsl_rng* _rng;
  
};

#endif
