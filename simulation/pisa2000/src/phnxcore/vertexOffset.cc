// $Id: vertexOffset.cc,v 1.6 2013/03/17 00:14:45 pinkenbu Exp $

/*!
   \file vertexOffset.cc
   \brief handles pisa internal vertex generation
   \author Hugo Pereira
   \version $Revision: 1.6 $
   \date $Date: 2013/03/17 00:14:45 $
*/

#include <gsl/gsl_randist.h>
#include <assert.h>
#include <iostream>
#include <sys/types.h>
#include <unistd.h>

#include "vertexOffset.h"

using namespace std;

//! wrapper to vertex offset initialization
/*! this allows to access the class in fortran code */
extern "C" {
  
  //! vertex parameters initialization
  void intialize_vertex_offset__( float*, float* ); 
  
  //! generate a vertex using parameters for a given event
  void generate_vertex_offset__( void );
  
  //! apply vertex offset to position passed in argument
  // void apply_vertex_offset__( float* );
  void apply_vertex_offset__( float*, float*, float* );
  
  //! print vertex parameters and current vertex
  void print_vertex_offset__( void );
  
}

//__________________________________________________________________
VertexOffset VertexOffset::singleton;

//__________________________________________
void intialize_vertex_offset__( float* mean_position, float* width )
{ 
  VertexOffset::singleton.set_mean_position( mean_position[0], mean_position[1], mean_position[2] ); 
  VertexOffset::singleton.set_width( width[0], width[1], width[2] ); 
  VertexOffset::singleton.print_parameters();
}

//__________________________________________
void generate_vertex_offset__( void )
{ VertexOffset::singleton.generate(); }

//__________________________________________
void apply_vertex_offset__( float* x, float* y, float* z )
{ 
  assert( x && y && z );
  VertexOffset::singleton.apply( *x, *y, *z ); 
}

//__________________________________________
void print_vertex_offset__( void )
{ VertexOffset::singleton.print(); }

//__________________________________________________________________
VertexOffset::VertexOffset( void ):
  _verbosity( 0 )
{
  cout << "VertexOffset::VertexOffset" << endl;
  _mean_position.assign(0);
  _width.assign(0);
  _position.assign(0);  
  
  // initialize random number generator
  gsl_rng_env_setup();
  const gsl_rng_type *T = gsl_rng_default;
  _rng = gsl_rng_alloc(T);

  gsl_rng_set(_rng, time(0) + getpid());
}

//__________________________________________________________________
VertexOffset::~VertexOffset( void )
{ 
  gsl_rng_free( _rng ); 
}

//__________________________________________________________________
void VertexOffset::generate( void )
{
  
  for( unsigned int i=0; i<3; i++ )
  {
    
    // add offsets
    _position[i] =  _mean_position[i];
    
    // add smearing
    // negative width. Use flat distribution
    if( _width[i] < 0 ) _position[i] +=  gsl_ran_flat( _rng, _width[i], -_width[i] );
    else if( _width[i] > 0 ) _position[i] += gsl_ran_gaussian( _rng, _width[i] );    
    
  }
  
  if( _verbosity > 0 )
  {
    cout << "VertexOffset::generate - ("
      << _position[0] << ","
      << _position[1] << ","
      << _position[2] << ")"
      << endl;
  }
  
  return;

}

//__________________________________________________________________
void VertexOffset::apply( float& x, float& y, float& z ) const
{
  
  if( _verbosity > 0 ) 
  { cout << "VertexOffset::apply - before: (" << x << "," << y << "," << z << ")" << endl; }
  
  x += _position[0];
  y += _position[1];
  z += _position[2];
  
  if( _verbosity > 0 ) 
  { cout << "VertexOffset::apply - after: (" << x << "," << y << "," << z << ")" << endl; }

}

//__________________________________________________________________
void VertexOffset::print_parameters( void ) const
{
  cout << "VertexOffset::print_parameters - _mean_position: (" << _mean_position[0] << "," << _mean_position[1] << "," << _mean_position[2] << ")" << endl;
  cout << "VertexOffset::print_parameters - _width: (" << _width[0] << "," << _width[1] << "," << _width[2] << ")" << endl;
}

//__________________________________________________________________
void VertexOffset::print( void ) const
{
  cout << "VertexOffset::print - _position: (" << _position[0] << "," << _position[1] << "," << _position[2] << ")" << endl;
}
