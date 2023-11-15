#ifndef vertexOffset_h
#define vertexOffset_h

// $Id: vertexOffset.h,v 1.2 2008/11/16 15:55:32 hpereira Exp $

/*!
   \file vertexOffset.h
   \brief handles pisa internal vertex generation
   \author Hugo Pereira
   \version $Revision: 1.2 $
   \date $Date: 2008/11/16 15:55:32 $
*/

#include <boost/array.hpp>
#include<gsl/gsl_rng.h>

//! handles internal vertex generation
class VertexOffset
{
  
  public:
  
  //! singleton
  static VertexOffset singleton;
  
  //! constructor
  VertexOffset( void );

  //! destructor
  ~VertexOffset( void );
  
  //! verbosity
  void set_verbosity( int value )
  { _verbosity = value; }
  
  //! set vertex average values
  void set_mean_position( const float& x, const float& y, const float& z )
  { 
    _mean_position[0] = x;
    _mean_position[1] = y;
    _mean_position[2] = z;
  }
  
  //! set vertex distribution width
  /*! 
  positive value means that a gaussian distribution will be used.
  negative value means that a flat distribution will be used.
  */
  void set_width( const float& width_x, const float& width_y, const float& width_z )
  {
    _width[0] = width_x;
    _width[1] = width_y;
    _width[2] = width_z;
  }  
  
  //! generate vertex offsets for a given event
  void generate( void );
  
  //! apply generated vertex offset to a given point
  void apply( float& x, float& y, float& z ) const;
  
  //! print vertex parameters
  void print_parameters( void ) const;
  
  //! print current vertex offsets
  void print( void ) const;
  
  private:
  
  //! verbosity
  int _verbosity;
  
  //! vertex mean position
  boost::array<float, 3> _mean_position;
  
  //! vertex distribution width
  /*!
  positive value means that a gaussian distribution will be used.
  negative value means that a flat distribution will be used.
  */  
  boost::array<float, 3> _width;
  
  //! vertex current offsets
  boost::array<float, 3> _position;
  
  //! gsl random number generator initialization structure
  gsl_rng* _rng;
  
};

#endif
