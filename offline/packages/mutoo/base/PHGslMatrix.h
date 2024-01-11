// $Id: PHGslMatrix.h,v 1.5 2011/06/06 19:12:52 mwysocki Exp $

//////////////////////////////////////////////////////////////////
/*! 
  \file PHGslMatrix.h
  \brief basic (and incomplete) c++ wrapper for gsl matrices
  \author	Hugo Pereira
  \version $Revision: 1.5 $
  \date		$Date: 2011/06/06 19:12:52 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __PHGslMatrix_H__
#define __PHGslMatrix_H__

#include<gsl/gsl_matrix.h>
#include <iostream>
#include <sstream>

#include "PHException.h"

/*! 
  \class PHGslMatrix
  \brief basic (and incomplete) c++ wrapper for gsl matrices
*/
class PHGslMatrix {
  public:
  
  //! allocation constructor
  PHGslMatrix( unsigned int n_rows, unsigned int n_cols );
  
  //! copy constructor
  PHGslMatrix( const PHGslMatrix& m );
  
  //! equal to operator
  PHGslMatrix& operator = ( const PHGslMatrix& m );
  
  //! add
  PHGslMatrix& operator += ( const PHGslMatrix& m );
  
  //! subtract
  PHGslMatrix& operator -= ( const PHGslMatrix& m );
  
  //! constant add
  PHGslMatrix& operator += ( double value );
    
  //! const subtract
  PHGslMatrix& operator -= ( double value );

  //! scalar multiplication
  PHGslMatrix& operator *= ( double value );

  //! matrix multiplication
  PHGslMatrix operator * ( const PHGslMatrix& m ) const;
  
  //! destructor
  ~PHGslMatrix( void );
  
  //! retrieves reference to matrix element (row, column)
  double& operator () ( unsigned int row, unsigned int col )
  { 
    if( !( row < _n_rows && col < _n_cols ) ) 
    throw std::logic_error(DESCRIPTION("indexes out of bound.") );
    return *gsl_matrix_ptr( _matrix, row, col ); 
  }

  //! retrieves const reference to matrix element (row, column)
  const double& operator () ( unsigned int row, unsigned int col ) const
  { 
    if( !( row < _n_rows && col < _n_cols ) ) 
    throw std::logic_error(DESCRIPTION("indexes out of bound.") );
    return *gsl_matrix_const_ptr( _matrix, row, col ); 
  }
  
  //! set matrix element (row,column)
  void set( unsigned int row, unsigned int col, double value )
  { (*this)(row,col) = value; }
  
  //! get matrix element (row,column)
  double get( unsigned int row, unsigned int col ) const
  { return (*this)(row,col); }
  
  //! Multiply the elements of input matrix with this, return new with result
  PHGslMatrix mul_elements(const PHGslMatrix& m) const;

  //! Return the sum of all the elements in the matrix
  double sum_elements() const;

  //! Return the trace of the matrix
  double trace() const;

  //! returns invert matrix of this
  PHGslMatrix invert( void ) const;

  //! returns diagonalized matrix of this
  PHGslMatrix diagonalize() const;
  
  //! returns transpose matrix of this
  PHGslMatrix transpose( void ) const;
  
  //! initialise all elements to zero
  void zero( void );
  
  //! return true if all elements are zero
  bool is_zero( void ) const;
  
  //! initialise to unit matrix (squared matrices only)
  void unit( void );
  
  //! dump matrix to stream	
  void print( std::ostream &out = std::cout ) const;   
  
  //! dump matrix to stream	
  friend std::ostream &operator << (std::ostream &o,const PHGslMatrix &m);   

  //! dump number of existing PHGslMatrix
  static void dump_counter( void ) 
  { std::cout << "PHGslMatrix::dump_counter: " << _obj_counter << std::endl; } 
  
  //________________________________________
  // matrix calculations
  inline static PHGslMatrix get_ABC( const PHGslMatrix& A, const PHGslMatrix &B, const PHGslMatrix &C ) { return A*B*C; } //!< returns A.B.C
  inline static PHGslMatrix get_ABCt( const PHGslMatrix& A, const PHGslMatrix &B, const PHGslMatrix &C ) { return A*B*( C.transpose() ); } //!< returns A.B.(C^t)
  inline static PHGslMatrix get_AtBC( const PHGslMatrix& A, const PHGslMatrix &B, const PHGslMatrix &C ) { return ( A.transpose() )*B*C; } //!< returns (A^t).B.C
  inline static PHGslMatrix get_ABtC( const PHGslMatrix& A, const PHGslMatrix &B, const PHGslMatrix &C ) { return A*( B.transpose() )*C; }	//!< returns A.(B^t).C
  inline static PHGslMatrix get_ABtCinv( const PHGslMatrix& A, const PHGslMatrix &B, const PHGslMatrix &C ) { return A*( B.transpose() )*( C.invert() ); } //!< returns A.(B^t).(C^{-1})

  private:
  gsl_matrix* _matrix;  //!< pointer to the allocated gsl matrix
  unsigned int _n_rows; //!< number of matrix rows
  unsigned int _n_cols; //!< number of matrix columns
  
  static int _obj_counter; //!< number of existing PHGslMatrix objects
  
  /*! 
    suffix string for exception handling
  	this is a copy from PHExecption.h put there to minimize dependencies of
    the class
  */
  static std::string get_exception_suffix(std::string file, int line)
  {
    std::ostringstream tmp;
    tmp << line;
    return "  file:" + file + " " + "line:" + tmp.str();
  }
  
};
  
#endif
