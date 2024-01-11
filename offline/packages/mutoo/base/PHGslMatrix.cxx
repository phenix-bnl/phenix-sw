// $Id: PHGslMatrix.cxx,v 1.11 2014/11/27 02:23:22 slash Exp $

//////////////////////////////////////////////////////////////////
/*!
	\file PHGslMatrix.cxx
	\brief basic (and incomplete) c++ wrapper for gsl matrices
	\author	Hugo Pereira
	\version $Revision: 1.11 $
	\date		$Date: 2014/11/27 02:23:22 $
*/
//////////////////////////////////////////////////////////////////

#include "PHGslMatrix.h"
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_eigen.h>
#include <stdexcept>

//______________________________________________________
int PHGslMatrix::_obj_counter = 0;

//______________________________________________________
PHGslMatrix::PHGslMatrix(unsigned int n_rows, unsigned int n_cols) :
    _matrix(NULL),
    _n_rows(n_rows),
    _n_cols(n_cols)
{
	if( !( n_rows > 0 && n_cols > 0 ) )
	throw std::logic_error( DESCRIPTION("matrix number of rows/columns must be strictly positive") );

	_matrix = gsl_matrix_alloc( n_rows, n_cols );
	_obj_counter++;
	zero();
}

//______________________________________________________
PHGslMatrix::PHGslMatrix( const PHGslMatrix& m ):
	_matrix( gsl_matrix_alloc( m._n_rows, m._n_cols ) ),
	_n_rows( m._n_rows ),
	_n_cols( m._n_cols )
{
	_obj_counter++;
	gsl_matrix_memcpy( _matrix, m._matrix );
}

//______________________________________________________
PHGslMatrix::~PHGslMatrix( void )
{
	_obj_counter--;
	gsl_matrix_free( _matrix );
}

//______________________________________________________
PHGslMatrix& PHGslMatrix::operator = ( const PHGslMatrix& m )
{
	_n_rows = m._n_rows;
	_n_cols = m._n_cols;

	// free old matrix
	gsl_matrix_free( _matrix );

	// create new one with proper size
	_matrix = gsl_matrix_alloc( _n_rows, _n_cols );

	// copy elements from old matrix
	gsl_matrix_memcpy( _matrix, m._matrix );
	return *this;

}

//______________________________________________________
PHGslMatrix& PHGslMatrix::operator += ( const PHGslMatrix& m )
{
	if( !( _n_rows == m._n_rows && _n_cols == m._n_cols ) )
	throw std::logic_error(DESCRIPTION("matrices number of rows and/or columns does not match"));
	gsl_matrix_add( _matrix, m._matrix );
	return *this;

}

//______________________________________________________
PHGslMatrix& PHGslMatrix::operator -= ( const PHGslMatrix& m )
{
	if( !( _n_rows == m._n_rows && _n_cols == m._n_cols ) )
	throw std::logic_error(DESCRIPTION("matrices number of rows and/or columns does not match"));
	gsl_matrix_sub( _matrix, m._matrix );
	return *this;

}

//______________________________________________________
PHGslMatrix& PHGslMatrix::operator += ( double value )
{
	gsl_matrix_add_constant( _matrix, value );
	return *this;
}

//______________________________________________________
PHGslMatrix& PHGslMatrix::operator -= ( double value )
{
	gsl_matrix_add_constant( _matrix, -value );
	return *this;
}

//______________________________________________________
PHGslMatrix& PHGslMatrix::operator *= ( double value )
{
	gsl_matrix_scale( _matrix, value );
	return *this;

}

//______________________________________________________
PHGslMatrix PHGslMatrix::operator * ( const PHGslMatrix& m ) const
{
	if( _n_cols != m._n_rows )
	throw std::logic_error(DESCRIPTION("matrices are not compatible for multiplication."));
	PHGslMatrix out( _n_rows, m._n_cols );
	for( unsigned int row = 0; row < _n_rows; row++ )
	for( unsigned int col = 0; col < m._n_cols; col++ ) {
		double value = 0;
		for( unsigned int i=0; i< _n_cols; i++ )
		value += (*this)(row,i)*m(i,col);
		out( row, col ) = value;
	}
	return out;

}

// Multiply the elements of input matrix with this, and return a new
// matrix with the result
PHGslMatrix
PHGslMatrix::mul_elements(const PHGslMatrix& m) const
{
  if( _n_cols != m._n_cols || _n_rows != m._n_rows )
    throw std::logic_error(DESCRIPTION("PHGslMatrix::mul_elements: matrices dimensions do not match."));

  // copy _matrix into local
  gsl_matrix* A = gsl_matrix_alloc( _n_rows, _n_cols );
  gsl_matrix_memcpy( A, _matrix );

  gsl_matrix_mul_elements(A, m._matrix);

  PHGslMatrix out( _n_rows, _n_cols );

  for( unsigned int i=0; i<_n_rows; i++)
  {
    for( unsigned int j=0; j<_n_cols; j++)
    { out(i,j) = *gsl_matrix_const_ptr( A, i, j ); }
  }

  gsl_matrix_free(A);

  return out;
}

// Return the sum of all the elements in the matrix
double
PHGslMatrix::sum_elements() const
{
  double sum = 0.0;
  for( unsigned int i=0; i<_n_rows; i++ )
  {
    for( unsigned int j=0; j<_n_cols; j++)
    {  sum += (*this)(i,j); }
  }
  return sum;
}

// Return the trace of the matrix
double
PHGslMatrix::trace() const
{
  if( _n_cols != _n_rows )
    throw std::logic_error(DESCRIPTION("PHGslMatrix::trace: matrix not square."));

  double sum = 0.0;
  for( unsigned int i=0; i<_n_rows; i++)
  { sum += (*this)(i,i); }

  return sum;
}

//______________________________________________________
PHGslMatrix PHGslMatrix::invert( void ) const
{
	if( _n_rows != _n_cols ) throw std::logic_error(DESCRIPTION("matrix is not square."));

	// copy _matrix into local
	gsl_matrix *lu_matrix = gsl_matrix_alloc( _n_rows, _n_cols );
	gsl_matrix_memcpy( lu_matrix, _matrix );

	// calculate lu decomposition of current matrix
	int signum=0;
	gsl_permutation* p = gsl_permutation_alloc( _n_rows );
	gsl_linalg_LU_decomp(lu_matrix, p, &signum);

 	// check matrix detertminant (very fast as LU decomposition is done)
 	double det = gsl_linalg_LU_det( lu_matrix, signum );
 	if( !det )
 	  {
            // free permutation and local matrix
            gsl_permutation_free(p);
            gsl_matrix_free(lu_matrix);

 	    throw std::runtime_error( DESCRIPTION("Matrix cannot be inverted."));
 	  }

	// create output (inverted) matrix
	PHGslMatrix inverted( _n_rows, _n_cols );
	gsl_linalg_LU_invert(lu_matrix, p, inverted._matrix);

	// free permutation and local matrix
	gsl_permutation_free(p);
	gsl_matrix_free(lu_matrix);

	return inverted;
}

PHGslMatrix
PHGslMatrix::diagonalize() const
{
  if ( _n_rows != _n_cols ) throw std::logic_error(DESCRIPTION("PHGslMatrix::diagonalize: matrix is not square."));

  // copy _matrix into local
  gsl_matrix* A = gsl_matrix_alloc( _n_rows, _n_cols );
  gsl_matrix_memcpy( A, _matrix );
  gsl_vector* eval = gsl_vector_alloc(_n_rows);

  PHGslMatrix P( _n_rows, _n_cols );

  // Allocate workspace
  gsl_eigen_symmv_workspace* w = gsl_eigen_symmv_alloc(_n_rows);

  // Note that the diagonal and lower triangular elements of A are destroyed
  gsl_eigen_symmv(A,eval,P._matrix,w);

  // Since GSL doesn't guarantee any ordering, sort ascending.
  gsl_eigen_symmv_sort (eval, P._matrix, GSL_EIGEN_SORT_VAL_ASC);

  // create output (diagonalized) matrix
  PHGslMatrix D( _n_rows, _n_cols );
  D = get_ABC(P.invert(), *this, P);

  gsl_matrix_free(A);
  gsl_vector_free(eval);
  gsl_eigen_symmv_free(w);

  return D;
}


//______________________________________________________
PHGslMatrix PHGslMatrix::transpose( void ) const
{
	// create output (transposed) matrix (n_rows and n_cols are exchanged)
	PHGslMatrix transposed( _n_cols, _n_rows );
	gsl_matrix_transpose_memcpy( transposed._matrix, _matrix );
	return transposed;
}

//______________________________________________________
void PHGslMatrix::zero( void )
{	gsl_matrix_set_zero(_matrix ); }

//______________________________________________________
bool PHGslMatrix::is_zero( void ) const
{ return gsl_matrix_isnull(_matrix ); }

//______________________________________________________
void PHGslMatrix::unit( void )
{
	if( _n_rows != _n_cols ) throw std::logic_error(DESCRIPTION("matrix is not square."));
	gsl_matrix_set_identity(_matrix );
}

//______________________________________________________
void PHGslMatrix::print( std::ostream & out ) const
{
	out << "\nMatrix " << _n_rows << "x" << _n_cols << " is as follows";

	// dump column index
	out << "\n\n     |";
 	for( unsigned int col = 0; col < _n_cols; col++ ) {
		char* tmp = new char[512];
		sprintf( tmp, "   %6u  |", col );
		out << tmp;
		delete[] tmp;
	}

	out << "\n------------------------------------------------------------------\n";

	// dump matrix
 	for( unsigned int row = 0; row < _n_rows; row++ ) {

		// dump row index
		char* tmp = new char[512];
		sprintf( tmp, "%4u |", row);
		out << tmp;

		// dump row content
		for( unsigned int col = 0; col < _n_cols; col++ ) {
			sprintf( tmp, "%11.4g ", (*this)(row,col) );
			out << tmp;
		}

		out << std::endl;
		delete[] tmp;

	}

	out << std::endl;

	return ;
}

//______________________________________________________
std::ostream &operator << (std::ostream &out,const PHGslMatrix &m)
{
	m.print( out );
	return out;
}
