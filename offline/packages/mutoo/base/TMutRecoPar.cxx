#include<TMutRecoPar.hh>
#include<gsl/gsl_linalg.h>
ClassImp(TMutRecoPar)

  double
TMutRecoPar::get_error(unsigned short i, unsigned short j) const
{
  unsigned short index = i*COVAR_ROW+j;
  BOUNDS_CHECK(index,COVAR_SIZE);
  if(!_error_matrix_current) {
    calculate_error_matrix();
  }
  return _error[index];
}

void 
TMutRecoPar::calculate_error_matrix() const
{
  _error_matrix_current = true;
  
  //  initialize slush storage for LU decomposition
  //
  double lu_data[COVAR_SIZE];
  std::copy(_covar, _covar+COVAR_SIZE, lu_data);
  
  gsl_matrix_view covar = gsl_matrix_view_array(lu_data,COVAR_ROW,COVAR_ROW);
  gsl_matrix_view error = gsl_matrix_view_array(_error,COVAR_ROW,COVAR_ROW);
  gsl_permutation* p = gsl_permutation_alloc(COVAR_ROW);
  int s=0;
  
  // LU decompose the covariance matrix
  //
  gsl_linalg_LU_decomp(&covar.matrix, p, &s);
  
  // Invert and cache the result in _error
  //
  gsl_linalg_LU_invert(&covar.matrix, p, &error.matrix);
  
  // Clean up
  //
  gsl_permutation_free(p);
}




