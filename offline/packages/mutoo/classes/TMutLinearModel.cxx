#include<TMutLinearModel.h>
#include <gsl/gsl_version.h>
//________________________________________________________________
/* 
   Default constuctor sizes the parameter vector 
   and initializes values to 0 
*/
TMutLinearModel::TMutLinearModel() : 
  _parameters(N_LINEAR_PAR,0.0),
  _z(0)
{
  // Initialize storage for covariance matrix
  //
  _covar = gsl_matrix_alloc(N_LINEAR_PAR, N_LINEAR_PAR);
  clear_jacobian();
}

//________________________________________________________________
/*! Non-trivial destructor courtesty GSL */
TMutLinearModel::~TMutLinearModel()
{ gsl_matrix_free(_covar); }

//________________________________________________________________
void TMutLinearModel::initialize(TMutTrkMap::const_pointer trk_ptr, double z)
{
  // Initialize track models data member parameter array from
  // current TMutFitPar from current TMutTrk
  //
  _parameters[0] = trk_ptr->get()->get_fit_par()->get_x();
  _parameters[1] = trk_ptr->get()->get_fit_par()->get_y();
  _parameters[2] = trk_ptr->get()->get_fit_par()->get_dxdz();
  _parameters[3] = trk_ptr->get()->get_fit_par()->get_dydz();
  _z = trk_ptr->get()->get_fit_par()->get_z();
  
  // Extrapolate to given z
  //
  _parameters = extrapolate(z);
  _z = z;
}

//________________________________________________________________
void TMutLinearModel::initialize(TMutTrkMap::const_pointer trk_ptr)
{
  // Take reference surface from TMutTrk
  //
  initialize(trk_ptr,trk_ptr->get()->get_fit_par()->get_z());
}

//________________________________________________________________
void TMutLinearModel::initialize(TMutStubMap::const_pointer stub_ptr, double z)
{
  // Initialize track models data member parameter array from
  // current TMutFitPar from current TMutTrk
  //
  _parameters[0] = stub_ptr->get()->get_fit_par()->get_x();
  _parameters[1] = stub_ptr->get()->get_fit_par()->get_y();
  _parameters[2] = stub_ptr->get()->get_fit_par()->get_dxdz();
  _parameters[3] = stub_ptr->get()->get_fit_par()->get_dydz();
  _z = stub_ptr->get()->get_fit_par()->get_z();
  
  // Extrapolate to given z
  //
  _parameters = extrapolate(z);
  _z = z;
}

//________________________________________________________________
void TMutLinearModel::initialize(TMutStubMap::const_pointer stub_ptr)
{
  // Take reference surface from TMutTrk
  initialize(stub_ptr,stub_ptr->get()->get_fit_par()->get_z());
}

//________________________________________________________________
void TMutLinearModel::finish(TMutTrkMap::pointer trk_ptr, double z)
{
  // Extrapolate to the nominal PHENIX vertex
  //
  value_array z_par = extrapolate(z);
  
  // Convenience local
  //
  TMutFitPar local_fit_par;
  local_fit_par.set_x(z_par[0]);
  local_fit_par.set_y(z_par[1]);
  local_fit_par.set_dxdz(z_par[2]);
  local_fit_par.set_dydz(z_par[3]);
  local_fit_par.set_z(z);

  // Update the covariance matrix
  //
  for(size_t i=0;i<get_n_par();++i){
    for(size_t j=0;j<get_n_par();++j){
      local_fit_par.set_covar(i,j,gsl_matrix_get(_covar,i,j));
    }
  }
  // Update track's TMutFitPar
  //
  trk_ptr->get()->set_fit_par(local_fit_par);  
  
  // Extrapolate to nominal PHENIX vertex, and fill relevant fields in Reco and Trk par
  value_array ext_par = extrapolate(0);
  trk_ptr->get()->set_trk_par_vtx(TMutTrkPar(ext_par[0], ext_par[0],0));
	
}

//________________________________________________________________
void TMutLinearModel::finish(TMutTrkMap::pointer trk_ptr)
{
  // Convenience local
  //
  TMutFitPar local_fit_par;
  local_fit_par.set_x(_parameters[0]);
  local_fit_par.set_y(_parameters[1]);
  local_fit_par.set_dxdz(_parameters[2]);
  local_fit_par.set_dydz(_parameters[3]);
  local_fit_par.set_z(_z);

  // Update the covariance matrix
  //
  for(size_t i=0;i<get_n_par();++i){
    for(size_t j=0;j<get_n_par();++j){
      local_fit_par.set_covar(i,j,gsl_matrix_get(_covar,i,j));
    }
  }
  
  // Update track
  //
  trk_ptr->get()->set_fit_par(local_fit_par);
  
}

//________________________________________________________________
void TMutLinearModel::finish(TMutStubMap::pointer stub_ptr, double z)
{
  // Extrapolate to given z
  //
  value_array vertex_par = extrapolate(z);
  
  // Convenience local
  //
  TMutFitPar local_fit_par;
  local_fit_par.set_x(vertex_par[0]);
  local_fit_par.set_y(vertex_par[1]);
  local_fit_par.set_dxdz(vertex_par[2]);
  local_fit_par.set_dydz(vertex_par[3]);
  local_fit_par.set_z(z);
  local_fit_par.set_z_begin(z);
  local_fit_par.set_z_end(stub_ptr->get()->get_fit_par()->get_z_end());
  // Update the covariance matrix
  //
  for(size_t i=0;i<get_n_par();++i){
    for(size_t j=0;j<get_n_par();++j){
      local_fit_par.set_covar(i,j,gsl_matrix_get(_covar,i,j));
    }
  }
  // Update track
  //
  stub_ptr->get()->set_fit_par(&local_fit_par);  
}

//________________________________________________________________
void TMutLinearModel::finish(TMutStubMap::pointer stub_ptr)
{
  // Convenience local
  //
  TMutFitPar local_fit_par;
  local_fit_par.set_x(_parameters[0]);
  local_fit_par.set_y(_parameters[1]);
  local_fit_par.set_dxdz(_parameters[2]);
  local_fit_par.set_dydz(_parameters[3]);
  local_fit_par.set_z(_z);
  local_fit_par.set_z_begin(_z);
  local_fit_par.set_z_end(stub_ptr->get()->get_fit_par()->get_z_end());

  // Update the covariance matrix
  //
  for(size_t i=0;i<get_n_par();++i){
    for(size_t j=0;j<get_n_par();++j){
      local_fit_par.set_covar(i,j,gsl_matrix_get(_covar,i,j));
    }
  }
  
  // Update track
  //
  stub_ptr->get()->set_fit_par(&local_fit_par);  
}



//________________________________________________________________
void TMutLinearModel::update(const gsl_vector* fit_par)
{
  // Update data member parameter array from input vector
  //
  for(unsigned short i=0; i<get_n_par();++i){
    _parameters[i] = gsl_vector_get(fit_par,i);
  }
}

//________________________________________________________________
void TMutLinearModel::update(const gsl_multifit_fdfsolver* solver_ptr) 
{
  // Never trust a raw pointer
  //
  if(solver_ptr==0){
    throw std::invalid_argument(DESCRIPTION("solver_ptr null in update"));
  }
  // Update data member parameter array from input vector
  //
  for(unsigned short i=0; i<get_n_par();++i){
    _parameters[i] = gsl_vector_get(solver_ptr->x,i);
  }
}

//________________________________________________________________
PHPoint TMutLinearModel::extrapolate_point(double z)

{
  // Convenience temporaries
  //
  double x0 = _parameters[0];
  double y0 = _parameters[1];
  double z0 = _z;
  double dxdz = _parameters[2];
  double dydz = _parameters[3];
  
  // Linear extrapolation to z
  //
  double x = x0 + dxdz*(z-z0);
  double y = y0 + dydz*(z-z0);
  
  // return PHPoint by value
  //
  return PHPoint(x,y,z);
}

//________________________________________________________________
/*! Returns the i'th parameter.  If i >= N_LINEARMut_PAR throws */ 
double TMutLinearModel::get_parameter(unsigned short i) const
{
  if(i>=N_LINEAR_PAR) throw std::invalid_argument(DESCRIPTION("Invalid parameter index"));
  return _parameters[i];
}

//________________________________________________________________
/*! Sets the i'th parameter.  If i >= N_LINEARMut_PAR throws */ 
void TMutLinearModel::set_parameter(unsigned short i, double value) 
{
  if(i>=N_LINEAR_PAR) throw std::invalid_argument(DESCRIPTION("Invalid parameter index"));
  _parameters[i] = value;
}

//________________________________________________________________
/*! Returns the parameter vector by value */
TMutLinearModel::value_array
TMutLinearModel::extrapolate(double z)
{
  // Extrapolate linear parameters to requested z
  //
  PHPoint trk_point = extrapolate_point(z);
  
  value_array new_parameters(N_LINEAR_PAR,0.0);
  new_parameters[0] = trk_point.getX();
  new_parameters[1] = trk_point.getY();
  new_parameters[2] = _parameters[2];
  new_parameters[3] = _parameters[3];
  return new_parameters;
}

//________________________________________________________________
/*! Returns the i'th row of the track model Jacobian */
const TMutLinearModel::value_array& TMutLinearModel::jacobian(unsigned short irow)
{ return _jacobian[irow]; }

//________________________________________________________________
/*! Evaluates and internally caches the Jacobian at given z */
void TMutLinearModel::calculate_jacobian(double z)
{
  PHPoint trk_point = extrapolate_point(z);
  // In all of the comments below X = {x,y,dxdz,dydz}
  //
  for(size_t irow=0; irow<N_LINEAR_PAR;++irow) {
    std::fill(_jacobian[irow].begin(), _jacobian[irow].end(),0);
    if(irow == 0) {
      // dx/dX = {1, 0, (z-z0), 0}  
      //
      _jacobian[irow][0] = 1;
      _jacobian[irow][2] = z-_z;
    } else if (irow == 1) {    
      // dy/dX = {0, 1, 0, (z-z0)}
      //
      _jacobian[irow][1] = 1;
      _jacobian[irow][3] = z-_z;
    } else if (irow == 2) {
      // d(dxdz)/dX = {0, 0, 1, 0}
      //
      _jacobian[irow][2] = 1;
    } else if (irow == 3) {
      // d(dydz)/dX = {0, 0, 0, 1}
      // 
      _jacobian[irow][3] = 1;
    }
  }
}

//________________________________________________________________
void TMutLinearModel::set_covariance(const gsl_matrix* covar_ptr) 
{
  if(covar_ptr==0){
    throw std::invalid_argument(DESCRIPTION("covar_ptr null in set_covariance"));
  }
  gsl_matrix_memcpy(_covar,covar_ptr);
}

//________________________________________________________________
void TMutLinearModel::set_covariance(const gsl_multifit_fdfsolver* solver_ptr) 
{
  // Never trust a raw pointer
  //
  if(solver_ptr==0){
    throw std::invalid_argument(DESCRIPTION("solver_ptr null in set_covariance"));
  }
  // This call writes the current solvers covariance matrix into _covar matrix
  //
#if GSL_MAJOR_VERSION == 2
  gsl_matrix * J = gsl_matrix_alloc(solver_ptr->fdf->n, solver_ptr->fdf->p);
  gsl_multifit_fdfsolver_jac(const_cast<gsl_multifit_fdfsolver *>(solver_ptr), J);
  gsl_multifit_covar(J, 0.0, _covar);

  // free previousely allocated memory
  gsl_matrix_free (J);
#else
  gsl_multifit_covar (solver_ptr->J, 0.0, _covar);
#endif
}

//________________________________________________________________
const gsl_matrix* TMutLinearModel::get_covariance() const 
{ return _covar; }

//________________________________________________________________
const gsl_matrix* TMutLinearModel::extrapolate_covariance(double z) 
{ return _covar; }
