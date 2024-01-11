//////////////////////////////////////////////////////////////////
//
// Utility class: TMutTrackModel
// Author: S.Kelly 
// Date: 4/16/02
// Description: ABC for MUTR track model
//              
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUTTRACKMODEL_H__
#define __TMUTTRACKMODEL_H__

#include<TMutTrkMap.h>
#include<gsl/gsl_vector.h>
#include<gsl/gsl_matrix.h>
#include<gsl/gsl_multifit_nlin.h>
#include <stdexcept>
#include<PHPoint.h>

class TMutTrackModel
{
 public:

  virtual ~TMutTrackModel() {}

  typedef std::vector<double> value_array;  

  virtual void update(const gsl_vector* fit_par) = 0;
 
  virtual void update(const gsl_multifit_fdfsolver* solver_ptr) = 0;

  virtual void initialize(TMutTrkMap::const_pointer, double z) = 0;

  virtual void initialize(TMutTrkMap::const_pointer) = 0;

  virtual void finish(TMutTrkMap::pointer, double z) = 0;

  virtual void finish(TMutTrkMap::pointer) = 0;
  
  virtual PHPoint extrapolate_point(double z) = 0;

  virtual value_array extrapolate(double z) = 0;
 
  virtual void calculate_jacobian(double z) = 0;
  
  virtual const value_array& jacobian(unsigned short irow) = 0;
  
  virtual size_t get_n_par() = 0;

  virtual double get_parameter(unsigned short i) const = 0;

  virtual void set_parameter(unsigned short i, double value) = 0;
  
  virtual void set_covariance(const gsl_matrix* covar) = 0;

  virtual void set_covariance(const gsl_multifit_fdfsolver* solver_ptr) = 0;
  
  virtual const gsl_matrix* get_covariance() const = 0;

  virtual const gsl_matrix* extrapolate_covariance(double z) = 0;

  struct abort_track : public std::runtime_error
  {
    abort_track(const std::string& description) : std::runtime_error(description){;}
  };

};

#endif



