//////////////////////////////////////////////////////////////////
//
// Utility class: TMutLinearModel
// Author: S.Kelly 
// Date: 4/16/02
// Description: Linear track model
//              
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUTLINEARMODEL_H__
#define __TMUTLINEARMODEL_H__

#include<TMutTrkMap.h>
#include<TMutStubMap.h>
#include<TMutTrackModel.h>
#include<boost/array.hpp>

//! used for linear extrapolation of a track
class TMutLinearModel : public TMutTrackModel
{
 public:

  /*! Default constructor */
  TMutLinearModel(); 

  /*! Virtual destructor */
  virtual ~TMutLinearModel();
  
  /*! 
    Initialize the linear track model from TMutTrk object.
    The reference surface of the fit is defined at the given z.
  */
  virtual void initialize(TMutTrkMap::const_pointer, double z);

  /*! 
    Initialize the linear track model from TMutTrk object.
   */
  virtual void initialize(TMutTrkMap::const_pointer);

  /*! 
    Initialize the linear track model from TMutStub object.
    The reference surface of the fit is defined at the given z.
  */
  void initialize(TMutStubMap::const_pointer, double z);

  /*! 
    Initialize the linear track model from TMutStub object.
   */
  void initialize(TMutStubMap::const_pointer);

  /*! Update TMutFitPar in given TMutTrk with track model parameters */
  virtual void finish(TMutTrkMap::pointer);  

  /*! Update TMutFitPar in given TMutTrk and extrapolate to given z reference */
  virtual void finish(TMutTrkMap::pointer, double z);  

  /*! Update TMutStub with track model parameters */
  void finish(TMutStubMap::pointer);  

  /*! Update TMutStub and extrapolate to given z reference */
  void finish(TMutStubMap::pointer, double z);  

  /*! Update linear track model parameters from gsl vector */  
  virtual void update(const gsl_vector* fit_par);

  /*! Update linear track model parameters from solver */  
  virtual void update(const gsl_multifit_fdfsolver* solver_ptr);

  /*! Return the intersection of the track with plane at z as PHPoint */
  virtual PHPoint extrapolate_point(double z);

  /*! Return the vector of track parameters extrapolated to z */
  virtual value_array extrapolate(double z);
  
  /*! Calculate the jacobian at given z */
  virtual void calculate_jacobian(double z);
  
  /*! Returns the i'th row of the jacobian */
  virtual const value_array& jacobian(unsigned short irow);
  
  virtual size_t get_n_par() 
	{ return N_LINEAR_PAR; }
  
  virtual double get_parameter(unsigned short i) const ;

  virtual void set_parameter(unsigned short i, double value);

  virtual void set_covariance(const gsl_matrix* covar_ptr);

  virtual void set_covariance(const gsl_multifit_fdfsolver* solver_ptr);

  virtual const gsl_matrix* get_covariance() const;

  virtual const gsl_matrix* extrapolate_covariance(double z);

  double get_x() { return _parameters[0]; }
  double get_y() { return _parameters[1]; }
  double get_theta() { return _parameters[2]; }
  double get_phi() { return _parameters[3]; }

 private:

	//! functor to clear jacobian arrays 
  class clear_ftor {
	  public:
    void operator()(value_array& row) { row.resize(N_LINEAR_PAR,0); }
  };

  //! Clear and resize jacobian array of vectors
  void clear_jacobian() { std::for_each(_jacobian.begin(),_jacobian.end(),clear_ftor()); }

  //! Hide the enumeration
  enum {N_LINEAR_PAR=4};  

	//! track parameters
  value_array _parameters;
	
	//! fit jacobian
  boost::array<value_array,N_LINEAR_PAR> _jacobian;

	//! reference z
  double _z;

	//! error on track parameters
  gsl_matrix* _covar;
};

#endif




