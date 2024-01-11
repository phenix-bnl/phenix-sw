// Class : TMutRecoPar
// Author: S.Kelly 
// Date: 4/22/02
// Description: Minimization parameters

#ifndef __TMUTRECOPAR_H__
#define __TMUTRECOPAR_H__

#include<PHException.h>
#include<TDataType.h>
#include<algorithm>
#include<TObject.h>
#include<PHVector.h>
#include<PHPoint.h>
#include<MUTOO.h>

/*! @ingroup classes */
//!  Reconstruction track parameters 
/*!  
Reconstruction track parameters <br>
<ul>
<li> <b>x</b> - x position (cm) at z reference.
<li> <b>y</b> - y position (cm) at z reference.
<li> <b>phi</b> - atan(py/px)
<li> <b>dzds</b> - pz/ptot
<li> <b>pinv</b> - q/p (charge over total momentum)
<li> <b>z</b> - z reference (parameter).
</ul>
*/
class TMutRecoPar : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{    
    
  /*! Construct with parameter set */
  TMutRecoPar(double x=0, 
	      double y=0, 
	      double z=0, 
	      double phi=0,
	      double dzds=0,
	      double pinv=0,
	      double chi_square=0) : 
    _x(x),
    _y(y),
    _z(z),
    _z_begin(0),
    _z_end(0),
    _phi(phi),
    _dzds(dzds),
    _pinv(pinv),
    _chi_square(chi_square),
    _error_matrix_current(false)
  {
    // initialize covariance matrix to 0
    //
    std::fill(_covar,_covar+COVAR_SIZE,0);
    std::fill(_error,_error+COVAR_SIZE,0);
  }
  /*! Virtual destructor */
  virtual ~TMutRecoPar(){;}
  //@}

  //! @name Functional Interface
  //@{    
  /*! x position (cm) at z reference. */
  virtual double get_x() const { return _x;}
  /*! y position (cm) at z reference. */
  virtual double get_y() const { return _y;}
  /*! z reference */
  virtual double get_z() const { return _z;}
  /*! direction tangent dxdz */
  virtual double get_phi() const { return _phi;}
  /*! direction tangent dydz */
  virtual double get_dzds() const { return _dzds;}
  /*! q/p */
  virtual double get_pinv() const { return _pinv;}
  /*! charge q */
  virtual double get_charge() const { return (_pinv > 0) ? 1 : -1 ;}
  /*! chi square statistic */ 
  virtual double get_chi_square() const { return _chi_square;}
  /*! covariance matrix */ 
  virtual double get_covar(unsigned short i, unsigned short j) const {  
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
  }  
  /*! error matrix (inverse of covariance matrix) */ 
  virtual double get_error(unsigned short i, unsigned short j) const;

  /*! Upstream endpoint of track */
  virtual double get_z_begin() const { return _z_begin;}
  /*! Downstream endpoint of track */  
  virtual double get_z_end() const { return _z_end;}
  
  /*! x position (cm) at z reference. */
  virtual void set_x(double x) { _x = x;}
  /*! y position (cm) at z reference. */
  virtual  void set_y(double y) { _y = y;}
  /*! z reference */
  virtual void set_z(double z) { _z = z;}
  /*! phi angle */
  virtual void set_phi(double phi) { _phi = phi;}
  /*! dzds */
  virtual void set_dzds(double dzds) { _dzds = dzds;}
  /*! charge / total momentum */
  virtual void set_pinv(double pinv) { _pinv = pinv;}
  /*! chi square statistic */ 
  virtual void set_chi_square(double chi_square) { _chi_square = chi_square;}
  /*! covariance matrix */ 
  virtual void set_covar(unsigned short i, unsigned short j, double val){
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = val;
    _error_matrix_current = false;
  }

  /*! Upstream endpoint of track */
  virtual void set_z_begin(double z_begin) { _z_begin = z_begin; }
  /*! Downstream endpoint of track */
  virtual void set_z_end(double z_end) { _z_end = z_end; }
  //@}

  /*! Return a PHPoint by value */
  PHPoint get_point() const 
  {
    return PHPoint(_x,_y,_z);
  }
  //! @name Convert to Physics Units
  //@{    
  /*! px */
  virtual double get_px(){  
    double ptot = std::fabs(1.0/_pinv);  
    double pt = ptot*std::sqrt(1-MUTOO::SQUARE(_dzds)); 
    return pt*std::cos(_phi);
  }
  /*! py */
  virtual double get_py(){
    double ptot = std::fabs(1.0/_pinv);  
    double pt = ptot*std::sqrt(1-MUTOO::SQUARE(_dzds)); 
    return pt*std::sin(_phi);
  }
  /*! pz */
  virtual double get_pz(){
    double ptot = std::fabs(1.0/_pinv);  	
    return ptot*_dzds;
  };
  /*! p total */
  virtual double get_ptot(){ return std::fabs(1.0/_pinv); }
  /*! charge */
  virtual double get_charge() { return (_pinv > 0) ? 1 : -1; }
  //@}

private:				    

  double _x;
  double _y;
  double _z;
  double _z_begin;
  double _z_end;
  double _phi;
  double _dzds;
  double _pinv;
  double _chi_square;
  enum { COVAR_ROW=5, COVAR_SIZE=25 };
  double _covar[COVAR_SIZE];

  void calculate_error_matrix() const;

#ifndef __CINT__
  // non-persistent storage for the error matrix (inverse of covariance) 
  //
  mutable bool _error_matrix_current;
  mutable double _error[COVAR_SIZE];

#endif

  ClassDef(TMutRecoPar,1)    
};

#endif

