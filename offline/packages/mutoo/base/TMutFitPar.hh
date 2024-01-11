// Class : TMutFitPar
// Author: S.Kelly 
// Date: 4/22/02
// Description: Holds track parameters in geometric units.

#ifndef __TMUTFITPAR_H__
#define __TMUTFITPAR_H__

#include<PHException.h>
#include<TDataType.h>
#include<algorithm>
#include<TObject.h>
#include<PHVector.h>
#include<PHPoint.h>
#include<MUTOO.h>

/*! @ingroup classes */
//!  Stub track parameters (Geometric Units) 
/*!  
Stub track parameters (Geometric Units) <br>
<ul>
<li> <b>x</b> - x position (cm) at z reference.
<li> <b>y</b> - y position (cm) at z reference.
<li> <b>theta</b> - dip angle at z reference (atan(Pt/Pz)).
<li> <b>phi</b> - azimuthal at z reference (atan(Px/Py)).
<li> <b>z</b> - z reference (parameter).
</ul>
*/
class TMutFitPar : public TObject
{
public:

  //! @name Constructors/Destructors
  //@{    
  /*! Construct with parameter set */
  TMutFitPar(double x=0, 
	     double y=0, 
	     double z=0, 
	     double dxdz=0,
	     double dydz=0,
	     double chi_square=0) : 
    _x(x),
    _y(y),
    _z(z),
    _z_begin(0),
    _z_end(0),
    _dxdz(dxdz),
    _dydz(dydz),
    _chi_square(chi_square) 
  {
    // initialize covariance matrix to 0
    //
    std::fill(_covar,_covar+COVAR_SIZE,0);
  }
  /*! Destructor */
  virtual ~TMutFitPar(){;}
  //@}

  //! @name Functional Interface
  //@{    
  /*! x position (cm) at z reference. */
  double get_x() const 
  { return _x;}
  
  /*! y position (cm) at z reference. */
  double get_y() const 
  { return _y;}
  
  /*! z reference */
  double get_z() const 
  { return _z;}
  
  /*! direction tangent dxdz */
  double get_dxdz() const 
  { return _dxdz;}
  
  /*! direction tangent dydz */
  double get_dydz() const 
  { return _dydz;}
  
  /*! drdz */
  double get_drdz() const 
  { return std::sqrt(
    MUTOO::SQUARE(_dxdz) + 
    MUTOO::SQUARE(_dydz)); 
  }
  
  /*! chi square statistic */ 
  double get_chi_square() const 
  { return _chi_square;}
  
  /*! covariance matrix */ 
  double get_covar(unsigned short i, unsigned short j) const 
  {  
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
  }  
  /*! Upstream endpoint of track */
  double get_z_begin() const 
  { return _z_begin;}
  
  /*! Downstream endpoint of track */  
  double get_z_end() const 
  { return _z_end;}
  
  /*! x position (cm) at z reference. */
  void set_x(double x) 
  { _x = x;}
  
  /*! y position (cm) at z reference. */
  void set_y(double y) 
  { _y = y;}
  
  /*! z reference */
  void set_z(double z) 
  { _z = z;}
  
  /*! direction tangent */
  void set_dxdz(double dxdz) 
  { _dxdz = dxdz;}
  
  /*! direction tangent */
  void set_dydz(double dydz) 
  { _dydz = dydz;}
  
  /*! chi square statistic */ 
  void set_chi_square(double chi_square) 
  { _chi_square = chi_square;}
  
  /*! covariance matrix */ 
  void set_covar(unsigned short i, unsigned short j, double val)
  {
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = val;
  }

  /*! Upstream endpoint of track */
  void set_z_begin(double z_begin) 
  { _z_begin = z_begin; }
  
  /*! Downstream endpoint of track */
  void set_z_end(double z_end) 
  { _z_end = z_end; }

  /*! Returns a normalized PHVector tangent to track */
  PHVector get_tangent( unsigned short arm ) const 
  { 
    if( arm == MUTOO::North ) 
		{
      PHVector local(_dxdz,_dydz,1);
      local.normalize();
      return local;
    } else {
      PHVector local(-_dxdz,-_dydz,-1);
      local.normalize();
      return local;
    } 
  }
  
  /*! Return a PHPoint by value */
  PHPoint get_point() const 
  { return PHPoint(_x,_y,_z); }
  //@}

  virtual void print(std::ostream& os = std::cout) const 
  {
    os << " Fit parameters = {";
    os << "x:" << get_x() << ", ";
    os << "y:" << get_y() << ", ";
    os << "dxdz: " << get_dxdz() << ", ";
    os << "dydz: " << get_dydz() << "}" << std::endl;    
    os << " z reference: " << get_z() << std::endl;  
  }

  bool operator==(const TMutFitPar& rhs) const 
  {
    return (_x == rhs._x && _y == rhs._y && _z == rhs._z && _dxdz == rhs._dxdz && _dydz == rhs._dydz);
  }
  
private:				    

  double _x;
  double _y;
  double _z;
  double _z_begin;
  double _z_end;
  double _dxdz;
  double _dydz;
  double _chi_square;
  enum { COVAR_ROW=4, COVAR_SIZE=16 };
  double _covar[COVAR_SIZE];
  ClassDef(TMutFitPar,1)

};

#endif

