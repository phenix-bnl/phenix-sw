// Class : TMutTrkPar
// Author: S.Kelly 
// Date: 4/22/02
// Description: Holds track parameters in physics units.

#ifndef __TMUTTRKPAR_H__
#define __TMUTTRKPAR_H__

#include <TDataType.h>
#include <PHException.h>
#include <algorithm>
#include <TObject.h>
#include <MUTOO.h>
#include <iostream>
#include <cmath>

#include <PHPoint.h>
#include <PHVector.h>

/*! @ingroup classes */
//!  Track parameters (Physics Units) 
/*!  
Track parameters (Physics Units) <br>
<ul>
<li> <b>x</b> - x position (cm) at z reference.
<li> <b>y</b> - y position (cm) at z reference.
<li> <b>px</b> - momentum (GeV/c) at z reference.
<li> <b>py</b> - momentum (GeV/c) at z reference.
<li> <b>pz</b> - momentum (GeV/c) at z reference.
<li> <b>z</b> - z reference (parameter).
</ul>
*/
class TMutTrkPar : public TObject
{
public:

  //! default constructor
  TMutTrkPar(
    double x=0, 
    double y=0, 
    double z=0, 
    double px=0, 
    double py=0, 
    double pz=0, 
    int    charge=0,
    double chi_square=0) : 
    _x(x),
    _y(y),
    _z(z),
    _px(px),
    _py(py),
    _pz(pz),
    _charge(charge),
    _chi_square(chi_square) 
  {
    // initialize covariance matrix to 0
    //
    std::fill(_covar,_covar+COVAR_SIZE,0);
  }
  
  //! x position (perp to the beam, horizontal)  
  double get_x() const 
  { return _x;}
  
  //! y position (perp to the beam, vertical)
  double get_y() const 
  { return _y;}
  
  //! z position (along the beam)
  double get_z() const 
  { return _z;}

  //! point
  PHPoint get_point() const
  { return PHPoint( _x, _y, _z ); }

  //! momentum along x  
  double get_px() const 
  { return _px;}

  //! momentum along y  
  double get_py() const 
  { return _py;}

  //! momentum along z  
  double get_pz() const 
  { return _pz;}

  //! momentum vector
  PHVector get_momentum() const
  { return PHVector( _px, _py, _pz ); }

  //! slope in xz plane  
  double get_dxdz() const 
  { return _px/_pz; }

  //! slope in yz plane  
  double get_dydz() const 
  { return _py/_pz; }

  /*! \brief 
    Returns a normalized PHVector tangent to track 
    the third coordinate sign is choosen from the arm argument
  */
  PHVector get_tangent( unsigned short arm ) const 
  { 
    PHVector local( _px/_pz, _py/_pz, 1 );
    local.normalize();
    if( arm == MUTOO::South ) local = local*(-1);
    return local;
  }

  //! charge 
  int get_charge() const 
  { return _charge;}
  
  //! total momentum
  double get_ptot() const 
  { 
    return std::sqrt(
      MUTOO::SQUARE(_px) + 
      MUTOO::SQUARE(_py) +
      MUTOO::SQUARE(_pz));
  }
  
  //! chisquare
  double get_chi_square() const 
  { return _chi_square;}
  
  //! covariance matrix element
  double get_covar(unsigned short i, unsigned short j) const 
  {
    unsigned short index = i*COVAR_ROW + j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
  }
  
  //! x position (perp to the beam, horizontal)  
  void set_x(double x) 
  { _x = x;}
  
  //! y position (perp to the beam, horizontal)  
  void set_y(double y) 
  { _y = y;}
  
  //! z position (perp to the beam, horizontal)  
  void set_z(double z) 
  { _z = z;}
  
  //! momentum along x  
  void set_px(double px) 
  { _px = px;}
  
  //! momentum along y  
  void set_py(double py)
  { _py = py;}
  
  //! momentum along z  
  void set_pz(double pz) 
  { _pz = pz;}
  
  //! charge 
  void set_charge(int charge) 
  { _charge = charge;}
  
  //! chisquare
  void set_chi_square(double chi_square) 
  { _chi_square = chi_square;}
  
  //! covariance matrix element
  void set_covar(unsigned short i, unsigned short j, double val)
  {
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = val;
  }
  
  //! dump object content to stream
  void print( std::ostream &out = std::cout ) const;
  enum { COVAR_ROW=5, COVAR_SIZE=25 };
  
private:
  double _x;
  double _y;
  double _z;
  double _px;
  double _py;
  double _pz;
  int _charge;
  double _chi_square;
  double _covar[COVAR_SIZE];
  ClassDef(TMutTrkPar,1)
};

#endif
