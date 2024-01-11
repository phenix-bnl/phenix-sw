// Class : TFvtxStraightTrkPar
// Author: D. Winter
// Date: 4/27/08
// Description: Trivial v1 implementation of TFvtxStraightTrkPar.  All work done by base class.

#ifndef __TFVTXSTRAIGHTTRKPAR_V1_H__
#define __TFVTXSTRAIGHTTRKPAR_V1_H__

#include <TFvtxStraightTrkPar.h>

class TFvtxStraightTrkPar_v1 : public TFvtxStraightTrkPar
{
public:

  TFvtxStraightTrkPar_v1() {}

  //! default constructor
  TFvtxStraightTrkPar_v1(const int arm, 
			 const double x=0, 
			 const double y=0, 
			 const double mx=0, 
			 const double my=0, 
			 const double chi_square=0,
			 const int ndf = 0) : 
    _index(0),
    _arm(arm),
    _x(x),
    _y(y),
    _mx(mx),
    _my(my),
    _chi_square(chi_square),
    _ndf(ndf)
  {
    // initialize covariance matrix to 0
    //
    std::fill(_covar,_covar+COVAR_SIZE,0);
  }
  
  //! constructor
  TFvtxStraightTrkPar_v1(const Key key, const unsigned short arm, const unsigned short index) :
    TFvtxStraightTrkPar(key),
    _index(index),
    _arm(arm),
    _x(0.0),
    _y(0.0),
    _mx(0.0),
    _my(0.0),
    _chi_square(0.0),
    _ndf(0)
  {
  }

  //! constructor
  TFvtxStraightTrkPar_v1(const TFvtxStraightTrkPar* ptr) :
    TFvtxStraightTrkPar(*ptr)
  {
  }

  //! constructor
  TFvtxStraightTrkPar_v1(const TFvtxStraightTrkPar& ref) :
    TFvtxStraightTrkPar(ref)
  {}

  //! arm of track
  virtual int get_arm() const { return _arm; }

  /*! Index [0,1024] */
  virtual unsigned short get_index() const {return _index; }
  
 //! x position (perp to the beam, horizontal)  
  virtual double get_x() const { return _x;}
  
  //! y position (perp to the beam, vertical)
  virtual double get_y() const { return _y;}
  
  //! momentum along x  
  virtual double get_mx() const { return _mx;}

  //! momentum along y  
  virtual double get_my() const { return _my;}

  //! slope in xz plane  
  virtual double get_dxdz() const { return _mx; }

  //! slope in yz plane  
  virtual double get_dydz() const { return _my; }

  //! chisquare
  virtual double get_chi_square() const { return _chi_square;}
  
  //! degrees of freedom
  virtual int get_ndf() const { return _ndf; }
  
  //! covariance matrix element
  virtual double get_covar(unsigned short i, unsigned short j) const 
  {
    unsigned short index = i*COVAR_ROW + j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    return _covar[index];
  }
  
  //! arm of track
  virtual void set_arm(const int arm) { _arm = arm; }

  /*! Index [0,1024] */
  virtual void set_index(unsigned short index) { _index = index; }               

  //! x position (perp to the beam, horizontal)  
  virtual void set_x(double x) { _x = x;}
  
  //! y position (perp to the beam, horizontal)  
  virtual void set_y(double y) { _y = y;}
  
  //! 
  virtual void set_mx(double px) { _mx = px;}
  
  //! 
  virtual void set_my(double py) { _my = py;}
  
  //! chisquare
  virtual void set_chi_square(double chi_square) { _chi_square = chi_square;}
  
  //! number of degrees of freedon
  virtual void set_ndf(int ndf) { _ndf = ndf;}
  
  //! covariance matrix element
  virtual void set_covar(unsigned short i, unsigned short j, double val)
  {
    unsigned short index = i*COVAR_ROW+j;
    BOUNDS_CHECK(index,COVAR_SIZE);
    _covar[index] = val;
  }

  //! dump object content to stream
  virtual void print( std::ostream &out = std::cout ) const;

protected:
  unsigned short _index;
  int _arm;
  double _x;
  double _y;
  double _mx;
  double _my;
  double _chi_square;
  int _ndf;
  double _covar[COVAR_SIZE];

  ClassDef(TFvtxStraightTrkPar_v1,1)
};

#endif
