// Author: D. Winter
// Date: 4/27/08
// Description: Holds track parameters of a quadratic-line fit in physics units.
// In order to avoid adding members and bookkeeping to the TFvtxTrk object, we
// make this class a PHKey.  That way we can transparently associate the result
// with the track.  Perhaps in the future we will add it as a member to the track
// object.

#ifndef __TFVTXQUADRATICTRKPAR_H__
#define __TFVTXQUADRATICTRKPAR_H__

#include <PHKey.hh>
#include <PHException.h>
#include <algorithm>
#include <FVTXOO.h>
#include <cmath>

//class TFvtxQuadraticTrkPar : public TObject
class TFvtxQuadraticTrkPar : public PHKey
{
public:

  //! Default constructor
  TFvtxQuadraticTrkPar() : 
    _index(0),
    _arm(-1),
    _ax(-9999),
    _ay(-9999),
    _bx(-9999),
    _by(-9999),
    _cx(-9999),
    _cy(-9999),
    _chi_square(-9999),
    _ndf(-1)
  {} 

  /*! Construct with key */
  TFvtxQuadraticTrkPar(const Key& key) : 
    PHKey(key),
    _index(0),
    _arm(-1),
    _ax(-9999),
    _ay(-9999),
    _bx(-9999),
    _by(-9999),
    _cx(-9999),
    _cy(-9999),
    _chi_square(-9999),
    _ndf(-1)
  {} 

  //! constructor
  TFvtxQuadraticTrkPar(const Key, const unsigned short arm, const unsigned short index);

  TFvtxQuadraticTrkPar(const int arm,
		       const double ax=0, 
		       const double ay=0, 
		       const double bx=0, 
		       const double by=0, 
		       const double cx=0,
		       const double cy=0,
		       const double chi_square=0,
		       const int ndf = 0) : 
    _index(0),
    _arm(arm),
    _ax(ax),
    _ay(ay),
    _bx(bx),
    _by(by),
    _cx(cx),
    _cy(cy),
    _chi_square(chi_square),
    _ndf(ndf)
  {
    // initialize covariance matrix to 0
    //
    std::fill(_covar,_covar+COVAR_SIZE,0);
  }
  
  virtual ~TFvtxQuadraticTrkPar() {}

  //! arm of track
  virtual int get_arm() const { return _arm; }

  /*! Index [0,1024] */
  virtual unsigned short get_index() const {return _index; }
  
 //! x position (perp to the beam, horizontal)  
  virtual double get_ax() const { return _ax;}
  
  //! y position (perp to the beam, vertical)
  virtual double get_ay() const { return _ay;}
  
  //! momentum along x  
  virtual double get_bx() const { return _bx;}

  //! momentum along y  
  virtual double get_by() const { return _by;}

  //! slope in xz plane  
  virtual double get_cx() const { return _cx; }

  //! slope in yz plane  
  virtual double get_cy() const { return _cy; }

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
  virtual void set_ax(double ax) { _ax = ax;}
  
  //! y position (perp to the beam, horizontal)  
  virtual void set_ay(double ay) { _ay = ay;}
  
  //! 
  virtual void set_bx(double bx) { _bx = bx;}
  
  //! 
  virtual void set_by(double by) { _by = by;}

  //! 
  virtual void set_cx(double cx) { _cx = cx;}
  
  //! 
  virtual void set_cy(double cy) { _cy = cy;}
  
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
  enum { COVAR_ROW=6, COVAR_SIZE=36 };
  
protected:
  unsigned short _index;
  int _arm;
  double _ax;
  double _ay;
  double _bx;
  double _by;
  double _cx;
  double _cy;
  double _chi_square;
  int _ndf;
  double _covar[COVAR_SIZE];
  ClassDef(TFvtxQuadraticTrkPar,1)
};

#endif
