// Class : TFvtxStraightTrkPar
// Author: D. Winter
// Date: 4/27/08
// Description: Holds track parameters of a straight-line fit in physics units.
// In order to avoid adding members and bookkeeping to the TFvtxTrk object, we
// make this class a PHKey.  That way we can transparently associate the result
// with the track.  Perhaps in the future we will add it as a member to the track
// object.

#ifndef __TFVTXSTRAIGHTTRKPAR_H__
#define __TFVTXSTRAIGHTTRKPAR_H__

#include <PHKey.hh>
#include <PHException.h>
#include <algorithm>
#include <FVTXOO.h>
#include <cmath>

/*! @ingroup classes */
//!  Track parameters (Physics Units) 
/*!  
Track parameters (Physics Units) <br>
<ul>
<li> <b>x</b> - x intercept (cm)
<li> <b>y</b> - y intercept (cm)
<li> <b>mx</b> - x slope (unitless)
<li> <b>my</b> - y slope (unitless)
</ul>
*/
class TFvtxStraightTrkPar : public PHKey
{
public:

  //! Default constructor
  TFvtxStraightTrkPar() {} 

  /*! Construct with key */
  TFvtxStraightTrkPar(const Key& key) {} 
  
  virtual ~TFvtxStraightTrkPar() {}

  //! arm of track
  virtual int get_arm() const { return 0; }

  /*! Index [0,1024] */
  virtual unsigned short get_index() const {return 0; }
  
  //! x position (perp to the beam, horizontal)  
  virtual double get_x() const { return 0.0;}
  
  //! y position (perp to the beam, vertical)
  virtual double get_y() const { return 0.0;}
  
  //! momentum along x  
  virtual double get_mx() const { return 0.0;}

  //! momentum along y  
  virtual double get_my() const { return 0.0;}

  //! slope in xz plane  
  virtual double get_dxdz() const { return 0.0; }

  //! slope in yz plane  
  virtual double get_dydz() const { return 0.0; }

  //! chisquare
  virtual double get_chi_square() const { return 0.0; }
  
  //! degrees of freedom
  virtual int get_ndf() const { return 0; }
  
  //! covariance matrix element
  virtual double get_covar(unsigned short i, unsigned short j) const { return 0.0; }
  
  //! arm of track
  virtual void set_arm(const int arm) {}

  /*! Index [0,1024] */
  virtual void set_index(unsigned short index) {}               

  //! x position (perp to the beam, horizontal)  
  virtual void set_x(double x) {}
  
  //! y position (perp to the beam, horizontal)  
  virtual void set_y(double y) {}
  
  //! 
  virtual void set_mx(double px) {}
  
  //! 
  virtual void set_my(double py) {}
  
  //! chisquare
  virtual void set_chi_square(double chi_square) {}
  
  //! number of degrees of freedon
  virtual void set_ndf(int ndf) {}
  
  //! covariance matrix element
  virtual void set_covar(unsigned short i, unsigned short j, double val) {}

  //! dump object content to stream
  virtual void print( std::ostream &out = std::cout ) const {}

  enum { COVAR_ROW=4, COVAR_SIZE=16 };
  
protected:
  ClassDef(TFvtxStraightTrkPar,1)
};

#endif
