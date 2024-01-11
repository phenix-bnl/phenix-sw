
#ifndef __SVXTRACKPROJECTION_V1_H__
#define __SVXTRACKPROJECTION_V1_H__

//========================================
/// \file SvxTrackProj_v1.h
/// \brief Projected track momentum vector
/// \author Matt Wysocki
//========================================

// PHENIX includes
#include <PHPoint.h>
#include <PHVector.h>

// Svx includes
#include "SvxTrackProj.h"

/// \class SvxTrackProj_v1
///
/// \brief Projected track momentum vector
///
class SvxTrackProj_v1 : public SvxTrackProj
{
 public:
  SvxTrackProj_v1( Location location=Undefined,
                   Float_t x=0, 
                   Float_t y=0, 
                   Float_t z=0, 
                   Float_t px=0, 
                   Float_t py=0, 
                   Float_t pz=0, 
                   Float_t chi_square=0 );
  
  SvxTrackProj_v1(const SvxTrackProj*);
  SvxTrackProj_v1(const SvxTrackProj&);
  virtual ~SvxTrackProj_v1() {};
  
  void print( std::ostream &out = std::cout ) const; ///< dump object content to stream
  
  // generalized reference locations
  Location get_location() const {return _location;}
  void set_location(Location location) {_location = location;}
  
  // projected spacial location
  Float_t get_x() const {return _x;} ///< get the x position (perp to the beam, horizontal)  
  void set_x(Float_t x) {_x = x;}    ///< set the x position (perp to the beam, horizontal)  

  Float_t get_y() const {return _y;} ///< get the y position (perp to the beam, vertical)  
  void set_y(Float_t y) {_y = y;}    ///< set the y position (perp to the beam, vertical)  

  Float_t get_z() const {return _z;} ///< get the z position (along the beam)
  void set_z(Float_t z) {_z = z;}    ///< set the z position (along the beam)

  PHPoint get_point() const {return PHPoint(_x,_y,_z);}                                    ///< 3D space point in x,y,z
  void set_point(PHPoint point) {_x = point.getX(); _y = point.getY(); _z = point.getZ();} ///< 3D space point in x,y,z

  // projected momentum
  Float_t get_px() const {return _px;} ///< momentum along x  
  void set_px(Float_t px) {_px = px;}  ///< momentum along x  

  Float_t get_py() const {return _py;} ///< momentum along y
  void set_py(Float_t py) {_py = py;}  ///< momentum along y  

  Float_t get_pz() const {return _pz;} ///< momentum along z
  void set_pz(Float_t pz) {_pz = pz;}  ///< momentum along z  

  PHVector get_momentum() const {return PHVector(_px,_py,_pz);}                                     ///< 3D momentum vector in px,py,pz
  void set_momentum(const PHVector& vector) {
    _px = vector.getX();
    _y = vector.getY();
    _z = vector.getZ();
  }                         ///< 3D momentum vector in px,py,pz
  Float_t get_ptot() const; ///< scalar momentum value

  // quality
  Float_t get_chi_square() const {return _chi_square;}                ///< chi-square contribution from this projection
  void set_chi_square(Float_t chi_square) {_chi_square = chi_square;} ///< set the chi-square contribution from this projection

  // error projections
  Float_t get_covar(UShort_t i, UShort_t j) const;     ///< get a convariance matrix element 
  void set_covar(UShort_t i, UShort_t j, Float_t val); ///< set a convariance matrix element

  enum { COVAR_ROW=5, COVAR_SIZE=25 }; ///< dimensions of the covariance matrix

 protected:
  Location _location;
  Float_t _x;
  Float_t _y;
  Float_t _z;
  Float_t _px;
  Float_t _py;
  Float_t _pz;
  Float_t _chi_square;
  Float_t _covar[25];

 private:
  ClassDef(SvxTrackProj_v1,1)
};

#endif
