
#ifndef __SVXTRACKPROJECTION_H__
#define __SVXTRACKPROJECTION_H__

//========================================
/// \file SvxTrackProj.h
/// \brief Projected track momentum vector
/// \author Mike McCumber & Matt Wysocki
//========================================

// PHENIX includes
#include <PHObject.h>
#include <PHPoint.h>
#include <PHVector.h>

/// \class SvxTrackProj
///
/// \brief Projected track momentum vector
///
class SvxTrackProj : public PHObject
{
 public:
  //! Projection locations enumeration
	/*!
    Important note: one should _never_ remove bits from here, or change bit values
    because it would break backward compatibility. Adding new bits is safe, though.
 	*/

//  	enum Location {
//    PrimaryVertex = 0,
//    DCA = 1,
//    DCA2D = 2,
//    Layer0 = 3,
//    Layer1 = 4,
//    Layer2A = 5,
//    Layer2B = 6,
//    Layer2C = 7,
//    Layer3A = 8,
//    Layer3B = 9,
//    Layer3C = 10,
//    DCH = 11,
//    Undefined = 12
//   };

  typedef int Location;
  static Location PrimaryVertex;
  static Location DCA;
  static Location DCA2D;
  static Location Layer0;
  static Location Layer1;
  static Location Layer2A;
  static Location Layer2B;
  static Location Layer2C;
  static Location Layer3A;
  static Location Layer3B;
  static Location Layer3C;
  static Location DCH;
  static Location Undefined;

  SvxTrackProj( Location loc=Undefined,
                Float_t x=0,
                Float_t y=0,
                Float_t z=0,
                Float_t px=0,
                Float_t py=0,
                Float_t pz=0,
                Float_t chi_square=0 );
  
  virtual ~SvxTrackProj() {};


  void print( std::ostream &out = std::cout ) const {}; ///< dump object content to stream
  
  // generalized reference locations
  virtual Location get_location() const {return Undefined;}
  virtual void set_location(Location location) {return;}
  virtual UShort_t get_layer() const {return 0;}
  
  // projected spacial location
  virtual Float_t get_x() const {return 0.0;} ///< get the x position (perp to the beam, horizontal)  
  virtual void set_x(Float_t x) {return;}     ///< set the x position (perp to the beam, horizontal)  

  virtual Float_t get_y() const {return 0.0;} ///< get the y position (perp to the beam, vertical)  
  virtual void set_y(Float_t y) {return;}     ///< set the y position (perp to the beam, vertical)  

  virtual Float_t get_z() const {return 0.0;} ///< get the z position (along the beam)
  virtual void set_z(Float_t z) {return;}     ///< set the z position (along the beam)

  virtual PHPoint get_point() const {return PHPoint(0.0,0.0,0.0);} ///< 3D space point in x,y,z
  virtual void set_point(PHPoint point) {return;}                        ///< 3D space point in x,y,z

  // projected momentum
  virtual Float_t get_px() const {return 0.0;} ///< momentum along x  
  virtual void set_px(Float_t px) {return;}    ///< momentum along x  

  virtual Float_t get_py() const {return 0.0;} ///< momentum along y
  virtual void set_py(Float_t py) {return;}    ///< momentum along y  

  virtual Float_t get_pz() const {return 0.0;} ///< momentum along z
  virtual void set_pz(Float_t pz) {return;}    ///< momentum along z  

  virtual PHVector get_momentum() const {return PHVector(0.0,0.0,0.0);} ///< 3D momentum vector in px,py,pz
  virtual void set_momentum(const PHVector&) {}                         ///< 3D momentum vector in px,py,pz
  virtual Float_t get_ptot() const {return 0.0;}                        ///< scalar momentum value

  // quality
  virtual Float_t get_chi_square() const {return 0.0;}      ///< chi-square contribution from this projection
  virtual void set_chi_square(Float_t chi_square) {return;} ///< set the chi-square contribution from this projection

  // error projections
  virtual Float_t get_covar(UShort_t i, UShort_t j) const {return 0.0;} ///< get a convariance matrix element  
  virtual void set_covar(UShort_t i, UShort_t j, Float_t val) {return;} ///< set a convariance matrix element

 private:
  ClassDef(SvxTrackProj,1)
};

#endif // __SVXTRACKPROJECTION_H__
