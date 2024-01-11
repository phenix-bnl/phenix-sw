
#ifndef __SVXTRACKFIT_H__
#define __SVXTRACKFIT_H__

//===================================
/// \file SvxTrackFit.h
/// \brief Fit object to the SvxTrack
/// \author Michael P. McCumber
//===================================

// standard includes
#include <vector>

// PHENIX includes
#include <PHObject.h>

// Svx includes
#include "SvxTrackProj.h"

/// \class SvxTrackFit
///
/// \brief A fit object to the SvxTrack
///
/// This object contains the result of one
/// particular type of fit to the track
/// clusters. Fits should fill a list of
/// projections at the relavant reference
/// radii for the VTX.
///
class SvxTrackFit : public PHObject
{
 public:
  /// fit type enumeration
	/*!
    Important note: one shoud _never_ remove bits from here, or change bit values
    because it would break backward compatibility. Adding new bits is safe, though.
 	*/
 	enum FitType {
    Undefined=0,
    OldSvxTracker=1,
    Kalman=2,
    FastHelix=3
  };

  // unversioned constructor/destructor
  SvxTrackFit() {};
  virtual ~SvxTrackFit() {};

  virtual void print(std::ostream& os = std::cout) const {}
  
  // fit type
  virtual FitType get_fit_type() const {return Undefined;}
  virtual void set_fit_type(FitType fit_type) {}

  // projection access methods
  virtual bool has_projection(SvxTrackProj::Location location) const {return false;}
  virtual const SvxTrackProj* get_projection(SvxTrackProj::Location location) const {return 0;}
  virtual const SvxTrackProj* get_innermost_projection() const {return 0;}
  virtual const SvxTrackProj* get_outermost_projection() const {return 0;}
  virtual void set_projection(const SvxTrackProj& projection) {return;}
  virtual size_t get_n_projections() const {return 0;}
  virtual void clear_projections() {return;}
  virtual std::vector<SvxTrackProj*> get_projections() const
  {return std::vector<SvxTrackProj*>();}
  
  // track quality
  virtual Float_t get_chi_square() const {return 0.0;}
  virtual void set_chi_square(Float_t chi_square) {return;}
  virtual Float_t get_chi_square_pdf() const {return 0.0;}

  // track charge
  virtual Float_t get_charge() const {return 0;}
  virtual void set_charge(Float_t charge) {return;}

 private:	
  ClassDef(SvxTrackFit,1)
};
  
#endif // __SVXTRACKFIT_H__
