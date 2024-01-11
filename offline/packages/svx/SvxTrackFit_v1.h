
#ifndef __SVXTRACKFIT_V1_H__
#define __SVXTRACKFIT_V1_H__

//===================================
/// \file SvxTrackFit_v1.h
/// \brief Fit object to the SvxTrack
/// \author Michael P. McCumber
//===================================

// standard includes
#include <vector>

// ROOT includes
#include <TObject.h>

// Svx includes
#include "SvxTrackFit.h"
#include "SvxTrackProj_v1.h"

/// \class SvxTrackFit_v1
///
/// \brief A fit object to the SvxTrack
///
/// This object contains the result of one
/// particular type of fit to the track
/// clusters. Fits should fill a list of
/// projections at the relavant reference
/// radii for the VTX.
///
class SvxTrackFit_v1 : public SvxTrackFit
{
 public:
  // type defines
  typedef std::vector<SvxTrackProj_v1> projection_list_type;
  typedef projection_list_type::iterator projection_iterator;
  typedef projection_list_type::const_iterator const_projection_iterator;

  // versioned constructor/destructor
  SvxTrackFit_v1(FitType type=Undefined);
  virtual ~SvxTrackFit_v1() {}

  virtual void print(std::ostream& os = std::cout) const;

  // fit type
  FitType get_fit_type() const {return _fit_type;}
  void set_fit_type(FitType fit_type) {_fit_type = fit_type;}

  // projection access methods
  bool has_projection(SvxTrackProj::Location location) const;
  const SvxTrackProj* get_projection(SvxTrackProj::Location location) const;
  const SvxTrackProj* get_innermost_projection() const;
  const SvxTrackProj* get_outermost_projection() const;
  void set_projection(const SvxTrackProj_v1& projection);
  void set_projection(const SvxTrackProj&) { PHOOL_VIRTUAL_WARNING; }
  size_t get_n_projections() const {return _projection_list.size();}
  void clear_projections() {_projection_list.clear();}
  
  // track quality
  Float_t get_chi_square() const {return _chi_square;}
  void set_chi_square(Float_t chi_square) {_chi_square = chi_square;}

  // track charge
  Float_t get_charge() const {return _charge;}
  void set_charge(Float_t charge) {_charge = charge;}

 protected:
  FitType _fit_type;
  Float_t _chi_square;
  Float_t _charge;

  projection_list_type _projection_list; ///< list of track projections

 private:
  ClassDef(SvxTrackFit_v1,1)
};
  
#endif // __SVXTRACKFIT_V1_H__
