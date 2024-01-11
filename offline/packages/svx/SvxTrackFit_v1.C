
#include "SvxTrackFit_v1.h"

#include "SvxTrackFit.h"

ClassImp(SvxTrackFit_v1);

using namespace std;

//_____________________________________________________________________
SvxTrackFit_v1::SvxTrackFit_v1(FitType type) :
  SvxTrackFit(),
  _fit_type(type),
  _chi_square(-1),
  _charge(0)
{}

//_____________________________________________________________________
// SvxTrackFit_v1::SvxTrackFit_v1(const SvxTrackFit* base_ptr) :
//   SvxTrackFit(*base_ptr),
//   _chi_square(base_ptr->get_chi_square()),
//   _charge(base_ptr->get_charge())
// {
//   vector<SvxTrackProj*> projlist = base_ptr->get_projections();
//   for(unsigned int i=0; i<projlist.size(); i++)
//     _projection_list.push_back(*projlist[i]);
// }

// //_____________________________________________________________________
// SvxTrackFit_v1::SvxTrackFit_v1(const SvxTrackFit& base_ref) :
//   SvxTrackFit(base_ref),
//   _chi_square(base_ref.get_chi_square()),
//   _charge(base_ref.get_charge())
// {
//   vector<SvxTrackProj*> projlist = base_ref.get_projections();
//   for(unsigned int i=0; i<projlist.size(); i++)
//     _projection_list.push_back(*projlist[i]);
// }

//_____________________________________________________________________
bool SvxTrackFit_v1::has_projection(SvxTrackProj::Location location) const
{
  // loop over the stored projections and look for the desired code
  for(unsigned int i=0; i<get_n_projections(); i++) {
    if(_projection_list[i].get_location() == location) {
      return true;
    }
  }

  // if the projection isn't found return zero
  return false;
}

//_____________________________________________________________________
const SvxTrackProj* SvxTrackFit_v1::get_projection(SvxTrackProj::Location location) const
{
  // loop over the stored projections and look for the desired code
  for(unsigned int i=0; i<get_n_projections(); i++) {
    if(_projection_list[i].get_location() == location) {
      return &(_projection_list[i]);
    }
  }

  // if the projection isn't found return zero
  return 0;
}

//_____________________________________________________________________
const SvxTrackProj* SvxTrackFit_v1::get_innermost_projection() const
{
  if(_projection_list.size() == 0)
    return 0;
  
  SvxTrackProj::Location location = SvxTrackProj::Undefined; // Undefined == 12
  unsigned int besti = 0;

  // loop over the stored projections and look for the desired code
  for(unsigned int i=0; i<get_n_projections(); i++) {
    if(_projection_list[i].get_location() < location) {
      besti = i;
      location = _projection_list[i].get_location();
    }
  }

  return &(_projection_list[besti]);
}

//_____________________________________________________________________
const SvxTrackProj* SvxTrackFit_v1::get_outermost_projection() const
{
  if(_projection_list.size() == 0)
    return 0;
  
  SvxTrackProj::Location location = 0;
  unsigned int besti = 0;

  // loop over the stored projections and look for the desired code
  for(unsigned int i=0; i<get_n_projections(); i++) {
    if(_projection_list[i].get_location() > location) {
      besti = i;
      location = _projection_list[i].get_location();
    }
  }

  return &(_projection_list[besti]);
}

//_____________________________________________________________________
void SvxTrackFit_v1::set_projection(const SvxTrackProj_v1& projection)
{
  // loop over the stored fits and replace the type if already existing
  for(unsigned int i=0; i<get_n_projections(); i++) {
    if(_projection_list[i].get_location() == projection.get_location()) {
      _projection_list[i] = projection;
      return;
    }
  }

  // otherwise add to the list of fits as a new entry
  _projection_list.push_back(projection);
  return;
}

//_____________________________________________________________________
void SvxTrackFit_v1::print(std::ostream& os) const
{
  cout << "Fit type: ";
  switch(_fit_type) {
  case Undefined:
    cout << "Undefined";
    break;
  case OldSvxTracker:
    cout << "OldSvxTracker";
    break;
  case Kalman:
    cout << "Kalman";
    break;
  case FastHelix:
    cout << "FastHelix";
    break;
  default:
    cout << "Unknown type: " << _fit_type << endl;
    break;
  };
  cout << endl;
  cout.setf(ios::showpos);
  cout << " charge: " << _charge << endl;
  cout.unsetf(ios::showpos);
  cout << " chisquared: " << _chi_square << endl;

  cout << " " << _projection_list.size() << " projection points for this fit"
       << (_projection_list.size() ? ":" : ".") << endl;
  for(unsigned int iproj=0; iproj<_projection_list.size(); iproj++) {
    _projection_list[iproj].print();
  }
  return;
}
