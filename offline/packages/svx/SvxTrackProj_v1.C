
#include "SvxTrackProj_v1.h"

#include <iostream>

ClassImp(SvxTrackProj_v1);

using namespace std;

//________________________________________________________________
SvxTrackProj_v1::SvxTrackProj_v1( Location location,
                                  Float_t x, 
                                  Float_t y, 
                                  Float_t z, 
                                  Float_t px, 
                                  Float_t py, 
                                  Float_t pz, 
                                  Float_t chi_square ) : 
  _location(location),
  _x(x),
  _y(y),
  _z(z),
  _px(px),
  _py(py),
  _pz(pz),
  _chi_square(chi_square) 
{
  // initialize covariance matrix to 0
  //
  std::fill(_covar,_covar+COVAR_SIZE,0);
}

//___________________________________________________
SvxTrackProj_v1::SvxTrackProj_v1(const SvxTrackProj* base_ptr) :
  _location(base_ptr->get_location()),
  _x(base_ptr->get_x()),
  _y(base_ptr->get_y()),
  _z(base_ptr->get_z()),
  _px(base_ptr->get_px()),
  _py(base_ptr->get_py()),
  _pz(base_ptr->get_pz()),
  _chi_square(base_ptr->get_chi_square())
{}

//___________________________________________________
SvxTrackProj_v1::SvxTrackProj_v1(const SvxTrackProj& base_ref) :
  _location(base_ref.get_location()),
  _x(base_ref.get_x()),
  _y(base_ref.get_y()),
  _z(base_ref.get_z()),
  _px(base_ref.get_px()),
  _py(base_ref.get_py()),
  _pz(base_ref.get_pz()),
  _chi_square(base_ref.get_chi_square())
{}

//___________________________________________________
void SvxTrackProj_v1::print( std::ostream &out ) const
{
  out << "=================== SvxTrackProj =======================" << endl;
  out << "Location: " << _location << endl;
  out << "Position: (" << _x << "," << _y << "," << _z << ")" << endl;
  out << "Momentum: (" << _px << "," << _py << "," << _pz << ")" << endl;
  out << "Chisquare=" << _chi_square << endl;
  out << "Covariance: " << endl;
  
  for( unsigned int i=0; i<COVAR_ROW; i++ ) {
    for( unsigned int j=0; j<COVAR_ROW; j++ ) {
      out << _covar[i*COVAR_ROW+j];
      if(j<COVAR_ROW-1) out << ", ";
    }
    out << endl;
  }
  
  out << "=================== ** =======================" << endl;
  return;
}

//___________________________________________________
Float_t SvxTrackProj_v1::get_ptot() const
{
  return sqrt(_px*_px + _py*_py + _pz*_pz);
}

//___________________________________________________
Float_t SvxTrackProj_v1::get_covar(UShort_t i, UShort_t j) const 
{
  UShort_t index = i*COVAR_ROW + j;
  return _covar[index];
}

//___________________________________________________
void SvxTrackProj_v1::set_covar(UShort_t i, UShort_t j, Float_t val)
{
  UShort_t index = i*COVAR_ROW+j;
  _covar[index] = val;
}
