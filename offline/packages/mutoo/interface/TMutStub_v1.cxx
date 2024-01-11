#include<TMutStub_v1.hh>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutTrkMap.h>
#include<TMutGeo.h>
#include<PHGeometry.h>
#include<TDataType.h>
#include<boost/array.hpp>
#include<gsl/gsl_fit.h>
#include<cmath>

ClassImp(TMutStub_v1)

using namespace std;

TMutStub_v1::TMutStub_v1() :
  _arm(0),
  _station(0),
  _octant(0),
  _half_octant(0),
  _index(0),
  _w_chi_square(0),
  _r_chi_square(0),
  _phi_min(0),
  _phi_max(0),
  _theta_min(0),
  _theta_max(0)
{;}

TMutStub_v1::TMutStub_v1(const Key& key,
			 UShort_t arm,
			 UShort_t station,
			 UShort_t octant,
			 UShort_t hoct,
			 UShort_t index) :
  TMutStub(key),
  _arm(arm),
  _station(station),
  _octant(octant),
  _half_octant(hoct),
  _index(index),
  _w_chi_square(0),
  _r_chi_square(0),
  _phi_min(0),
  _phi_max(0),
  _theta_min(0),
  _theta_max(0)
{;}

TMutStub_v1::TMutStub_v1(const TMutStub* base_ptr) :
  TMutStub(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _half_octant(base_ptr->get_half_octant()),
  _index(base_ptr->get_index()),
  _fit_par(*base_ptr->get_fit_par()),
  _w_residual_list(*base_ptr->get_w_residual_list()),
  _r_residual_list(*base_ptr->get_r_residual_list()),
  _w_chi_square(base_ptr->get_w_chi_square()),
  _r_chi_square(base_ptr->get_r_chi_square()),
  _phi_min(base_ptr->get_phi_min()),
  _phi_max(base_ptr->get_phi_max()),
  _theta_min(base_ptr->get_theta_min()),
  _theta_max(base_ptr->get_theta_min())
{}

TMutStub_v1::TMutStub_v1(const TMutStub& base_ref) :
  TMutStub(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _half_octant(base_ref.get_half_octant()),
  _index(base_ref.get_index()),
  _fit_par(*base_ref.get_fit_par()),
  _w_residual_list(*base_ref.get_w_residual_list()),
  _r_residual_list(*base_ref.get_r_residual_list()),
  _w_chi_square(base_ref.get_w_chi_square()),
  _r_chi_square(base_ref.get_r_chi_square()),
  _phi_min(base_ref.get_phi_min()),
  _phi_max(base_ref.get_phi_max()),
  _theta_min(base_ref.get_theta_min()),
  _theta_max(base_ref.get_theta_min())
{}

UShort_t TMutStub_v1::get_n_coord() const {
  return get_associated<TMutCoord>().count();
}

UShort_t TMutStub_v1::get_n_gap_coord() const {
  return get_associated<TMutGapCoord>().count();
}

double 
TMutStub_v1::get_chi_pdf() const {
  UShort_t ncoord = get_associated<TMutCoord>().count();
  return (ncoord-4>0) ? get_w_chi_square()/(ncoord-4.0) : 0;
}

void 
TMutStub_v1::print(std::ostream& os, bool max) const 
{
  MUTOO::PRINT(os,GetName());
  os << " arm: " << _arm << "  ";
  os << " station: " << _station << "  ";
  os << " octant: " << _octant << "  ";
  os << " half octant: " << _half_octant << "  ";
  os << " index: " << _index << " key: " << get_key().get_obj_key() << std::endl;

  os << " stub par = {";

  os << setw(8) << setprecision(5) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);  
  os << _fit_par.get_x() << ", " 
    << _fit_par.get_y() << ", " 
    << _fit_par.get_z() << ", " 
    << _fit_par.get_dxdz() << ", " 
    << _fit_par.get_dydz() << "} chi2=" 
    << _fit_par.get_chi_square() 
    << std::endl;

  os << " chi_sq: " << get_w_chi_square() << " coords: "; 
  TMutCoordMap::const_key_iterator coord_iter = get_associated<TMutCoord>();
  while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
    os << coord_ptr->get()->get_key().get_obj_key() << "  ";
  }
  os << std::endl;

  if(max) {
    os << " drdz: " << _fit_par.get_drdz() << std::endl;
    os << " z begin: " << _fit_par.get_z_begin() << std::endl;
    os << " z end: " << _fit_par.get_z_end() << std::endl;
    os << " phi window: = {" << _phi_min << "," << _phi_max << "}" << std::endl;
    os << " theta window: = {" << _theta_min << "," << _theta_max << "}" << std::endl;
    
    TMutCoordMap::const_key_iterator coord_iter = get_associated<TMutCoord>();
    os << " number of associated TMutCoord:" << coord_iter.count() << std::endl;
    
    TMutGapCoordMap::const_key_iterator gap_coord_iter = get_associated<TMutGapCoord>();
    os << " number of associated TMutGapCoord:" << gap_coord_iter.count() << std::endl;
    os << " number of associated w residuals:" << _w_residual_list.size() << std::endl;
    os << " number of associated r residuals:" << _r_residual_list.size() << std::endl;

    TMutTrkMap::const_key_iterator trk_iter = get_associated<TMutTrk>();
    os << " number of associated TMutTrk:" << trk_iter.count() << std::endl;
    
    //    TMutCoordMap::const_key_iterator coord_iter = get_associated<TMutCoord>();
    //    while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
    //      coord_ptr->get()->print();
    //    }
    if(max){
      // dump residuals
      //
      os << " residuals" << std::endl;
      residual_list::const_iterator residual_iter = _w_residual_list.begin();
      for(;residual_iter != _w_residual_list.end(); ++residual_iter){
	residual_iter->print();
      }
    }
  }
  MUTOO::PRINT(os,"**");
}

PHVector
TMutStub_v1::get_anode_direction() const 
{
  TMutGapCoordMap::const_pointer min_gap_ptr = 0;
  UShort_t minimum_gap = USHRT_MAX;
  TMutGapCoordMap::const_key_iterator gap_iter = get_associated<TMutGapCoord>();  

  if(gap_iter.at_end()) return PHVector();

  while(TMutGapCoordMap::const_pointer gap_ptr = gap_iter.next()) {
    if(gap_ptr->get()->get_gap() < minimum_gap) {
      minimum_gap = gap_ptr->get()->get_gap();
      min_gap_ptr = gap_ptr;
    }
  }
  return min_gap_ptr->get()->get_anode_direction();
}

bool
TMutStub_v1::check_phi_window(const std::pair<float,float>& phi_window) const
{

//   /* 
//     hugo Pereira - 2004/07/24 this is a new check for the theta window
//     which is supposed to fix a bug in the official one. It is not entirely
//     validated yet, so it's commented on CVS in the meantime
//   */
//   double phi_ave( 0.5*( phi_window.first + phi_window.second ) );
//   double dphi_max( 0.5 *( phi_window.second - phi_window.first ) );
//    
//   double dphi( fabs( phi_ave - get_phi() ) );
//   dphi -= 2*M_PI*int( dphi/M_PI );
//   return dphi < dphi_max;
  
  // Project onto x and y axis to avoid [0,2PI] boundary shenanigans
  double x1 = std::cos(phi_window.first);
  double x2 = std::cos(phi_window.second);
  double y1 = std::sin(phi_window.first);
  double y2 = std::sin(phi_window.second);
  double xp = std::cos(get_phi());
  double yp = std::sin(get_phi());

  double r1 = std::sqrt(MUTOO::SQUARE(x1) + MUTOO::SQUARE(y1));
  double r2 = std::sqrt(MUTOO::SQUARE(x2) + MUTOO::SQUARE(y2));
  double rp = std::sqrt(MUTOO::SQUARE(xp) + MUTOO::SQUARE(yp));
  
  double dphi_window = std::acos((x1*x2 + y1*y2)/(r1*r2));
  double dphi_left = std::acos((x1*xp + y1*yp)/(r1*rp));
  double dphi_right = std::acos((x2*xp + y2*yp)/(r2*rp));
  
  // intermediate value -- has to be withing dphi_window of
  // both edges.
  return (dphi_left < dphi_window && dphi_right < dphi_window);

}

bool
TMutStub_v1::has_point() const 
{
  bool val = get_associated<TMutGapCoord>().count() > 0;
  return val;
}

double 
TMutStub_v1::get_dwdz() const 
{
  // Project tangent onto wz plane and return dwdz
  //
  PHVector w_axis = TMutGeo::get_w_axis(_arm,_station,_octant,_half_octant);
  double wcomp = PHGeometry::dot(TMutGeo::get_w_axis(_arm,_station,_octant,_half_octant), _fit_par.get_tangent( _arm ));
  double zcomp = PHGeometry::dot(PHVector(0,0,1), _fit_par.get_tangent( _arm ));
  return (zcomp!=0) ? wcomp/zcomp : 0;
}
