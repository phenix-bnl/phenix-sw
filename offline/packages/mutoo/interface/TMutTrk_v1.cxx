// $Id: TMutTrk_v1.cxx,v 1.39 2011/12/29 20:19:31 slash Exp $

#include<TMutTrk_v1.hh>
#include<TDataType.h>
#include<TMutCoordMap.h>
#include<TMutStubMap.h>
#include<TMutGapCoordMap.h>
#include<TMutTrackUtil.h>
#include<TMutGeo.h>
#include<PHGeometry.h>
#include<algorithm>

ClassImp(TMutTrk_v1)

using namespace std;

//_____________________________________________________________________
TMutTrk_v1::TMutTrk_v1() :
  _arm(0),
  _octant(0),
  _index(0),
  _w_chi_square(0),
  _r_chi_square(0),
  _charge(1),
  _status(0)
{
  std::fill(_phi_min, _phi_min+WINDOWS_SIZE,0);
  std::fill(_phi_max, _phi_max+WINDOWS_SIZE,0);
  std::fill(_theta_min, _theta_min+WINDOWS_SIZE,0);
  std::fill(_theta_max, _theta_max+WINDOWS_SIZE,0);
}


//_____________________________________________________________________
TMutTrk_v1::TMutTrk_v1(const Key& key,
  UShort_t arm,
  UShort_t octant,
  UShort_t index) :
  TMutTrk(key),
  _arm(arm),
  _octant(octant),
  _index(index),
  _w_chi_square(0),
  _r_chi_square(0),
  _charge(1),
  _status(0)
{
  std::fill(_phi_min, _phi_min+WINDOWS_SIZE,0);
  std::fill(_phi_max, _phi_max+WINDOWS_SIZE,0);
  std::fill(_theta_min, _theta_min+WINDOWS_SIZE,0);
  std::fill(_theta_max, _theta_max+WINDOWS_SIZE,0);
}

//_____________________________________________________________________
TMutTrk_v1::TMutTrk_v1(const TMutTrk* base_ptr) :
  TMutTrk(*base_ptr),
  _arm(base_ptr->get_arm()),
  _octant(base_ptr->get_octant()),
  _index(base_ptr->get_index()),
  _trk_par(*(base_ptr->get_trk_par())),
  _trk_par_vtx(*(base_ptr->get_trk_par_vtx())),
  _trk_par_list(*(base_ptr->get_trk_par_list())),
  _fit_par(*(base_ptr->get_fit_par())),
  _w_residual_list(*(base_ptr->get_w_residual_list())),
  _r_residual_list(*(base_ptr->get_r_residual_list())),
  _w_chi_square(base_ptr->get_w_chi_square()),
  _r_chi_square(base_ptr->get_r_chi_square()),
  _charge(base_ptr->get_charge()),
  _status(base_ptr->get_status())
{
  for(int i=0;i<WINDOWS_SIZE;++i) _phi_min[i]=base_ptr->get_phi_min(i);
  for(int i=0;i<WINDOWS_SIZE;++i) _phi_max[i]=base_ptr->get_phi_max(i);
  for(int i=0;i<WINDOWS_SIZE;++i) _theta_min[i]=base_ptr->get_theta_min(i);
  for(int i=0;i<WINDOWS_SIZE;++i) _theta_max[i]=base_ptr->get_theta_max(i);
}

//_____________________________________________________________________
TMutTrk_v1::TMutTrk_v1(const TMutTrk& base_ref) :
  TMutTrk(base_ref),
  _arm(base_ref.get_arm()),
  _octant(base_ref.get_octant()),
  _index(base_ref.get_index()),
  _trk_par(*(base_ref.get_trk_par())),
  _trk_par_vtx(*(base_ref.get_trk_par_vtx())),
  _trk_par_list(*(base_ref.get_trk_par_list())),
  _fit_par(*(base_ref.get_fit_par())),
  _w_residual_list(*(base_ref.get_w_residual_list())),
  _r_residual_list(*(base_ref.get_r_residual_list())),
  _w_chi_square(base_ref.get_w_chi_square()),
  _r_chi_square(base_ref.get_r_chi_square()),
  _charge(base_ref.get_charge()),
  _status(base_ref.get_status())
{
  for(int i=0;i<WINDOWS_SIZE;++i) _phi_min[i]=base_ref.get_phi_min(i);
  for(int i=0;i<WINDOWS_SIZE;++i) _phi_max[i]=base_ref.get_phi_max(i);
  for(int i=0;i<WINDOWS_SIZE;++i) _theta_min[i]=base_ref.get_theta_min(i);
  for(int i=0;i<WINDOWS_SIZE;++i) _theta_max[i]=base_ref.get_theta_max(i);
}

//_____________________________________________________________________
void TMutTrk_v1::print(std::ostream& os, bool max) const
{
  MUTOO::PRINT(os,GetName());
  
  os << " arm: " << _arm
    << "	octant: " << _octant << "		 ";
  
  // Dump the status
  print_status( os );
  
  // dump track parameters
  os << " track par = {";
  
  os << setw(5) << setprecision(3) << setiosflags(ios::showpoint) << setiosflags(ios::fixed);	
  
  os << _trk_par.get_x() << ", ";
  os << _trk_par.get_y() << ", ";
  os << _trk_par.get_px() << ", ";
  os << _trk_par.get_py() << ", ";
  os << _trk_par.get_pz() << "}" << std::endl;		
  os << " ptot:" << _trk_par.get_ptot() << "	 z ref: " << _trk_par.get_z() << "	";	
  os << " chi_w:" << get_w_chi_square_pdf() << " ";
  os << " charge: " << get_charge() << std::endl;
  
  os << " TMutStub:" << get_associated<TMutStub>().count()	<< " ";
  os << " TMutCoord:" << get_associated<TMutCoord>().count()	<< " ";
  os << " TMutGapCoord:" << get_associated<TMutGapCoord>().count() << std::endl;
  
  max = true;
  
  if(max) {
    
    // dump track parameter list
    //	
    trk_par_list::const_iterator trk_list_iter = _trk_par_list.begin();
    
    for(;trk_list_iter!=_trk_par_list.end();++trk_list_iter) {
      os << " track par = {";
      os << trk_list_iter->get_x() << ", ";
      os << trk_list_iter->get_y() << ", ";
      os << trk_list_iter->get_px() << ", ";
      os << trk_list_iter->get_py() << ", ";
      os << trk_list_iter->get_pz() << "}" << "	 ";
      os << " z reference: " << trk_list_iter->get_z() << std::endl;	
    }
    
    
    // dump fit parameters
    //	
    os << " fit parameters = {";
    os << "x:" << _fit_par.get_x() << ", ";
    os << "y:" << _fit_par.get_y() << ", ";
    os << "dxdz: " << _fit_par.get_dxdz() << ", ";
    os << "dydz: " << _fit_par.get_dydz() << "}" << std::endl;		
    
    // dump vtx track parameters
    //	
    os << " track parameters vertex = {";
    os << "x:" << _trk_par_vtx.get_x() << ", ";
    os << "y:" << _trk_par_vtx.get_y() << ", ";
    os << "px: " << _trk_par_vtx.get_px() << ", ";
    os << "py: " << _trk_par_vtx.get_py() << ", ";
    os << "pz: " << _trk_par_vtx.get_pz() << "}" << std::endl;		
    os << " ptot: = " << _trk_par_vtx.get_ptot() << "	 z reference: " << _trk_par_vtx.get_z() << std::endl;	
    
    os << " station 1 phi window = {" << get_phi_min(0) << "," << get_phi_max(0) << "}" << std::endl;
    os << " station 2 phi window = {" << get_phi_min(1) << "," << get_phi_max(1) << "}" << std::endl;
    os << " station 3 phi window = {" << get_phi_min(2) << "," <<get_phi_max(2) << "}" << std::endl;
    os << " station 1 theta window = {" << get_theta_min(0) << "," << get_theta_max(0) << "}" << std::endl;
    os << " station 2 theta window = {" << get_theta_min(1) << "," << get_theta_max(1) << "}" << std::endl;
    os << " station 3 theta window = {" << get_theta_min(2) << "," << get_theta_max(2) << "}" << std::endl;
    
    os << std::endl;
    
    // extrapolate the track to gap0 MUID and dump coordinate
    //
    double z_muid = (_arm==MUTOO::South) ? -704.24 : 704.24;
    PHPoint trk_point = TMutTrackUtil::linear_track_model(&_fit_par,z_muid);
    
    os << " track gap0 point: " << trk_point.getX() << " " <<
      trk_point.getY() << " " << trk_point.getZ() << std::endl;
    
    double chi_w_dof = (get_n_w_residual()>2) ? get_w_chi_square()/(get_n_w_residual()-2) : 0;
    double chi_r_dof = (get_n_r_residual()>2) ? get_r_chi_square()/(get_n_r_residual()-2) : 0;
    os << " chi w square/dof: " << chi_w_dof << std::endl;
    os << " chi r square/dof: " << chi_r_dof << std::endl;
    
    os << " rdphi 12 " << get_rdphi_12() << std::endl;
    os << " rdphi 23 " << get_rdphi_23() << std::endl;
    
    if(0){
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

//_____________________________________________________________________
double TMutTrk_v1::get_w_chi_square_pdf() const 
{ 
  int ndf( get_ndf() );
  return (ndf > 0 ) ? _w_chi_square/ndf:0;
}

//_____________________________________________________________________
double TMutTrk_v1::get_r_chi_square_pdf() const 
{ 
  int ndf( get_ndf() );
  return (ndf > 0 ) ? _r_chi_square/ndf:0;
}

