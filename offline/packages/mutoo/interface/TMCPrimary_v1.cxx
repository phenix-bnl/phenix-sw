#include <MUTOO.h>
#include <TMCPrimary_v1.hh>

ClassImp(TMCPrimary_v1);

/*! default constructor */
TMCPrimary_v1::TMCPrimary_v1() :   
  _index(0),
  _file_key(0),
  _pisa_process_id(0),
  _pid(0),
  _x_orig(0),
  _y_orig(0),
  _z_orig(0),
  _px_orig(0),
  _py_orig(0),
  _pz_orig(0),
  _energy_orig(0)
{;}

TMCPrimary_v1::TMCPrimary_v1(const Key& key, 
			     UShort_t index) :
  TMCPrimary(key), 
  _index(index),
  _file_key(0),
  _pisa_process_id(0),
  _pid(0),
  _x_orig(0),
  _y_orig(0),
  _z_orig(0),
  _px_orig(0),
  _py_orig(0),
  _pz_orig(0),
  _energy_orig(0)
{;}

TMCPrimary_v1::TMCPrimary_v1(const TMCPrimary* base_ptr):
  TMCPrimary(*base_ptr),
  _index(base_ptr->get_index()),
  _file_key(base_ptr->get_file_key()),
  _pisa_process_id(base_ptr->get_pisa_process_id()),
  _pid(base_ptr->get_pid()),
  _x_orig(base_ptr->get_x_orig()),
  _y_orig(base_ptr->get_y_orig()),
  _z_orig(base_ptr->get_z_orig()),
  _px_orig(base_ptr->get_px_orig()),
  _py_orig(base_ptr->get_py_orig()),
  _pz_orig(base_ptr->get_pz_orig()),
  _energy_orig(base_ptr->get_energy_orig()){;}

TMCPrimary_v1::TMCPrimary_v1(const TMCPrimary& base_ref):
  TMCPrimary(base_ref),
  _index(base_ref.get_index()),
  _file_key(base_ref.get_file_key()),
  _pisa_process_id(base_ref.get_pisa_process_id()),
  _pid(base_ref.get_pid()),
  _x_orig(base_ref.get_x_orig()),
  _y_orig(base_ref.get_y_orig()),
  _z_orig(base_ref.get_z_orig()),
  _px_orig(base_ref.get_px_orig()),
  _py_orig(base_ref.get_py_orig()),
  _pz_orig(base_ref.get_pz_orig()),
  _energy_orig(base_ref.get_energy_orig())
{;}
  

  
void TMCPrimary_v1::print(std::ostream& os) const {		       
  MUTOO::PRINT(os,GetName());
  os << " index: " << _index << std::endl;
  os << " pisa process  id: " << _pisa_process_id << " particle id : " << _pid << std::endl;
  os << " p orig = {" << _px_orig << "," << _py_orig << "," << _pz_orig << "}" << std::endl;
  os << " x orig = {" << _x_orig << "," << _y_orig << "," << _z_orig << "}" << std::endl;
  os << " ptot orig = " << get_ptot_orig() << std::endl;
  os << " energy orig = " << get_energy_orig() << std::endl;
  MUTOO::PRINT(os,"**");
}








