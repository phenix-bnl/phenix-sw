#include <MUTOO.h>
#include <TMutMCHit_v2.hh>

ClassImp(TMutMCHit_v2)

/*! default constructor */
TMutMCHit_v2::TMutMCHit_v2() : _arm(0),
			       _station(0),
			       _octant(0),
			       _half_octant(0),
			       _gap(0),
			       _wire(0),
			       _index(0),
			       _file_key(0),
			       _track_id(0),
			       _tof(0),
			       _eloss(0),
			       _x(0),
			       _y(0),
			       _z(0),
			       _px(0),
			       _py(0),
			       _pz(0)
{
  for(int i=0;i<W_SIZE;++i){
    _w_true[i]=0;
    _w_digit[i]=0;
  }	  
}

TMutMCHit_v2::TMutMCHit_v2(const Key& key, 
		     UShort_t arm, 
		     UShort_t station, 
		     UShort_t octant, 
		     UShort_t half_octant, 
		     UShort_t gap, 
		     UShort_t index) : 
  TMutMCHit(key), 
  _arm(arm), 
  _station(station),
  _octant(octant),
  _half_octant(half_octant),
  _gap(gap),
  _wire(0),
  _index(index),
  _file_key(0),
  _track_id(0),
  _tof(0),
  _eloss(0),
  _x(0),
  _y(0),
  _z(0),
  _px(0),
  _py(0),
  _pz(0)
{
  for(int i=0;i<W_SIZE;++i){
    _w_true[i]=0;
    _w_digit[i]=0;
  }	  
}

TMutMCHit_v2::TMutMCHit_v2(const TMutMCHit* base_ptr) : 
  TMutMCHit(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _half_octant(base_ptr->get_half_octant()),
  _gap(base_ptr->get_gap()),
  _wire(base_ptr->get_wire()),
  _index(base_ptr->get_index()),
  _file_key(base_ptr->get_file_key()),
  _track_id(base_ptr->get_track_id()),
  _tof(base_ptr->get_tof()),
  _eloss(base_ptr->get_eloss()),
  _x(base_ptr->get_x()),
  _y(base_ptr->get_y()),
  _z(base_ptr->get_z()),
  _px(base_ptr->get_px()),
  _py(base_ptr->get_py()),
  _pz(base_ptr->get_pz()),
  _strip_list(*(base_ptr->get_strip_list()))  
{
  for(int i=0; i<W_SIZE;++i){
    _w_true[i]=base_ptr->get_w_true(i);
    _w_digit[i]=base_ptr->get_w_digit(i);
  }
}

TMutMCHit_v2::TMutMCHit_v2(const TMutMCHit& base_ref) : 
  TMutMCHit(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _half_octant(base_ref.get_half_octant()),
  _gap(base_ref.get_gap()),
  _wire(base_ref.get_wire()),
  _index(base_ref.get_index()),
  _file_key(base_ref.get_file_key()),
  _track_id(base_ref.get_track_id()),
  _tof(base_ref.get_tof()),
  _eloss(base_ref.get_eloss()),
  _x(base_ref.get_x()),
  _y(base_ref.get_y()),
  _z(base_ref.get_z()),
  _px(base_ref.get_px()),
  _py(base_ref.get_py()),
  _pz(base_ref.get_pz()),
  _strip_list(*(base_ref.get_strip_list()))  
{
  for(int i=0; i<W_SIZE;++i){
    _w_true[i]=base_ref.get_w_true(i);
    _w_digit[i]=base_ref.get_w_digit(i);
  }
}

void TMutMCHit_v2::print(std::ostream& os) const {		       

  MUTOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _half_octant
     << "  gap: " << _gap
     << "  wire: " << _wire
     << "  index: " << _index << std::endl;
  os << " parent track id: " << _track_id << std::endl;
  os << " p = {" << _px << "," << _py << "," << _pz << "}" << std::endl;
  os << " x = {" << _x << "," << _y << "," << _z << "}" << std::endl;
  os << " w_true cathode 0: " << _w_true[0] << " w_true cathode 1: " << _w_true[1] << std::endl;
  os << " w_digit cathode 0: " << _w_digit[0] << " w_digit cathode 1: " << _w_digit[1] << std::endl;
  // Print TMutMCStrips
  //
  const strip_list* strips = get_strip_list();  
  strip_iterator strip_iter = strips->begin();  
  for(;strip_iter!=strips->end();++strip_iter){    
    strip_iter->print(os);
  }
  MUTOO::PRINT(os,"**");
}










