// $Id: TMutCoord_v3.cxx,v 1.5 2011/12/29 20:19:29 slash Exp $

#include "TMutCoord_v3.hh"

ClassImp(TMutCoord_v3)  
  
TMutCoord_v3::TMutCoord_v3() : 
  _arm(0),
  _station(0),
  _octant(0),
  _half_octant(0),
  _gap(0),
  _cathode(0),
  _index(0),
  _q_peak(0),
  _q_tot(0),
  _peak_strip(0),
  _cos_theta_wire(0),
  _error(0),
  _w(0),
  _status(0),
  _q_error(0)
{
  std::fill(_point_1,_point_1+POINT_SIZE,0);
  std::fill(_point_2,_point_1+POINT_SIZE,0);
}

TMutCoord_v3::TMutCoord_v3(const Key& key,       
			   UShort_t arm,         
			   UShort_t station,     
			   UShort_t octant,      
			   UShort_t half_octant, 
			   UShort_t gap,         
			   UShort_t cathode,
			   UShort_t index) :     
  TMutCoord(key), 
  _arm(arm), 
  _station(station),
  _octant(octant),
  _half_octant(half_octant),
  _gap(gap),
  _cathode(cathode),
  _index(index),
  _q_peak(0),
  _q_tot(0),
  _peak_strip(0),
  _cos_theta_wire(0),
  _error(0),
  _w(0),
  _status(0),
  _q_error(0)
{
  std::fill(_point_1,_point_1+POINT_SIZE,0);
  std::fill(_point_2,_point_1+POINT_SIZE,0);
}

TMutCoord_v3::TMutCoord_v3(const TMutCoord* base_ptr) :
  TMutCoord(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _half_octant(base_ptr->get_half_octant()),
  _gap(base_ptr->get_gap()),
  _cathode(base_ptr->get_cathode()),
  _index(base_ptr->get_index()),
  _q_peak(base_ptr->get_q_peak()),
  _q_tot(base_ptr->get_q_tot()),
  _peak_strip(base_ptr->get_peak_strip()),
  _cos_theta_wire(base_ptr->get_cos_theta_wire()),
  _error(base_ptr->get_error()),
  _w(base_ptr->get_w()),
  _status(base_ptr->get_status()),
  _q_error(base_ptr->get_q_error())
{
  set_coord(PHLine(base_ptr->get_coord_begin(),
		    base_ptr->get_coord_end()));
}

TMutCoord_v3::TMutCoord_v3(const TMutCoord& base_ref) :
  TMutCoord(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _half_octant(base_ref.get_half_octant()),
  _gap(base_ref.get_gap()),
  _cathode(base_ref.get_cathode()),
  _index(base_ref.get_index()),
  _q_peak(base_ref.get_q_peak()),
  _q_tot(base_ref.get_q_tot()),
  _peak_strip(base_ref.get_peak_strip()),
  _cos_theta_wire(base_ref.get_cos_theta_wire()),
  _error(base_ref.get_error()),
  _w(base_ref.get_w()),
  _status(base_ref.get_status()),
  _q_error(base_ref.get_q_error())
{
  set_coord(PHLine(base_ref.get_coord_begin(),
		   base_ref.get_coord_end()));
}


void TMutCoord_v3::print(std::ostream& os) const {
  MUTOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _half_octant
     << "  gap: " << _gap
     << "  cathode: " << _cathode 
     << "  index: " << _index << std::endl;

  // dump the status
  //
  os << "key: " << get_key().get_obj_key() << " status: ";
  if(get_peak_bound()) os << "PEAK_BOUND ";
  if(get_low_charge()) os << "LOW_CHARGE ";
  if(get_high_charge()) os << "HIGH_CHARGE ";
  os << std::endl;
  
  os << " x1: " << _point_1[0] << " y1: " << _point_1[1] << " z1: " << _point_1[2] << std::endl;
  os << " x2: " << _point_2[0] << " y2: " << _point_2[1] << " z2: " << _point_2[2] << std::endl;
  os << " q_peak: " << _q_peak << " q_tot: " << _q_tot << std::endl;
  os << " q error: " << _q_error << std::endl;
  os << " w: " << _w << std::endl;
  os << " w error: " << _error << std::endl;
  MUTOO::PRINT(os,"**");
}

PHLine TMutCoord_v3::get_coord() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHLine(PHPoint(_point_1[0],
  			_point_1[1],
  			_point_1[2]),
  		PHPoint(_point_2[0],
  			_point_2[1],
  			_point_2[2]));
  return PHLine();
}

PHPoint TMutCoord_v3::get_coord_end() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(_point_2[0],
		 _point_2[1],
		 _point_2[2]);
}

PHPoint TMutCoord_v3::get_coord_midpoint() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(0.5*(_point_2[0] + _point_1[0]),
		 0.5*(_point_2[1] + _point_1[1]),
		 0.5*(_point_2[2] + _point_1[2]));
}

PHPoint TMutCoord_v3::get_coord_begin() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(_point_1[0],
		 _point_1[1],
		 _point_1[2]);
}

double TMutCoord_v3::get_mean_z() const
{
  return 0.5*(_point_1[2] + _point_2[2]);
}

void TMutCoord_v3::set_coord(const PHLine& line) 
{
  _point_1[0] = line.getBasepoint().getX();
  _point_1[1] = line.getBasepoint().getY();
  _point_1[2] = line.getBasepoint().getZ();
  _point_2[0] = _point_1[0] + line.getDirection().getX();
  _point_2[1] = _point_1[1] + line.getDirection().getY();
  _point_2[2] = _point_1[2] + line.getDirection().getZ();
}
