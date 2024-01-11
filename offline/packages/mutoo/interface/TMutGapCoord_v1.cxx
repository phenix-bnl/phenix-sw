#include<TMutGapCoord_v1.hh>
#include<TMutGeo.h>

ClassImp(TMutGapCoord)
ClassImp(TMutGapCoord_v1)

TMutGapCoord_v1::TMutGapCoord_v1() : 
  _arm(0),
  _station(0),
  _octant(0),
  _half_octant(0),
  _gap(0),
  _index(0),
  _x(0),
  _y(0),
  _z(0),
  _anode(0),
  _anode_dca(0)
{}

TMutGapCoord_v1::TMutGapCoord_v1(const Key& key,       
				 UShort_t arm,         
				 UShort_t station,     
				 UShort_t octant,      
				 UShort_t half_octant, 
				 UShort_t gap,         
				 UShort_t index) :     
  TMutGapCoord(key), 
  _arm(arm), 
  _station(station),
  _octant(octant),
  _half_octant(half_octant),
  _gap(gap),
  _index(index),
  _x(0),
  _y(0),
  _z(0),
  _anode(0),
  _anode_dca(0){}

TMutGapCoord_v1::TMutGapCoord_v1(const TMutGapCoord* base_ptr) : 
  TMutGapCoord(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _half_octant(base_ptr->get_half_octant()),
  _gap(base_ptr->get_gap()),
  _index(base_ptr->get_index()),
  _anode(base_ptr->get_anode()),
  _anode_dca(base_ptr->get_anode_dca())
{
  set_coord(base_ptr->get_coord());
}

TMutGapCoord_v1::TMutGapCoord_v1(const TMutGapCoord& base_ref) : 
  TMutGapCoord(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _half_octant(base_ref.get_half_octant()),
  _gap(base_ref.get_gap()),
  _index(base_ref.get_index()),
  _anode(base_ref.get_anode()),
  _anode_dca(base_ref.get_anode_dca())
{
  set_coord(base_ref.get_coord());
}

void TMutGapCoord_v1::print(std::ostream& os) const {
  MUTOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _half_octant
     << "  gap: " << _gap
     << "  index: " << _index << std::endl;
  os << " x: " << _x << " y: " << _y << " z: " << _z << std::endl;
  os << " anode: " << _anode << " anode dca: " << _anode_dca << std::endl;
  MUTOO::PRINT(os,"**");
}

PHVector TMutGapCoord_v1::get_anode_direction() const {
  return TMutGeo::get_anode_direction(_arm,_station,_octant,_half_octant,_gap,_anode);
}

