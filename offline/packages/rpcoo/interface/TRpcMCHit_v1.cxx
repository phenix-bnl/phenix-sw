// $Id: TRpcMCHit_v1.cxx,v 1.3 2008/08/28 00:50:21 kempel Exp $

/*!
	\file TRpcMCHit_v1.cxx
	\brief Rpc MC hit interface object
	\author H. Pereira Da Costa
	\version $Revision: 1.3 $
	\date $Date: 2008/08/28 00:50:21 $
*/

#include "TRpcMCHit_v1.h"
#include "RPCOO.h"
ClassImp(TRpcMCHit_v1)

//_______________________________________________________
TRpcMCHit_v1::TRpcMCHit_v1():
	_arm(0),
	_station(0),
	_octant(0),
        _halfoctant(0),
        _rseg(0),
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
{}

//_______________________________________________________
TRpcMCHit_v1::TRpcMCHit_v1(
		const Key& key, 
		const UShort_t& arm, 
		const UShort_t& station, 
		const UShort_t& octant,
		const UShort_t& halfoctant,
		const UShort_t& rseg,
		const UShort_t& index) : 
	TRpcMCHit(key), 
	_arm(arm), 
	_station(station),
	_octant(octant),
	_halfoctant(halfoctant),
	_rseg(rseg),
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
{}

//_______________________________________________________
TRpcMCHit_v1::TRpcMCHit_v1(const TRpcMCHit* base_ptr) : 
	TRpcMCHit(*base_ptr),
	_arm(base_ptr->get_arm()),
	_station(base_ptr->get_station()),
	_octant(base_ptr->get_octant()),
	_halfoctant(base_ptr->get_half_octant()),
	_rseg(base_ptr->get_rseg()),
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
	_pz(base_ptr->get_pz())
{
	
	// copy strip_list
	strip_list strips( base_ptr->get_strip_list() );
	for( strip_iterator iter = strips.begin(); iter!= strips.end(); iter++ )
	if( *iter ) add_strip( (*iter)->get_stripid(), (*iter)->get_q() );
		
}

//_______________________________________________________
TRpcMCHit_v1::TRpcMCHit_v1(const TRpcMCHit& base_ref) : 
	TRpcMCHit(base_ref),
	_arm(base_ref.get_arm()),
	_station(base_ref.get_station()),
	_octant(base_ref.get_octant()),
	_halfoctant(base_ref.get_half_octant()),
	_rseg(base_ref.get_rseg()),
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
	_pz(base_ref.get_pz())
{	
	// copy strip_list
	strip_list strips( base_ref.get_strip_list() );
	for( strip_iterator iter = strips.begin(); iter!= strips.end(); iter++ )
	if( *iter ) add_strip( (*iter)->get_stripid(), (*iter)->get_q() );
}

//_______________________________________________________
void TRpcMCHit_v1::print( std::ostream& os ) const
{
	RPCOO::PRINT(os,GetName());
	os << " arm: " << _arm
		 << "	station: " << _station
	         << "  octant: " << _octant
	         << "  half octant: " << _halfoctant
	         << "  rseg: " << _rseg
		 << "	index: " << _index << std::endl;
	os << " parent track id: " << _track_id << std::endl;
	os << " p = {" << _px << "," << _py << "," << _pz << "}" << std::endl;
	os << " x = {" << _x << "," << _y << "," << _z << "}" << std::endl;

	// Print TRpcMCstrips
	strip_list strips = get_strip_list();		
	for( strip_iterator strip_iter = strips.begin(); strip_iter!=strips.end();++strip_iter)		
	(*strip_iter)->print();
	RPCOO::PRINT(os,"**");
}
