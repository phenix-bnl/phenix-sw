// $Id: TRpcHit_v1.cxx,v 1.2 2008/08/28 00:50:20 kempel Exp $

/*!
	\file TRpcHit_v1.cxx
	\brief Rpc MC hit interface object
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2008/08/28 00:50:20 $
*/

#include "TRpcHit_v1.h"
#include "RPCOO.h"
ClassImp(TRpcHit_v1)

//_______________________________________________________
TRpcHit_v1::TRpcHit_v1():
	_arm(0),
	_station(0),
	_octant(0),
	_halfoctant(0),
	_rseg(0),
	_strip(0),
	_status(0),
	_q(0),
	_q_error(0),
	_t(0),
	_t_error(0)
{}

//_______________________________________________________
TRpcHit_v1::TRpcHit_v1(
		const Key& key, 
		UShort_t arm, 
		UShort_t station, 
		UShort_t octant, 
		UShort_t halfoctant,
		UShort_t rseg, 
		UShort_t strip) : 
	TRpcHit(key), 
	_arm(arm), 
	_station(station),
	_octant(octant),
	_halfoctant(halfoctant),
	_rseg(rseg),
	_strip(strip),
	_status(0),
	_q(0),
	_q_error(0),
	_t(0),
	_t_error(0)
{}

//_______________________________________________________
TRpcHit_v1::TRpcHit_v1(const TRpcHit* base_ptr) : 
  TRpcHit(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _octant(base_ptr->get_octant()),
  _halfoctant(base_ptr->get_half_octant()),
  _rseg(base_ptr->get_rseg()),
	_strip(base_ptr->get_strip()),
	_status(base_ptr->get_status()),
	_q(base_ptr->get_q()),
	_q_error(base_ptr->get_q_error()),
	_t(base_ptr->get_t()),
	_t_error(base_ptr->get_t_error())
{}

//_______________________________________________________
TRpcHit_v1::TRpcHit_v1(const TRpcHit& base_ref) : 
  TRpcHit(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _octant(base_ref.get_octant()),
  _halfoctant(base_ref.get_half_octant()),
  _rseg(base_ref.get_rseg()),
	_strip(base_ref.get_strip()),
	_status(base_ref.get_status()),
	_q(base_ref.get_q()),
	_q_error(base_ref.get_q_error()),
	_t(base_ref.get_t()),
	_t_error(base_ref.get_t_error())
{}

//_______________________________________________________
void TRpcHit_v1::print( std::ostream& os ) const
{
	RPCOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _halfoctant
     << "  rseg: " << _rseg
     << "  strip: " << _strip << std::endl;
  os << " q = " << _q << " q_error = " << _q_error  << std::endl;
  os << " t = " << _t << " t_error = " << _t_error  << std::endl;
  os << " status = {" << _status << std::endl;
	RPCOO::PRINT(os,"**");
}
