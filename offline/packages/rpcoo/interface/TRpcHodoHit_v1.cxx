
/*!
	\file TRpcHodoHit_v1.cxx
	\brief Rpc Hodoscope hit interface object
	\author R.S.Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/02/13 02:53:10 $
*/

#include "TRpcHodoHit_v1.h"
#include "RPCOO.h"
ClassImp(TRpcHodoHit_v1)

//_______________________________________________________
TRpcHodoHit_v1::TRpcHodoHit_v1():
	_arm(0),
	_station(0),
	_strip(0),
	_status(0),
	_q(0),
	_q_error(0),
	_t(0),
	_t_error(0)
{}

//_______________________________________________________
TRpcHodoHit_v1::TRpcHodoHit_v1(
		const Key& key, 
		UShort_t arm, 
		UShort_t station, 
		UShort_t strip) : 
	TRpcHodoHit(key), 
	_arm(arm), 
	_station(station),
	_strip(strip),
	_status(0),
	_q(0),
	_q_error(0),
	_t(0),
	_t_error(0)
{}

//_______________________________________________________
TRpcHodoHit_v1::TRpcHodoHit_v1(const TRpcHodoHit* base_ptr) : 
  TRpcHodoHit(*base_ptr),
  _arm(base_ptr->get_arm()),
  _station(base_ptr->get_station()),
  _strip(base_ptr->get_strip()),
  _status(base_ptr->get_status()),
  _q(base_ptr->get_q()),
  _q_error(base_ptr->get_q_error()),
  _t(base_ptr->get_t()),
  _t_error(base_ptr->get_t_error())
{}

//_______________________________________________________
TRpcHodoHit_v1::TRpcHodoHit_v1(const TRpcHodoHit& base_ref) : 
  TRpcHodoHit(base_ref),
  _arm(base_ref.get_arm()),
  _station(base_ref.get_station()),
  _strip(base_ref.get_strip()),
  _status(base_ref.get_status()),
  _q(base_ref.get_q()),
  _q_error(base_ref.get_q_error()),
  _t(base_ref.get_t()),
  _t_error(base_ref.get_t_error())
{}

//_______________________________________________________
void TRpcHodoHit_v1::print( std::ostream& os ) const
{
	RPCOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  strip: " << _strip << std::endl;
  os << " q = " << _q << " q_error = " << _q_error  << std::endl;
  os << " t = " << _t << " t_error = " << _t_error  << std::endl;
  os << " status = {" << _status << std::endl;
	RPCOO::PRINT(os,"**");
}
