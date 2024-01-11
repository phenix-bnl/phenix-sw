// $Id: TFvtxMCHit_v1.cxx,v 1.9 2011/12/01 04:16:20 slash Exp $

/*!
	 \file TFvtxMCHit_v1.cxx
	 \brief The forward vertex Monte Carlo hit object 
	 \author Hugo Pereira Da Costa
	 \version $Revision: 1.9 $
	 \date $Date: 2011/12/01 04:16:20 $
*/

#include "TFvtxMCHit_v1.h"
ClassImp( TFvtxMCHit_v1 );

//________________________________________________________
TFvtxMCHit_v1::TFvtxMCHit_v1( void ):
	_arm( 0 ),
        _cage( 0 ),
	_station( 0 ),
	_sector( 0 ),
	_column( 0 ),
	_index( 0 ),
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
		
//_____________________________________________
TFvtxMCHit_v1::TFvtxMCHit_v1( 
		const Key& key,
		const unsigned short& arm,
                const unsigned short& cage,
		const unsigned short& station,
		const unsigned short& sector,
		const unsigned short& column,
		const unsigned short& index ):
	TFvtxMCHit(key), 
	_arm( arm ),
        _cage( cage ),
	_station( station ),
	_sector( sector ),
	_column( column ),
	_index( index ),
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
TFvtxMCHit_v1::TFvtxMCHit_v1(const TFvtxMCHit* base_ptr) : 
	TFvtxMCHit(*base_ptr),
	_arm( base_ptr->get_arm() ),
        _cage( base_ptr->get_cage() ),
	_station( base_ptr->get_station() ),
	_sector( base_ptr->get_sector() ),
	_column( base_ptr->get_column() ),
	_index( base_ptr->get_index() ),
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
	for( unsigned int i = 0; i < base_ptr->get_n_strip(); i++ )
	add_strip( base_ptr->get_strip(i)->get_strip(), base_ptr->get_strip(i)->get_q() );
		
}

//___________________________________________________
TFvtxMCHit_v1::TFvtxMCHit_v1(const TFvtxMCHit& base_ref) : 
	TFvtxMCHit(base_ref),
	_arm(base_ref.get_arm()),
        _cage(base_ref.get_cage()),
	_station(base_ref.get_station()),
	_sector(base_ref.get_sector()),
	_column(base_ref.get_column()),
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
	for( unsigned int i = 0; i < base_ref.get_n_strip(); i++ )
	add_strip( base_ref.get_strip(i)->get_strip(), base_ref.get_strip(i)->get_q() );
		
}

//_______________________________________________________
void TFvtxMCHit_v1::print( std::ostream& os ) const
{
	FVTXOO::PRINT(os,GetName());
	os 
		<< " arm: " << _arm
                << " cage: " << _cage
		<< " station: " << _station
		<< " sector: " << _sector
		<< " column: " << _column
		<< " index: " << _index << std::endl;
	os << " parent track id: " << _track_id << std::endl;
	os << " p = {" << _px << "," << _py << "," << _pz << "}" << std::endl;
	os << " x = {" << _x << "," << _y << "," << _z << "}" << std::endl;

	// Print TFvtxMCStrips
	for( strip_list::const_iterator strip_iter = _strip_list.begin(); strip_iter!=_strip_list.end();++strip_iter)		
	strip_iter->print();
	FVTXOO::PRINT(os,"**");
}
