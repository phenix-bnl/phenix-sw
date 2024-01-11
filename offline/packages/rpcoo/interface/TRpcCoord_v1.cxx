// $Id: TRpcCoord_v1.cxx,v 1.3 2011/07/16 03:58:59 slash Exp $

/*!
	\file TRpcCoord_v1.cxx
	\brief Rpc coordinate interface object
	\author H. Pereira Da Costa
  \version $Revision: 1.3 $
  \date    $Date: 2011/07/16 03:58:59 $
*/

#include "TRpcCoord_v1.h"
//INCLUDECHECKER: Removed this line: #include "RPCOO.h"
ClassImp(TRpcCoord_v1)

using namespace std;


//____________________________________________
TRpcCoord_v1::TRpcCoord_v1( void ):
		_arm(0),
		_station(0),
		_octant(0),
		_halfoctant(0),
		_rseg(0),
		_index(0),
		_peak_strip(0),
		_q_peak(0),
		_q_tot(0),
		_q_tot_error(0), 
		_t(0),
		_t_error(0),
		_x(0),
		_y(0),
		_z(0)
{ std::fill(_covar,_covar+COVAR_SIZE,0); }

//____________________________________________
TRpcCoord_v1::TRpcCoord_v1( const Key& key, UShort_t arm, UShort_t station, UShort_t octant, UShort_t halfoctant, UShort_t rseg, UShort_t index ):
		TRpcCoord( key ),
		_arm( arm ),
		_station( station ),
		_octant(octant),
		_halfoctant(halfoctant),
		_rseg(rseg),
		_index( index ),
		_peak_strip(0),
		_q_peak(0),
		_q_tot(0),
		_q_tot_error(0), 
		_t(0),
		_t_error(0),
		_x(0),
		_y(0),
		_z(0)
{ std::fill(_covar,_covar+COVAR_SIZE,0); }
	
//____________________________________________
TRpcCoord_v1::TRpcCoord_v1( const TRpcCoord& ref ):
		TRpcCoord( ref ),
		_arm( ref.get_arm() ),
		_station( ref.get_station() ),
		_octant( ref.get_octant() ),
		_halfoctant( ref.get_half_octant() ),
		_rseg( ref.get_rseg() ),
		_index( ref.get_index() ),
		_peak_strip( ref.get_peak_strip() ),
		_q_peak( ref.get_q_peak() ),
		_q_tot( ref.get_q_tot() ),
		_q_tot_error( ref.get_q_tot_error() ), 
		_t( ref.get_t() ),
		_t_error( ref.get_t_error() ),
		_x( ref.get_x() ),
		_y( ref.get_y() ),
		_z( ref.get_y() )
{
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
	set_covar( i, j, ref.get_covar(i,j) );
}

//____________________________________________
TRpcCoord_v1::TRpcCoord_v1( const TRpcCoord* ptr ):
		TRpcCoord( *ptr ),
		_arm( ptr->get_arm() ),
		_station( ptr->get_station() ),
		_octant( ptr->get_octant() ),
		_halfoctant( ptr->get_half_octant() ),
		_rseg( ptr->get_rseg() ),
		_index( ptr->get_index() ),
		_peak_strip( ptr->get_peak_strip() ),
		_q_peak( ptr->get_q_peak() ),
		_q_tot( ptr->get_q_tot() ),
		_q_tot_error( ptr->get_q_tot_error() ), 
		_t( ptr->get_t() ),
		_t_error( ptr->get_t_error() ),
		_x( ptr->get_x() ),
		_y( ptr->get_y() ),
		_z( ptr->get_y() )
{
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
	set_covar( i, j, ptr->get_covar(i,j) );
}

//___________________________________________
void TRpcCoord_v1::print( ostream& os ) const
{
	
  RPCOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  station: " << _station
     << "  octant: " << _octant
     << "  half octant: " << _halfoctant
     << "  rseg: " << _rseg
     << "  index: " << _index << endl;
  os 
     	<< " peak strip: " << get_peak_strip()
     	<< " q_peak: " << get_q_peak() 
     	<< " q_tot: " << get_q_tot()  << " q_tot_error: " << get_q_tot_error() 
	   	<< " t: " << get_t() << " t_error: " << get_t_error() 
     	<< std::endl;
	os
     	<< " x: " << get_x() << " y: " << get_y() << " z: " << get_z() << endl;
	
	if( get_station() == RPCOO::Station1 || get_station() == RPCOO::Station2 )
	os 
			<< " r: " << sqrt( RPCOO::SQUARE( get_x() ) + RPCOO::SQUARE( get_y() ) )
			<< " phi: " << atan2( get_y(), get_x() )
			<< endl;
	
	os 				
	   	<< " cov: [" << get_covar(0,0) << "," << get_covar(0,1) << endl
			<< "       " << get_covar( 1,0 ) << "," << get_covar(1,1) << "]"
			<< endl; 
  RPCOO::PRINT(os,"**");
}
