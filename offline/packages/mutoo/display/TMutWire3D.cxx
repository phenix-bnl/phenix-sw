// $Id: TMutWire3D.cxx,v 1.3 2007/05/30 11:52:22 hpereira Exp $

/*!
	\file TMutWire3D.cxx
	\brief 3D object for mutr coordinates
	\author Hugo Pereira
	\version $Revision: 1.3 $
	\date $Date: 2007/05/30 11:52:22 $
*/

#include <MUTOO.h>
#include <MutGeom.h>
#include <MutWire.h>

#include "TMutWire3D.h"

using namespace std;

//_______________________________________________________
TMutWire3D::TMutWire3D( const TMutIndex &index, const unsigned int& wire_id, TNode* parent ):
	PHObj3D( parent ),
	_index( index ), 
  _wire_id( wire_id ),
	_name( "unnamed" ),
	_wire_ptr( 0 )
{

	_is_valid = false;
	_line_color = 4;
  
	// check indexes
	if( !_index.check() ) {
		cout << "TMutWire3D::TMutWire3D - invalid ids.\n";
		return;
	}
	
	// create name
	ostringstream what;
	what << "Wire3D_" 
		<< _index.arm() << "_" 
		<< _index.station() << "_" 
		<< _index.octant() << "_" 
		<< _index.half() << "_"
		<< _index.gap() << "_"
    << _wire_id;
	_name = what.str();
		
	// retrieve/initialize/check arm structure
	MutArm* arm_ptr( (_index.arm()==MUTOO::South) ? SouthArm():NorthArm() );	
	if( !arm_ptr ) {
		cout << "TMutWire3D::TMutWire3D - invalid arm pointer.\n";
		return;
	}
  
  // retrieve plane
  MutPlane* plane_ptr = arm_ptr->
    f_pMutStations[_index.station()]->
    f_pMutOctants[_index.octant()]->
    f_pMutHalfOctants[_index.half()]->
    f_pMutGaps[_index.gap()]->
    f_pMutPlanes[MUTOO::Wire];
  if( !plane_ptr )
  {
    cout << "TMutWire3D;;TMutWire3D - invalid plane pointer.\n";
    return;
  }
  
  if( int( _wire_id ) >= plane_ptr->getNumElements() )
  {
    cout << "TMutWire3D;;TMutWire3D - invalid wire index.\n";
    return;
  }
  
  _wire_ptr = arm_ptr->
    f_pMutStations[_index.station()]->
    f_pMutOctants[_index.octant()]->
    f_pMutHalfOctants[_index.half()]->
    f_pMutGaps[_index.gap()]->
    f_pMutPlanes[MUTOO::Wire]->
    f_pMutWires[_wire_id];  
  
	if( !_wire_ptr ) 
  {
		cout << "TMutWire3D::TMutWire3D - invalid gap pointer.\n";
		return;
	}
  	
	if( !_parent ) return;
	
	_is_valid = true;

}

	 
//_______________________________________________________
void TMutWire3D::print( ostream& out ) const
{ 
  
  if( !_is_valid ) {
		out << "TMutWire3D::print - invalid object.\n"; 
		return;
	}
  
  PHPoint begin( _wire_ptr->getGlobalPositionBegin() );
  PHPoint end( _wire_ptr->getGlobalPositionEnd() );
  
  out << "TMutWire3D::print -"
    << " begin: (" << begin.getX() << "," << begin.getY() << "," << begin.getZ() << ")"
    << " end: (" << end.getX() << "," << end.getY() << "," << end.getZ() << ")"
    << endl;
  
}

//_______________________________________________________
void TMutWire3D::_make_nodes( void )
{
  cout << "TMutWire3D::make_nodes.\n"; 
  
	if( !_is_valid ) {
		cout << "TMutWire3D::make_nodes - invalid object.\n"; 
		return;
	}
	
  if( _parent ) _parent->cd();
  _make_segment( _wire_ptr->getGlobalPositionBegin(), _wire_ptr->getGlobalPositionEnd(), 1 );
  return;

}
