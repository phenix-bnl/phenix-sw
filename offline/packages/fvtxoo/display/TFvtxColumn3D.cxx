// $Id: TFvtxColumn3D.cxx,v 1.5 2011/02/08 23:12:17 youzy Exp $

/*!
	 \file TFvtxColumn3D.cxx
	 \brief 3D object for forward vertex column
	 \author Hugo Pereira
	 \version $Revision: 1.5 $
	 \date $Date: 2011/02/08 23:12:17 $
*/

#include <FvtxGeom.h>
#include <FvtxColumn.h>
#include <FvtxStrip.h>
#include <FVTXOO.h>

#include <TBRIK.h>
#include <TTRD1.h>
#include <TRotMatrix.h>

#include <cmath>
#include <sstream>

#include "TFvtxColumn3D.h"

using namespace std;

//_______________________________________________________
TFvtxColumn3D::TFvtxColumn3D( const TFvtxIndex &index, TNode* parent ):
	PHObj3D( parent ),
	_index( index ), 
	_name( "unnamed" ),
	_draw_strips( false ),
	_column_ptr( 0 )
{

	_line_color = 1;

	// check indexes
	if( !_index.check() ) {
		cout << "TFvtxColumn3D::TFvtxColumn3D - invalid ids.\n";
		return;
	}
	
	// create name
	ostringstream what;
	what << "Column3D_" 
		<< _index.arm() << "_" 
		<< _index.station() << "_" 
		<< _index.column();
	_name = what.str();
		
	// retrieve/initialize/check column pointer
	_column_ptr = FvtxGeom::get_arm( _index.arm() )
                        ->get_cage( _index.cage() )
			->get_station( _index.station() )
			->get_sector( _index.sector() )
			->get_column( _index.column() );	
	if( !_column_ptr ) {
		cout << "TFvtxColumn3D::TFvtxColumn3D - invalid gap pointer.\n";
		return;
	}
	
	if( !_parent ) return;
	
	_is_valid = true;

}
	 
//_______________________________________________________
void TFvtxColumn3D::_make_nodes( void )
{
	
	if( !_is_valid ) {
		cout << "TFvtxColumn3D::make_nodes - invalid object.\n"; 
		return;
	}
		
	if( _parent ) _parent->cd();
	
	// retrieve half octant phi window
	// ensures angles are between 0 and 2pi
	double phi_begin = _column_ptr->get_phi_begin();
	double phi_end = _column_ptr->get_phi_end();
	if( phi_end < phi_begin ) phi_end+=2*M_PI;
	double phi_center = ( phi_end+phi_begin )/2;
	
	// retrieve r window
	double r_inner = _column_ptr->get_inner_radius();
	double r_outer = _column_ptr->get_outer_radius();	
		
	// calculate shape geometry
	double dw_small = fabs( r_inner*sin(phi_end-phi_begin)/2 );
	double dw_big = fabs( r_outer*sin(phi_end-phi_begin)/2 );
	double dr = fabs( r_outer - r_inner)/2;

	// temporary should be replaced by a FVTXGeom value	
	double dz = _column_ptr->get_delta_z()/2;

	// calculate half octant center position
	double r_center = ( r_inner+r_outer )/2;
	double x_center = r_center*cos( phi_center );
	double y_center = r_center*sin( phi_center );
	double z_center = _column_ptr->get_z();

	// octant shape
	string shape_name( "trd_"+_name );
	TTRD1 *trd = new TTRD1( shape_name.c_str(), shape_name.c_str(), "void", dw_big, dw_small, dz, dr );
	_shapes.push_back( trd );

	// rotation to have octant mediane along phi_center
	double s( sin( phi_center ) );
	double c( cos( phi_center ) );
	double rotv[] = { 
		 s,	-c, 0,
		 0,	 0, 1,
		 -c, -s, 0 };
		 
	string rot_name( "rot_" + _name );
	TRotMatrix* rot = new TRotMatrix( rot_name.c_str(), rot_name.c_str(), rotv );
	_matrices.push_back( rot );

	// shape node
	string node_name( "node_"+_name );
	TNode* node = new TNode( node_name.c_str(), node_name.c_str(), trd, 0, 0, 0 );
	node->SetMatrix( rot );
	node->SetLineWidth( _line_width );
	node->SetLineColor( _line_color );
	node->SetVisibility( 0 );
	node->SetPosition( x_center, y_center, z_center );
	_nodes.push_back( node );

	if( _parent ) _parent->cd();

	if( _draw_strips )
	for( unsigned int strip_id = 0; strip_id < _column_ptr->get_n_strips(); strip_id++ )
	{
		FvtxStrip* strip = _column_ptr->get_strip( strip_id );
		_make_segment( 
			strip->get_position_begin(), 
			strip->get_position_end(), 
			strip->get_width() );
	}
}

//_______________________________________________________
void TFvtxColumn3D::print( std::ostream& out ) const
{
	
	// some dump
	FVTXOO::PRINT( cout, "TFvtxColumn3D::print" );
	cout << "location : [" << _index.arm() << "," << _index.cage() << "," << _index.station() << "," << _index.sector() << "," << _index.column() << "]" << endl;
	cout << "theta=(" << atan2( _column_ptr->get_inner_radius(), fabs(_column_ptr->get_z()) ) << "," << atan2( _column_ptr->get_outer_radius(), fabs(_column_ptr->get_z()) ) << ")" << endl;
	cout << "phi=(" << _column_ptr->get_phi_begin() << "," << _column_ptr->get_phi_end() << ")" << endl;
	cout << "z=" << _column_ptr->get_z() << endl;
	FVTXOO::PRINT( cout, "**" );

}
