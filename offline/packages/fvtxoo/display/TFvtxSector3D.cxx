// $Id: TFvtxSector3D.cxx,v 1.3 2011/02/08 23:12:17 youzy Exp $

/*!
	 \file TFvtxSector3D.cxx
	 \brief 3D object for forward vertex sector
	 \author Hugo Pereira
	 \version $Revision: 1.3 $
	 \date $Date: 2011/02/08 23:12:17 $
*/

#include <FvtxGeom.h>
#include <FvtxSector.h>
#include <FVTXOO.h>

#include <TBRIK.h>
#include <TTRD1.h>
#include <TRotMatrix.h>

#include <cmath>
#include <sstream>

#include "TFvtxSector3D.h"

using namespace std;

//_______________________________________________________
TFvtxSector3D::TFvtxSector3D( const TFvtxIndex &index, TNode* parent ):
	PHObj3D( parent ),
	_index( index ), 
	_name( "unnamed" ),
	_sector_ptr( 0 )
{

	_line_color = 4;

	// check indexes
	if( !_index.check() ) {
		cout << "TFvtxSector3D::TFvtxSector3D - invalid ids.\n";
		return;
	}
	
	// create name
	ostringstream what;
	what << "Sector3D_" 
		<< _index.arm() << "_" 
                << _index.cage() << "_"
		<< _index.station() << "_" 
		<< _index.sector();
	_name = what.str();
		
	// retrieve/initialize/check sector pointer
	_sector_ptr = FvtxGeom::get_arm( _index.arm() )->get_cage( _index.cage() )->get_station( _index.station() )->get_sector( _index.sector() );	
	if( !_sector_ptr ) {
		cout << "TFvtxSector3D::TFvtxSector3D - invalid gap pointer.\n";
		return;
	}
	
	if( !_parent ) return;
	
	_is_valid = true;

}
	 
//_______________________________________________________
void TFvtxSector3D::_make_nodes( void )
{
	
	if( !_is_valid ) {
		cout << "TFvtxSector3D::make_nodes - invalid object.\n"; 
		return;
	}
		
	if( _parent ) _parent->cd();
	
	// retrieve half octant phi window
	// ensures angles are between 0 and 2pi
	double phi_begin = _sector_ptr->get_phi_begin();
	double phi_end = _sector_ptr->get_phi_end();
	if( phi_end < phi_begin ) phi_end+=2*M_PI;
	double phi_center = ( phi_end+phi_begin )/2;
	
	// retrieve r window
	double r_inner = _sector_ptr->get_inner_radius();
	double r_outer = _sector_ptr->get_outer_radius();	
		
	// calculate shape geometry
	double dw_small = fabs( r_inner*sin(phi_end-phi_begin)/2 );
	double dw_big = fabs( r_outer*sin(phi_end-phi_begin)/2 );
	double dr = fabs( r_outer - r_inner)/2;
	double dz = _sector_ptr->get_delta_z()/2;

	// calculate half octant center position
	double r_center = ( r_inner+r_outer )/2;
	double x_center = r_center*cos( phi_center );
	double y_center = r_center*sin( phi_center );
	double z_center = _sector_ptr->get_z();

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
	
}

//_______________________________________________________
void TFvtxSector3D::print( std::ostream& out ) const
{
	
	// some dump
	FVTXOO::PRINT( cout, "TFvtxSector3D::print" );
	cout << "location : [" << _index.arm() << "," << _index.cage() << "," << _index.station() << "," << _index.sector() << "]" << endl;
	cout << "theta=(" << atan2( _sector_ptr->get_inner_radius(), fabs(_sector_ptr->get_z()) ) << "," << atan2( _sector_ptr->get_outer_radius(), fabs(_sector_ptr->get_z()) ) << ")" << endl;
	cout << "phi=(" << _sector_ptr->get_phi_begin() << "," << _sector_ptr->get_phi_end() << ")" << endl;
	cout << "z=" << _sector_ptr->get_z() << endl;
	FVTXOO::PRINT( cout, "**" );

}
