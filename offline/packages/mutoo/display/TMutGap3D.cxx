// $Id: TMutGap3D.cxx,v 1.6 2008/12/02 00:45:11 hpereira Exp $

/*!
	 \file TMutGap3D.cxx
	 \brief 3D object for mutr cathode
	 \author Hugo Pereira
	 \version $Revision: 1.6 $
	 \date $Date: 2008/12/02 00:45:11 $
*/

#include <MutGeom.h>

#include <MutOctant.h>
#include <MutGap.h>
#include <MUTOO.h>

#include <TBRIK.h>
#include <TTRD1.h>
#include <TRotMatrix.h>

#include <cmath>
#include <sstream>

#include "TMutGap3D.h"

using namespace std;

//_______________________________________________________
TMutGap3D::TMutGap3D( const TMutIndex &index, TNode* parent ):
	PHObj3D( parent ),
	_index( index ), 
	_name( "unnamed" ),
	_octant_ptr( 0 ),
	_gap_ptr( 0 )
{

	_line_color = 1;

	// check indexes
	if( !_index.check() ) 
  {
		cout << "TMutGap3D::TMutGap3D - invalid ids.\n";
		return;
	}
	
	// create name
	ostringstream what;
	what << "Cathode3D_" 
		<< _index.arm() 
		<< _index.station()  
		<< _index.octant() 
		<< _index.half()
		<< _index.gap();
	_name = what.str();
		
	// retrieve/initialize/check arm structure
	MutArm* arm_ptr( (_index.arm()==MUTOO::South) ? SouthArm():NorthArm() );	
	if( !arm_ptr ) {
		cout << "TMutGap3D::TMutGap3D - invalid arm pointer.\n";
		return;
	}
 
	// retrieve octant object
	_octant_ptr = arm_ptr->f_pMutStations[_index.station()] ->f_pMutOctants[_index.octant()];
	if( !_octant_ptr ) {
		cout << "TMutGap3D::TMutGap3D - invalid octant pointer.\n";
		return;
	} 
	
	// retrieve/check gap object	
	_gap_ptr = _octant_ptr->f_pMutHalfOctants[_index.half()]->f_pMutGaps[_index.gap()];
	if( !_gap_ptr ) {
		cout << "TMutGap3D::TMutGap3D - invalid gap pointer.\n";
		return;
	}
	
	if( !_parent ) return;
	
	_is_valid = true;

}
	 
//_______________________________________________________
void TMutGap3D::_make_nodes( void )
{
	
	if( !_is_valid ) {
		cout << "TMutGap3D::make_nodes - invalid object.\n"; 
		return;
	}
		
	if( _parent ) _parent->cd();
	
	// retrieve half octant phi window
	// ensures angles are between 0 and 2pi
	double phi_begin = _octant_ptr->getBeginPhi();
	double phi_end = _octant_ptr->getEndPhi();
	if( phi_end < phi_begin ) phi_end+=2*M_PI;
	
	// cut in half octant	
	if( _index.half() == 0 ) phi_end = (phi_begin+phi_end)/2;
	else phi_begin = (phi_begin+phi_end)/2;
	
	double phi_center = ( phi_end+phi_begin )/2;
	
	// retrieve r window
	double r_inner = _octant_ptr->getInnerRadius();
	double r_outer = _octant_ptr->getOuterRadius();	
		
	// calculate shape geometry
	double dw_small = fabs( r_inner*sin(phi_end-phi_begin)/2 );
	double dw_big = fabs( r_outer*sin(phi_end-phi_begin)/2 );
	double dr = fabs( r_outer - r_inner)/2;
	double dz = _gap_ptr->getGapThickness()/2;

	// calculate half octant center position
	double r_center = ( r_inner+r_outer )/2;
	double x_center = r_center*cos( phi_center );
	double y_center = r_center*sin( phi_center );
	double z_center = _gap_ptr->getGlobalPosition().getZ();

	// octant shape
	string shape_name( "trd_"+_name );
	TTRD1 *trd = new TTRD1( shape_name.c_str(), shape_name.c_str(), "void", dw_big, dw_small, dz, dr );
	_shapes.push_back( trd );

	// rotation to have octant mediane along phi_center
	double s( sin( phi_center ) );
	double c( cos( phi_center ) );
	double rotv[] = { 
		 s, -c, 0,
		 0, 0, 1,
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
void TMutGap3D::print( std::ostream& out ) const
{
	
	// some dump
	MUTOO::PRINT( cout, "TMutGap3D::print" );
	cout << "location : [" << _index.arm() << "," << _index.station() << "," << _index.gap() << "," << _index.octant() << "," << _index.half() << "]" << endl;
	cout << "theta=(" << atan2( _octant_ptr->getInnerRadius(), fabs(_gap_ptr->getGlobalPosition().getZ()) ) << "," << atan2( _octant_ptr->getOuterRadius(), fabs(_gap_ptr->getGlobalPosition().getZ()) ) << ")" << endl;
	cout << "phi=(" << _octant_ptr->getBeginPhi() << "," << _octant_ptr->getEndPhi() << ")" << endl;
	cout << "z=" << _gap_ptr->getGlobalPosition().getZ() << endl;
	MUTOO::PRINT( cout, "**" );

}
