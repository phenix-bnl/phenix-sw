// $Id: TRpcPad3DType1.cxx,v 1.1 2006/04/22 02:01:38 hpereira Exp $

/*!
   \file    TRpcPad3DType1.cxx
   \brief   3D object for type1 RPC pad (r,phi)
   \author  H. Pereira Da Costa
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 02:01:38 $
*/

#include "TRpcPad3DType1.h"
#include "RpcGeom.h"
#include "RpcPadType1.h"
#include "RPCOO.h"

//INCLUDECHECKER: Removed this line: #include <sstream>
#include <TTUBS.h>


using namespace std;

//_______________________________________________________
void TRpcPad3DType1::_make_nodes( void )
{
  
  if( !_is_valid ) {
    cerr << "TRpcPad3DType1::make_nodes - invalid object.\n"; 
    return;
  }
    
  if( _parent ) _parent->cd();

	RpcPadType1 *pad( static_cast<RpcPadType1*>( RpcGeom::get_arm( _arm )->get_station( _station )->get_pad(_index) ) );
	RPCGEOM::Window r_window( pad->get_r_window() );
	RPCGEOM::Window phi_window( pad->get_phi_window() );
	
	double z( RpcGeom::get_arm( _arm )->get_station( _station )->get_z() );
  
	// we assume 1cm detector thickness
	double dz = 1; 
	  
  // retrieve r window
  double r_inner = r_window.first;
  double r_outer = r_window.second;  

  // octant shape
  string shape_name( "tube_"+_name );
  TTUBS *tube = new TTUBS( shape_name.c_str(), shape_name.c_str(), "void", r_inner, r_outer, dz, RPCOO::RAD_TO_DEG*phi_window.first, RPCOO::RAD_TO_DEG*phi_window.second );
  _shapes.push_back( tube );
 
  // main node
  string node_name( "node_"+_name );
  TNode* node = new TNode( node_name.c_str(), node_name.c_str(), tube, 0, 0, z );
  node->SetLineWidth( _line_width );
  node->SetLineColor( _line_color );
  node->SetVisibility( 0 );
  _nodes.push_back( node );

  if( _parent ) _parent->cd();
  
}

//_______________________________________________________
void TRpcPad3DType1::print( std::ostream& out ) const
{

	RpcPadType1 *pad( static_cast<RpcPadType1*>( RpcGeom::get_arm( _arm )->get_station( _station )->get_pad(_index) ) );
	RPCGEOM::Window r_window( pad->get_r_window() );
	RPCGEOM::Window phi_window( pad->get_phi_window() );
	
	double z( RpcGeom::get_arm( _arm )->get_station( _station )->get_z() );
	
	// some dump
	RPCOO::PRINT( cout, "TRpcPad3DType1::print" );
	cout << "location : [" << _arm << "," << _index << _station << "]" << endl;
	cout << "r=(" << r_window.first << "," << r_window.second << ")" << endl;
	cout << "phi=(" << phi_window.first << "," << phi_window.second << ")" << endl;
	cout << "z=" << z << endl;
	RPCOO::PRINT( cout, "**" );

}
