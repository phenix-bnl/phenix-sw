// $Id: TRpcPad3DType2.cxx,v 1.1 2006/04/22 02:01:39 hpereira Exp $

/*!
   \file    TRpcPad3DType2.cxx
   \brief   3D object for type2 RPC pad (x,y)
   \author  H. Pereira Da Costa
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 02:01:39 $
*/

#include "TRpcPad3DType2.h"
#include "RpcGeom.h"
//INCLUDECHECKER: Removed this line: #include "RpcPadType2.h"
#include "RPCOO.h"

//INCLUDECHECKER: Removed this line: #include <sstream>
#include <TBRIK.h>


using namespace std;

//_______________________________________________________
void TRpcPad3DType2::_make_nodes( void )
{
  
  if( !_is_valid ) {
    cerr << "TRpcPad3DType2::make_nodes - invalid object.\n"; 
    return;
  }
    
  if( _parent ) _parent->cd();

	RpcPadType2 *pad( static_cast<RpcPadType2*>( RpcGeom::get_arm( _arm )->get_station( _station )->get_pad(_index) ) );
	PHPoint center( pad->get_center() );
	double dx( pad->get_x_size() );
	double dy( pad->get_y_size() );
  
	// we assume 1cm detector thickness
	double dz = 1; 

  // station shape
  string shape_name( "brik_"+_name );
  TBRIK *brik = new TBRIK( shape_name.c_str(), shape_name.c_str(), "void", dx/2, dy/2, dz/2 );
  _shapes.push_back( brik );
 
  // main node
  string node_name( "node_"+_name );
  TNode* node = new TNode( node_name.c_str(), node_name.c_str(), brik, center.getX(), center.getY(), center.getZ() );
  node->SetLineWidth( _line_width );
  node->SetLineColor( _line_color );
  node->SetVisibility( 0 );
  _nodes.push_back( node );

  if( _parent ) _parent->cd();
  
}

//_______________________________________________________
void TRpcPad3DType2::print( std::ostream& out ) const
{

	RpcPadType2 *pad( static_cast<RpcPadType2*>( RpcGeom::get_arm( _arm )->get_station( _station )->get_pad(_index) ) );
	RPCGEOM::Window x_window( pad->get_x_window() );
	RPCGEOM::Window y_window( pad->get_y_window() );
	
	double z( RpcGeom::get_arm( _arm )->get_station( _station )->get_z() );
	
	// some dump
	RPCOO::PRINT( cout, "TRpcPad3DType2::print" );
	cout << "location : [" << _arm << "," << _index << _station << "]" << endl;
	cout << "x=(" << x_window.first << "," << x_window.second << ")" << endl;
	cout << "y=(" << y_window.first << "," << y_window.second << ")" << endl;
	cout << "z=" << z << endl;
	RPCOO::PRINT( cout, "**" );

}
