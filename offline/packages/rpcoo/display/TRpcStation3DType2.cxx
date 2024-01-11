// $Id: TRpcStation3DType2.cxx,v 1.1 2006/04/22 02:01:39 hpereira Exp $

/*!
   \file    TRpcStation3DType2.cxx
   \brief   3D object for Type2 RPC station (x,y)
   \author  H. Pereira Da Costa
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 02:01:39 $
*/

#include "TRpcStation3DType2.h"
#include "RpcGeom.h"
#include "RPCOO.h"

#include <sstream>
#include <TBRIK.h>


using namespace std;

//_______________________________________________________
TRpcStation3DType2::TRpcStation3DType2( const TRpcIndex &index, TNode* parent ):
  PHObj3D( parent ),
  _index( index ), 
  _name( "unnamed" )
{

  _line_color = 1;

  // check indexes
  if( !_index.check() ) {
    cerr << "TRpcIndex::TRpcIndex - invalid ids.\n";
    return;
  }
  
  // create name
  ostringstream what;
  what << "RpcStation_3D" 
    << _index.arm() << "_" 
    << _index.station();
  _name = what.str();
  
  if( !_parent ) return;
  
  _is_valid = true;

}

//_______________________________________________________
void TRpcStation3DType2::_make_nodes( void )
{
  
  if( !_is_valid ) {
    cerr << "TRpcStation3DType2::make_nodes - invalid object.\n"; 
    return;
  }
    
  if( _parent ) _parent->cd();

	RpcStationType2* station( static_cast<RpcStationType2*>( RpcGeom::get_arm( _index.arm() )->get_station( _index.station() ) ) );
	RPCGEOM::Window x_window( station->get_x_window() );
	RPCGEOM::Window y_window( station->get_y_window() );
	double x( 0.5*( x_window.second + x_window.first ) );
	double y( 0.5*( y_window.second + y_window.first ) );
	double z( station->get_z() );
  
	// we assume 1cm detector thickness
	double dx( fabs( x_window.second - x_window.first ) );
	double dy( fabs( y_window.second - y_window.first ) );
	double dz( 1 ); 

  // octant shape
  string shape_name( "brik_"+_name );
  TBRIK *brik = new TBRIK( shape_name.c_str(), shape_name.c_str(), "void", dx/2, dy/2, dz/2 );
  _shapes.push_back( brik );
 
  // main node
  string node_name( "node_"+_name );
  TNode* node = new TNode( node_name.c_str(), node_name.c_str(), brik, x, y, z );
  node->SetLineWidth( _line_width );
  node->SetLineColor( _line_color );
  node->SetVisibility( 0 );
  _nodes.push_back( node );

  if( _parent ) _parent->cd();
  
}

//_______________________________________________________
void TRpcStation3DType2::print( std::ostream& out ) const
{

	RpcStationType2* station( static_cast<RpcStationType2*>( RpcGeom::get_arm( _index.arm() )->get_station( _index.station() ) ) );
	RPCGEOM::Window x_window( station->get_x_window() );
	RPCGEOM::Window y_window( station->get_y_window() );
	double z( station->get_z() );
	
	// some dump
	RPCOO::PRINT( cout, "TRpcStation3DType2::print" );
	cout << "location : [" << _index.arm() << "," << _index.station() << "]" << endl;
	cout << "x=(" << x_window.first << "," << x_window.second << ")" << endl;
	cout << "y=(" << y_window.first << "," << y_window.second << ")" << endl;
	cout << "z=" << z << endl;
	RPCOO::PRINT( cout, "**" );

}
