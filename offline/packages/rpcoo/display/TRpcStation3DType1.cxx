// $Id: TRpcStation3DType1.cxx,v 1.1 2006/04/22 02:01:39 hpereira Exp $

/*!
   \file    TRpcStation3DType1.cxx
   \brief   3D object for type1 RPC station (r,phi)
   \author  H. Pereira Da Costa
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 02:01:39 $
*/

#include "TRpcStation3DType1.h"
#include "RpcGeom.h"
#include "RPCOO.h"

#include <sstream>
#include <TTUBE.h>


using namespace std;

//_______________________________________________________
TRpcStation3DType1::TRpcStation3DType1( const TRpcIndex &index, TNode* parent ):
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
void TRpcStation3DType1::_make_nodes( void )
{
  
  if( !_is_valid ) {
    cerr << "TRpcStation3DType1::make_nodes - invalid object.\n"; 
    return;
  }
    
  if( _parent ) _parent->cd();

	RpcStationType1* station( static_cast<RpcStationType1*>( RpcGeom::get_arm( _index.arm() )->get_station( _index.station() ) ) );
	RPCGEOM::Window window( station->get_r_window() );
	double z( station->get_z() );
  
	// we assume 1cm detector thickness
	double dz = 1; 
	  
  // retrieve r window
  double r_inner = window.first;
  double r_outer = window.second;  

  // station shape
  string shape_name( "tube_"+_name );
  TTUBE *tube = new TTUBE( shape_name.c_str(), shape_name.c_str(), "void", r_inner, r_outer, dz );
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
void TRpcStation3DType1::print( std::ostream& out ) const
{

	RpcStationType1* station( static_cast<RpcStationType1*>( RpcGeom::get_arm( _index.arm() )->get_station( _index.station() ) ) );
	RPCGEOM::Window window( station->get_r_window() );
	double z( station->get_z() );
	
	// some dump
	RPCOO::PRINT( cout, "TRpcStation3DType1::print" );
	cout << "location : [" << _index.arm() << "," << _index.station() << "]" << endl;
	cout << "r=(" << window.first << "," << window.second << ")" << endl;
	cout << "z=" << z << endl;
	RPCOO::PRINT( cout, "**" );

}
