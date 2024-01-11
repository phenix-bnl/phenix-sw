// $Id: TMuiPanel3D.cxx,v 1.2 2007/01/07 20:21:05 hpereira Exp $

/*!
   \file    TMuiPanel3D.cxx
   \brief   3D object for muid panel
   \author  Hugo Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2007/01/07 20:21:05 $
*/

#include "TMuiPanel3D.h"

#include <MUIOO.h>
#include <sstream>
#include <TBRIK.h>

using namespace std;

//_______________________________________________________
TMuiPanel3D::TMuiPanel3D( const TMuiIndex &index, TNode* parent ):
  PHObj3D( parent ),
  _panel_ptr( 0 ),
  _index( index ), 
  _name( "unnamed" )
{

  _line_color = 1;

  // check indexes
  if( !_index.check() ) {
    cerr << "TMuiPanel3D::TMuiPanel3D - invalid ids.\n";
    return;
  }
  
  // create name
  ostringstream what;
  what << "Panel3D_" 
    << _index.arm() << "_" 
    << _index.plane() << "_" 
    << _index.panel(); 
  _name = what.str();

  // retrieve mui geometry
  TMuiGeometry* mui_geom = TMuiGeometry::Geom();
  
  // retrieve panel
  _panel_ptr = mui_geom->getPanel( _index.arm(), _index.plane(), _index.panel() );
  if( !_panel_ptr ) {
    cerr << "TMuiPanel3D::TMuiPanel3D - invalid panel id.\n";
    return;
  }
  
  _is_valid = true;
  return;
}
    
//_______________________________________________________
void TMuiPanel3D::_make_nodes( void )
{
  
  if( !( _is_valid && _panel_ptr ) ) 
  {
    cerr << "TMutCathode3D::make_nodes - invalid object.\n"; 
    return;
  }
    
  // retrieve pannel center position
  float x_center, y_center, z_center;
  _panel_ptr->CenterPos( x_center, y_center, z_center );
  
  // retrieve pannel sizes
  float x_size, y_size, z_size;
  _panel_ptr->Size( x_size, y_size, z_size );
 
  // create main shape
  string shape_name( "brik_"+_name );
  TBRIK *brik = new TBRIK( shape_name.c_str(), shape_name.c_str(), "void", x_size/2, y_size/2, z_size/2 );
  _shapes.push_back( brik );
  
  // create main node
  string node_name( "node_"+_name );
  TNode* node = new TNode( node_name.c_str(), node_name.c_str(), brik, x_center, y_center, z_center );
  node->SetLineWidth( _line_width );
  node->SetLineColor( _line_color );
  node->SetVisibility( 0 );
  _nodes.push_back( node );

  if( _parent ) _parent->cd();
  
}

//_______________________________________________________
void TMuiPanel3D::print( std::ostream& out ) const
{

  // retrieve pannel center position
  float x_center, y_center, z_center;
  _panel_ptr->CenterPos( x_center, y_center, z_center );
  
  // retrieve pannel sizes
  float x_size, y_size, z_size;
  _panel_ptr->Size( x_size, y_size, z_size );
  
  // some dump
  MUIOO::PRINT( cout, "TMuiPanel3D::print" );
  cout << "location : [" << _index.arm() << "," << _index.plane() << "," << _index.panel() << "]" << endl;
  cout << "center position: (" << x_center << "," << y_center << "," << z_center << ")" << endl;
  cout << "size: (" << x_size << "," << y_size << "," << z_size << ")" << endl;
  MUIOO::PRINT( cout, "**" );

}
