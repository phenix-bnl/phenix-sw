// $Id: TMutAnode3D.cxx,v 1.4 2009/07/21 16:13:28 hpereira Exp $

/*!
\file TMutAnode3D.cxx
\brief 3D object for mutr anode
\author Hugo Pereira
\version $Revision: 1.4 $
\date $Date: 2009/07/21 16:13:28 $
*/

#include <MutGeom.h>
#include <MutAnodeMap.h>

#include <MutAnodeMap.h>
#include <MutOctant.h>
#include <MutGap.h>
#include <MutWire.h>
#include <MUTOO.h>

#include <TBRIK.h>
#include <TTRD1.h>
#include <TRotMatrix.h>

#include <cmath>
#include <sstream>

#include "TMutAnode3D.h"

using namespace std;

//_______________________________________________________
TMutAnode3D::TMutAnode3D( const TMutIndex &index, const unsigned int& card_id, TNode* parent ):
  PHObj3D( parent ),
  _index( index ), 
  _card_id( card_id ),
  _name( "unnamed" ),
  _octant_ptr( 0 ),
  _gap_ptr( 0 ),
  _r_inner( 0 ),
  _r_outer( 0 )
{
  
  _line_color = 1;
  
  // check indexes
  if( !_index.check() ) 
  {
    cout << "TMutAnode3D::TMutAnode3D - invalid ids.\n";
    return;
  }
  
  // create name
  ostringstream what;
  what << "Anode3D_" 
    << _index.arm()  
    << _index.station() 
    << _index.octant() 
    << _index.half()
    << _index.gap()
    << _card_id;

  // also add the geometric module name (asogc)
  what << " (" << MutAnodeMap::mutr_anode2HV[_index.arm()][_index.station()][_index.octant()][_index.gap()][card_id] << ")";
  
  _name = what.str();
  
  // retrieve/initialize/check arm structure
  MutArm* arm_ptr( (_index.arm()==MUTOO::South) ? SouthArm():NorthArm() );	
  if( !arm_ptr ) {
    cout << "TMutAnode3D::TMutAnode3D - invalid arm pointer.\n";
    return;
  }
  
  // retrieve octant object
  _octant_ptr = arm_ptr->f_pMutStations[_index.station()] ->f_pMutOctants[_index.octant()];
  if( !_octant_ptr ) {
    cout << "TMutAnode3D::TMutAnode3D - invalid octant pointer.\n";
    return;
  } 
  
  // retrieve/check gap object	
  _gap_ptr = _octant_ptr->f_pMutHalfOctants[_index.half()]->f_pMutGaps[_index.gap()];
  if( !_gap_ptr ) {
    cout << "TMutAnode3D::TMutAnode3D - invalid gap pointer.\n";
    return;
  }
  
  // retrieve number of wires for this 
  MutPlane *plane_ptr = _gap_ptr->f_pMutPlanes[ MUTGEOM::Wire ];
  
  if( !plane_ptr )
  { 
    cout << "TMutAnode3D::TMutAnode3D - invalid plane" << endl;
    return;
  }
  
  int num_elements( plane_ptr->getNumElements() );
  
  // retrieve wires
  try
  {
    
    int first_wire =min( num_elements,  MutAnodeMap::get_start_wire( _index.arm(), _index.station(), _index.octant(), _card_id  ) );
    int last_wire = min( num_elements, MutAnodeMap::get_end_wire( _index.arm(), _index.station(), _index.octant(), _card_id ) );
    bool first( true );
    for( int j= first_wire; j<last_wire; j++ )
    {
      
      // retrieve wire
      MutWire* wire = plane_ptr->f_pMutWires[j];
      if( !wire ) continue;
      
      // retrieve begin and end point
      PHPoint point_begin( wire->getGlobalPositionBegin() );
      PHPoint point_end( wire->getGlobalPositionEnd() );
      
      // calculate distance to beam
      double r( fabs( 
        point_begin.getX()*(point_end.getY() - point_begin.getY()) -
        point_begin.getY()*(point_end.getX() - point_begin.getX()) )/
        sqrt( 
        MUTGEOM::SQUARE(point_end.getX() - point_begin.getX()) +
        MUTGEOM::SQUARE(point_end.getY() - point_begin.getY()) ) );
      
      if( first || r < _r_inner ) _r_inner = r;
      if( first || r > _r_outer ) _r_outer = r;
      first = false;
      
    }
    
  } catch( exception &e ) { 
    cout << e.what();
    return;
  }
  
  if( !_parent ) return;
  
  _is_valid = true;
  
}

//_______________________________________________________
void TMutAnode3D::_make_nodes( void )
{
  
  if( !_is_valid ) {
    cout << "TMutAnode3D::make_nodes - invalid object.\n"; 
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
  
  // calculate shape geometry
  double dw_small = fabs( _r_inner*sin(phi_end-phi_begin)/2 );
  double dw_big = fabs( _r_outer*sin(phi_end-phi_begin)/2 );
  double dr = fabs( _r_outer - _r_inner)/2;
  double dz = _gap_ptr->getGapThickness()/2;
  
  // calculate half octant center position
  double r_center = ( _r_inner+_r_outer )/2;
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
  double rotv[] = 
  { 
    s, -c, 0,
      0, 0, 1,
      -c, -s, 0 
    };
    
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
  void TMutAnode3D::print( std::ostream& out ) const
  {
    
    // some dump
    MUTOO::PRINT( cout, "TMutAnode3D::print" );
    cout << "location : [" << _index.arm() << "," << _index.station() << "," << _index.gap() << "," << _index.octant() << "," << _index.half() << "]" << endl;
    cout << "r=(" << _r_inner << "," << _r_outer << ")" << endl;
    cout << "theta=(" << atan2( _r_inner, fabs(_gap_ptr->getGlobalPosition().getZ()) ) << "," << atan2( _r_outer, fabs(_gap_ptr->getGlobalPosition().getZ()) ) << ")" << endl;
    cout << "phi=(" << _octant_ptr->getBeginPhi() << "," << _octant_ptr->getEndPhi() << ")" << endl;
    cout << "z=" << _gap_ptr->getGlobalPosition().getZ() << endl;
    MUTOO::PRINT( cout, "**" );
    
  }
  
