// $Id: PHObj3D.cxx,v 1.2 2004/11/29 22:08:26 hpereira Exp $

/*!
   \file    PHObj3D.cxx
   \brief   generic 3D object
   \author  Hugo Pereira
   \version $Revision: 1.2 $
   \date    $Date: 2004/11/29 22:08:26 $
*/

#include "PHObj3D.h"
#include <MUTOO.h>
#include <PHVector.h>
#include <TTUBE.h>

using namespace std;

//_______________________________________________________
void PHObj3D::draw( void )
{
  if( _drawn ) return;
  if( _nodes.empty() ) _make_nodes();
  if( _nodes.empty() ) return;
  for( list< TNode* >::iterator It = _nodes.begin(); It!= _nodes.end(); It++ )
  if( *It ) (*It)->SetVisibility( 1 );

  _drawn = true;
}

//_______________________________________________________
void PHObj3D::hide( void )
{
  if( !_drawn ) return;
  for( list< TNode* >::iterator It = _nodes.begin(); It!= _nodes.end(); It++ )
  if( *It ) (*It)->SetVisibility( 0 );
  _drawn = false;
}


//_______________________________________________________
void PHObj3D::_clear_nodes( void )
{
  
  // delete all nodes
  for( list< TNode* >::iterator It = _nodes.begin(); It != _nodes.end(); It++ )
  if( *It ) SafeDelete( *It );
  _nodes.clear();
  
  // delete all shapes
  for( list< TShape* >::iterator It = _shapes.begin(); It != _shapes.end(); It++ )
  if( *It ) SafeDelete( *It );
  _shapes.clear();
  
  // delete all rotation matrices
  for( list< TRotMatrix* >::iterator It = _matrices.begin(); It != _matrices.end(); It++ )
  if( *It ) SafeDelete( *It );
  _matrices.clear();
  
}


//_________________________________________________
void PHObj3D::_make_segment( const PHPoint& first_point, const PHPoint& second_point, const double& radius )
{
    
  //! calculate center point
  PHPoint center_point( 
    ( first_point.getX() + second_point.getX() )/2,
    ( first_point.getY() + second_point.getY() )/2,
    ( first_point.getZ() + second_point.getZ() )/2 );
  
  //! calculate tube length
  double length = sqrt( 
    MUTOO::SQUARE( first_point.getX() - second_point.getX() ) + 
    MUTOO::SQUARE( first_point.getY() - second_point.getY() ) + 
    MUTOO::SQUARE( first_point.getZ() - second_point.getZ() ) );
  
  //! get direction
  PHVector direction( 
    ( first_point.getX() - second_point.getX() )/length,
    ( first_point.getY() - second_point.getY() )/length,
    ( first_point.getZ() - second_point.getZ() )/length );
    
  // build direct rotation matrix to change [0,0,1] into direction
  double x( direction.getX() ),   x2( MUTOO::SQUARE( direction.getX() ) );  
  double y( direction.getY() ),   y2( MUTOO::SQUARE( direction.getY() ) );  
  double z( direction.getZ()+1 ), z2( MUTOO::SQUARE( direction.getZ()+1 ) );
  double n2( x2 + y2+ z2 );
    
  double rotv[] = {
    1-2*x2/n2, -2*x*y/n2,  2*x*z/n2, 
    -2*x*y/n2,  1-2*y2/n2, 2*y*z/n2, 
    -2*x*z/n2, -2*y*z/n2,  1-2*z2/n2 };
  
  TRotMatrix *rot = new TRotMatrix( "rot", "rot", rotv );
  _matrices.push_back( rot );
  
  TTUBE *tube = new TTUBE( "tube", "tube" , "void", radius,  0.5*length );
  _shapes.push_back( tube );
    
  TNode *node = new TNode( "segment_node", "segment_node" ,tube, 
    center_point.getX(), 
    center_point.getY(), 
    center_point.getZ() );
  node->SetMatrix( rot );
  node->SetLineWidth( _line_width );
  node->SetLineColor( _line_color );
  node->SetVisibility( 0 );
    
  _nodes.push_back( node );  
  if( _parent ) _parent->cd();
  return;
}  
  
